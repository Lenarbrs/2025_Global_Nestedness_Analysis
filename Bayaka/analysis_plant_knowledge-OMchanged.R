setwd("/Users/edgardubourg/Documents/Recherche/Travaux/Articles/Expertise Evaluation/Soumission/Psychological Science/REVISION/REVISION 2/Study Plant Knowledge")

#0. Loading packages ####

library(readr) #to open files
library(tidyverse) #for data wrangling
library(lmerTest) #for regressions
library(performance) #for model comparison
library(ggplot2) #data viz
library(vegan) #nestedness analysis & matrix permutations

rawdata <- read_csv("plant_knowledge.csv")
d <- rawdata %>% select(-ID)
d <- d %>% drop_na()

#1. Testing H1 ##########

#1.1. Computing 2 measures of nestedness ######

#The two measures are nodf and temp
#High NODF = High nestedness
#High temp = LOW nestedness.

nestednodf(d)
nestedtemp(d)

#1.2. Bootstrapping analysis #####

#Stored parameter: 
#the number of simulated matrices
nsimul = 200

#setting the seed to reproduce the random bootstraps
set.seed(123)

# Creating a folder to store figures
dir.create(file.path("figures"), showWarnings = FALSE)

# Nestedness measures for the real dataset
real.nodf <- round(nestednodf(d)$statistic['NODF'])
print(paste("Plant dataset's nestedness (NODF):", real.nodf))

real.temp <- nestedtemp(d)$statistic
print(paste("Plant dataset's nestedness (temperature):", real.temp))

# Simulation function
null.r1 <- nullmodel(d, 'r1')
r1.simulations <- simulate(null.r1, nsim = nsimul)

# Creating empty vectors to store simulation results
r1.simulated.temperatures <- c()
r1.simulated.nodfs <- c()

# Running simulations
for (j in 1:nsimul) {
  R1Matrix <- r1.simulations[, , j]
  r1.simulated.temperatures[j] <- nestedtemp(R1Matrix)$statistic
  r1.simulated.nodfs[j] <- nestednodf(R1Matrix)$statistic['NODF']
}

# Bootstrapped significance tests
nodf.r1.p.value <- sum(r1.simulated.nodfs >= real.nodf) / length(r1.simulated.nodfs)
nodf.r1.z.value <- (real.nodf - mean(r1.simulated.nodfs)) / sd(r1.simulated.nodfs)
print(paste("Bootstrapped p-value for NODF:", nodf.r1.p.value))
print(paste("Test z-value for NODF:", nodf.r1.z.value))

temperature.r1.p.value <- sum(r1.simulated.temperatures <= real.temp) / length(r1.simulated.temperatures)
temperature.r1.z.value <- (real.temp - mean(r1.simulated.temperatures)) / sd(r1.simulated.temperatures)
print(paste("Bootstrapped p-value for temperature:", temperature.r1.p.value))
print(paste("Test z-value for temperature:", temperature.r1.z.value))

# Plotting results
plot.title <- "figures/plant_bootstrapping.png"
png(file = plot.title)
par(mfrow = c(1, 2))

# NODF plot
plot(density(r1.simulated.nodfs), 
     xlim = c(min(r1.simulated.nodfs), as.numeric(real.nodf + 1)), 
     main = "NODF - R1", xlab = "Simulated NODF values")
abline(v = real.nodf, lwd = 3, col = 'gold')

# Temperature plot
plot(density(r1.simulated.temperatures), 
     xlim = c(as.numeric(real.temp - 1), max(r1.simulated.temperatures)), 
     main = "Temperature - R1", xlab = "Simulated Temperature values")
abline(v = real.temp, lwd = 3, col = 'gold')

dev.off()

#2. Testing H1' ####

d <- rawdata
d <- d %>% drop_na()

# Rename columns for ease of processing
colnames(d) <- gsub(" ", "_", colnames(d))  # Replace spaces with underscores

# Compute participant objective performance
participant_objective_performance <- d %>%
  mutate(Actual_Knowledge = rowSums(select(., starts_with("Plant_"))) / 33) %>%
  select(ID, Actual_Knowledge)

# Preview the performance table
head(participant_objective_performance)

# Compute rarity for each plant
plant_rarity <- d %>%
  select(starts_with("Plant_")) %>%
  summarise(across(everything(), ~ sum(. == 0))) %>%
  pivot_longer(cols = everything(), names_to = "Plant", values_to = "Rarity")

# Turn into percentage
plant_rarity$Rarity <- (plant_rarity$Rarity/216)

# Preview the rarity table
head(plant_rarity)

# Convert dataset to long format for processing
participant_knowledge_long <- d %>%
  pivot_longer(cols = starts_with("Plant_"), names_to = "Plant", values_to = "Knowledge") %>%
  left_join(participant_objective_performance, by = c("ID" = "ID"))

# Filter rows for participants who knew the plant (Knowledge == 1)
known_plants_with_performance <- participant_knowledge_long %>%
  filter(Knowledge == 1)

# Compute average objective performance score for each plant
plant_average_objective_performance <- known_plants_with_performance %>%
  group_by(Plant) %>%
  summarise(Average_Actual_Knowledge = mean(Actual_Knowledge))

# Preview the final table
head(plant_average_objective_performance)

# Join
dataset_for_h1bis <- participant_knowledge_long %>%
  left_join(plant_average_objective_performance, by = "Plant") %>%
  left_join(plant_rarity, by="Plant")

# Null model
null_model_h1bis <- lmer(Average_Actual_Knowledge ~ (1|ID) , data = dataset_for_h1bis)
summary(null_model_h1bis)
# Create the model
model_h1bis <- lmer(Average_Actual_Knowledge ~ Rarity + (1|ID) , data = dataset_for_h1bis)
summary(model_h1bis)
#without random intercept for participant, as preregistered (but it's the incorrect way)
model_h1ter <- lm(Average_Actual_Knowledge ~ Rarity, data = dataset_for_h1bis)
summary(model_h1ter)
#with a random slope for the effect of Rarity as it varies by Theme:
model_h1_alt <- lmer(Average_Actual_Knowledge ~ Rarity + (1 + Rarity | ID) , data = dataset_for_h1bis)
summary(model_h1_alt)

AIC(null_model_h1bis)
AIC(model_h1bis)
AIC(model_h1ter)
AIC(model_h1_alt)

ggplot(dataset_for_h1bis, aes(x = Rarity, y = Average_Actual_Knowledge)) +
  geom_point(size=4) + # Plot points with increased size
  geom_smooth(method = "lm", se = FALSE, size=2) + # Add linear model line for each theme
  labs(x = "Actual Rarity", 
       y = "Average Actual Knowledge", # Move subtitle to y-axis label
       title = "H1'") +
  theme_minimal() + # Use a minimal theme
  theme(
        plot.title = element_text(size = 18), # Increase title size
        plot.subtitle = element_text(size = 18), # Adjust subtitle size
        axis.title = element_text(size = 18), # Increase axis title size
        axis.text = element_text(size = 18), # Increase axis text size
        legend.title = element_blank(), # Remove legend title
        legend.text = element_text(size = 18)) # Increase legend text size



###OM addition : Using oecosimu #####

help(oecosimu)

#Temperature 

print(d[,-1])

temperature.r1 <- oecosimu(d[,-1], nestedtemp, "r1", nsimul = 99, alternative = "less")
temperature.r1

temperature.c0 <- oecosimu(d[,-1], nestedtemp, "c0", nsimul = 99, alternative = "less")
temperature.c0

temperature.curveball <- oecosimu(d[,-1], nestedtemp, "curveball", nsimul = 99, alternative = "less")
temperature.curveball

#NODF
nodf.r1 <- oecosimu(d[,-1], nestednodf, "r1", nsimul = 99, alternative = "greater")
nodf.r1

nodf.c0 <- oecosimu(d[,-1], nestednodf, "c0", nsimul = 99, alternative = "greater")
nodf.c0

nodf.curveball <- oecosimu(d[,-1], nestednodf, "curveball", nsimul = 99, alternative = "greater")
nodf.curveball

#Visualising the matrix

out <- nestedtemp(d[,-1])
out
plot(out, kind="incid", main = "Incidence Plot for Binary Matrix")
df.temp <- as.data.frame(out$u)
