setwd("/Users/edgardubourg/Documents/Recherche/Travaux/Articles/Epistemic curiosity/Trivia Knowledge and Nestedness/V3")

#### 1. STUDY 1 ####
#### 1.1. DATA CLEANING AND KAPPA ANALYSIS ####
# Data cleaning for the project "Nestdness & trivia knowledge"
# 1.1.0. and 1.1.1. DON'T NEED TO BE RUN: preparation of data for manual coding ####
# 1.1.0. Preparation for coding ####

# Setting directory and opening dataset (cvs from Qualtrics)

library(readr)
rawdata_study1 <- read_csv("raw_study1.csv")
library(tidyverse)
library(dplyr)


# Cleaning the raw data : 
# Removing irrelevant rows and renaming wrongly named columns, adding "id" column to anonymize results

names(rawdata_study1)[names(rawdata_study1)=="Q52"] <- "free_consent"
names(rawdata_study1)[names(rawdata_study1)=="Q39'"] <- "Q39"
names(rawdata_study1)[names(rawdata_study1)=="Q53"] <- "attention_check"
data_study1 <- rawdata_study1 %>%
  slice(3:nrow(rawdata_study1))
data_study1$id=1:nrow(data_study1)

# Keeping only relevant data for analysis : 
# Removing participants that did not pass the attention check 
#selecting questions and id 
study1_relevant_data <- data_study1 %>%  drop_na(attention_check) %>%
  filter(attention_check == "I pay attention"| attention_check == "I PAY ATTENTION" | attention_check == "i pay attention") %>%
  select(id, Q1, Q2, Q3, Q4, Q5, Q6, Q7, Q8, Q9, Q10, Q11, Q12, Q13, Q14, Q15, Q16, Q17, Q18, Q19, Q20, Q21, Q22, Q23, Q24, Q25, Q26, Q27, Q28, Q29, Q30, Q31, Q32, Q33, Q34, Q35, Q36, Q37, Q38, Q39, Q40, Q41, Q42, Q43, Q44, Q45)



# Isolating each question from Q1 to Q45 and the responses of all participants to this question and then collapsing all the tables together

db1 <- study1_relevant_data %>% select(id, Q1) %>% pivot_longer(cols=Q1, names_repair = "unique")
db2 <- study1_relevant_data %>% select(id, Q2) %>% pivot_longer(cols=Q2, names_repair = "unique")
db3 <- study1_relevant_data %>% select(id,  Q3) %>% pivot_longer(cols=Q3, names_repair = "unique")
db4 <- study1_relevant_data %>% select(id, Q4) %>% pivot_longer(cols=Q4, names_repair = "unique") 
db5 <- study1_relevant_data %>% select(id, Q5) %>% pivot_longer(cols=Q5, names_repair = "unique")
db6 <- study1_relevant_data %>% select(id, Q6) %>% pivot_longer(cols=Q6, names_repair = "unique")
db7 <- study1_relevant_data %>% select(id, Q7) %>% pivot_longer(cols=Q7, names_repair = "unique")
db8 <- study1_relevant_data %>% select(id, Q8) %>% pivot_longer(cols=Q8, names_repair = "unique")
db9 <- study1_relevant_data %>% select(id, Q9) %>% pivot_longer(cols=Q9, names_repair = "unique")
db10 <- study1_relevant_data %>% select(id, Q10) %>% pivot_longer(cols=Q10, names_repair = "unique")
db11 <- study1_relevant_data %>% select(id, Q11) %>% pivot_longer(cols=Q11, names_repair = "unique")
db12 <- study1_relevant_data %>% select(id, Q12) %>% pivot_longer(cols=Q12, names_repair = "unique")
db13 <- study1_relevant_data %>% select(id, Q13) %>% pivot_longer(cols=Q13, names_repair = "unique")
db14 <- study1_relevant_data %>% select(id, Q14) %>% pivot_longer(cols=Q14, names_repair = "unique")
db15 <- study1_relevant_data %>% select(id, Q15) %>% pivot_longer(cols=Q15, names_repair = "unique")
db16 <- study1_relevant_data %>% select(id, Q16) %>% pivot_longer(cols=Q16, names_repair = "unique")
db17 <- study1_relevant_data %>% select(id, Q17) %>% pivot_longer(cols=Q17, names_repair = "unique")
db18 <- study1_relevant_data %>% select(id, Q18) %>% pivot_longer(cols=Q18, names_repair = "unique")
db19 <- study1_relevant_data %>% select(id, Q19) %>% pivot_longer(cols=Q19, names_repair = "unique") 
db20 <- study1_relevant_data %>% select(id, Q20) %>% pivot_longer(cols=Q20, names_repair = "unique")
db21 <- study1_relevant_data %>% select(id, Q21) %>% pivot_longer(cols=Q21, names_repair = "unique")
db22 <- study1_relevant_data %>% select(id, Q22) %>% pivot_longer(cols=Q22, names_repair = "unique")
db23 <- study1_relevant_data %>% select(id, Q23) %>% pivot_longer(cols=Q23, names_repair = "unique")
db24 <- study1_relevant_data %>% select(id, Q24) %>% pivot_longer(cols=Q24, names_repair = "unique")
db25 <- study1_relevant_data %>% select(id, Q25) %>% pivot_longer(cols=Q25, names_repair = "unique")
db26 <- study1_relevant_data %>% select(id, Q26) %>% pivot_longer(cols=Q26, names_repair = "unique")
db27 <- study1_relevant_data %>% select(id, Q27) %>% pivot_longer(cols=Q27, names_repair = "unique")
db28 <- study1_relevant_data %>% select(id, Q28) %>% pivot_longer(cols=Q28, names_repair = "unique")
db29 <- study1_relevant_data %>% select(id, Q29) %>% pivot_longer(cols=Q29, names_repair = "unique")
db30 <- study1_relevant_data %>% select(id, Q30) %>% pivot_longer(cols=Q30, names_repair = "unique")
db31 <- study1_relevant_data %>% select(id, Q31) %>% pivot_longer(cols=Q31, names_repair = "unique")
db32 <- study1_relevant_data %>% select(id, Q32) %>% pivot_longer(cols=Q32, names_repair = "unique")
db33 <- study1_relevant_data %>% select(id, Q33) %>% pivot_longer(cols=Q33, names_repair = "unique")
db34 <- study1_relevant_data %>% select(id, Q34) %>% pivot_longer(cols=Q34, names_repair = "unique")
db35 <- study1_relevant_data %>% select(id, Q35) %>% pivot_longer(cols=Q35, names_repair = "unique")
db36 <- study1_relevant_data %>% select(id, Q36) %>% pivot_longer(cols=Q36, names_repair = "unique")
db37 <- study1_relevant_data %>% select(id, Q37) %>% pivot_longer(cols=Q37, names_repair = "unique")
db38 <- study1_relevant_data %>% select(id, Q38) %>% pivot_longer(cols=Q38, names_repair = "unique")
db39 <- study1_relevant_data %>% select(id, Q39) %>% pivot_longer(cols=Q39, names_repair = "unique")
db40 <- study1_relevant_data %>% select(id, Q40) %>% pivot_longer(cols=Q40, names_repair = "unique")
db41 <- study1_relevant_data %>% select(id, Q41) %>% pivot_longer(cols=Q41, names_repair = "unique")
db42 <- study1_relevant_data %>% select(id, Q42) %>% pivot_longer(cols=Q42, names_repair = "unique")
db43 <- study1_relevant_data %>% select(id, Q43) %>% pivot_longer(cols=Q43, names_repair = "unique")
db44 <- study1_relevant_data %>% select(id, Q44) %>% pivot_longer(cols=Q44, names_repair = "unique")
db45 <- study1_relevant_data %>% select(id, Q45) %>% pivot_longer(cols=Q45, names_repair = "unique")

study1_to_be_rated_data <- bind_rows(db1, db2, db3, db4, db5, db6, db7, db8, db9, db10, db11, db12, db13, db14, db15, db16, db17, db18, db19, db20, db21, db22, db23, db24, db25, db26, db27, db28, db29, db30, db31, db32, db33, db34, db35, db36, db37, db38, db39, db40, db41, db42, db43, db44, db45)
names(study1_to_be_rated_data)[2] = "question"
names(study1_to_be_rated_data)[3] = "answer"

#removing irrelevant data-frames

rm(rawdata_study1, data_study1)
rm(db1, db2, db3, db4, db5, db6, db7, db8, db9, db10, db11, db12, db13, db14, db15, db16, db17, db18, db19, db20, db21, db22, db23, db24, db25, db26, db27, db28, db29, db30, db31, db32, db33, db34, db35, db36, db37, db38, db39, db40, db41, db42, db43, db44, db45)

# 1.1.1..Separating dataset in 2 for 2 coders (with 15% of overlap) ####

# I sample 15% of the dataset and add a column 'inter' with 1 (and 0 for the remaining 85%)
study1_sample1 <- sample_n(study1_to_be_rated_data, 669) 
study1_sample1$inter = 1
study1_sample1 <- full_join(study1_to_be_rated_data, study1_sample1)
study1_sample1[is.na(study1_sample1)] <- 0

# I isolate the inter=0 and sample half of this subset to attribute to one coder
study1_sample2 <- study1_sample1 %>% filter(inter==0)  
study1_sample2_ed <- study1_sample2 %>% sample_n(1893)
study1_sample2_ed$rater="ed"
study1_sample3 <- full_join(study1_sample2, study1_sample2_ed)

# I attribute the other half to the other coder
study1_sample3[is.na(study1_sample3)] <- "td"

# I isolate the inter=1 and attribute them to rater=both
study1_sample4 <- study1_sample1 %>% filter(inter==1) 
study1_sample4$rater="both"

# I collapse all and extract in Excel for coding
study1_sample_full <- bind_rows(study1_sample3, study1_sample4)
#write_xlsx(db_master,"nestedness_coding")

#removing irrelevant data-frames

rm(study1_sample1, study1_sample2, study1_sample2_ed, study1_sample3, study1_sample4)
rm(study1_to_be_rated_data, study1_sample_full)
rm(study1_relevant_data)

# 1.1.2. Aggregating databases after coding ####
# I open the previously extracted dataset (to get back the id of the participants) and the two datasets of the coders for their rating of the answers and join them together
# I have to use this dataset because each run of the code sample randomly the questions attributed to each coder in a different manner
# WARNING # never run again the 'Preparation for coding part'

library(readr)
library(tidyverse)
library(dplyr)
library("readxl")

study1_coding_ed <- read_xlsx("study1_coding_ed.xlsx") 
study1_coding_td <- read_xlsx("study1_coding_td.xlsx")


study1_full_coded_dataset <- full_join(study1_coding_ed, study1_coding_td)


#removing irrelevant data-frames

rm(study1_coding_ed, study1_coding_td)



# 1.1.3. Testing inter-rater agreement with Cohen's K ####
# I isolate the subset of coders' overlap for testing the inter-rater agreement
study1_overlap_coded_dataset <- study1_full_coded_dataset %>% drop_na(corr_ed, corr_td)
study1_overlap_coded_dataset$rater="both"

study1_kappa <- study1_overlap_coded_dataset %>% dplyr::select(corr_ed, corr_td)
library("irr")
kappa2(study1_kappa, "unweighted")

#removing irrelevant data-frames

rm(study1_kappa)



# 1.1.4. Final data cleaning ####
# I add a 'rater' column to identify overlap in the full dataset
study1_full_coded_with_rater_column <- study1_full_coded_dataset %>% full_join(dplyr::select(study1_overlap_coded_dataset, c(id, question, time, answer, rater))) 
# here, I delete the questions coded by both coders from one of the coder dataset (randomly chosen)
study1_full_coded_with_rater_column$rater[is.na(study1_full_coded_with_rater_column$rater)] <- 0
study1_coded_eg_only <- study1_full_coded_with_rater_column %>% filter(rater==0) %>% drop_na(corr_ed) %>% dplyr::select(id, question, answer, time, corr_ed)
names(study1_coded_eg_only)[5]="score"
study1_coded_td_and_both <- study1_full_coded_with_rater_column %>% drop_na(corr_td) %>% dplyr::select(!corr_ed)
names(study1_coded_td_and_both)[5]="score"
study1_coded_cleaned <- bind_rows(study1_coded_td_and_both, study1_coded_eg_only) %>% unique() %>% dplyr::select(-rater)


# I replace the coding 2 (that we used when participants responsded 'I don't know') by 0 ('I don't knwow' treated as a 'bad response'
study1_coded_cleaned$score[study1_coded_cleaned$score == 2] <- 0

# I select the relevant columns
# study1_final is the dataset used in future analyses
study1_final <- study1_coded_cleaned %>%
  dplyr::select(id, question, score)

study1_final$question <- gsub("[^0-9.]", "",  study1_final$question)
study1_final$question <- as.numeric(study1_final$question)


# I pivot the table wider to have the matrice for future analysis
# d.trivia is the dataset used in the NESTEDNESS ANALYSIS
d.trivia <- study1_final %>%
  pivot_wider(names_from=question, values_from = score)

#removing irrelevant data-frames 

rm(study1_overlap_coded_dataset, study1_full_coded_dataset)
rm(study1_full_coded_with_rater_column, study1_coded_eg_only, study1_coded_td_and_both)
rm(study1_coded_cleaned)


#### 1.2. NESTEDNESS ANALYSIS ####

# Analyses for the project "Nestedness & trivia knowledge"

#Olivier Morin

# 1.2.0. Loading the data & packages #####

#Packages:
library(tidyverse)
library(vegan) #for our two nestedness meaasures

#We shall also be using the biparte package later, to make figures; but it is 
#*very important* not to have it loaded right now.
#Some of bipartite's functions interfere with vegan's and cause vegan to bug.

# 1.2.1. Constituting the datasets  #######

#I remove the first column that assigns an id to each row,
d.trivia <- d.trivia[,-1]

#I need to make sure that my data is a numeric matrix - this will be crucial later to build the simulated matrices
d.trivia <- as.matrix(d.trivia)

#Now I build the 3 sets following the info given in the prereg & confirmed by Edgar
#Condition 1: Planet: Q1 : Q15  (included)
#Condition 2: History: Q16 : 30 (included)
#Condition 3: Super hero: Q31 : Q45 (included)

planetQs <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15")
historyQs <- c("16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30")
marveldcQs <- c("31", "32", "33", "34", "35", "36", "37", "38", "39", "40", "41", "42", "43", "44", "45")

d.planets <- d.trivia[,planetQs]
d.history <- d.trivia[,historyQs]
d.marveldc <- d.trivia[,marveldcQs]

# 1.2.2. Measuring nestedness ####

help(nestednodf)
help(nestedtemp)

#full set
nestednodf(d.trivia)
nestedtemp(d.trivia)
full.nodf <- nestednodf(d.trivia)$statistic['NODF']
full.temp <- nestedtemp(d.trivia)$statistic

#history
nestednodf(d.history)
nestedtemp(d.history)
history.nodf <- as.numeric(round(nestednodf(d.history)$statistic['NODF']))
history.temp <- nestedtemp(d.history)$statistic

#marveldc
nestednodf(d.marveldc)
nestedtemp(d.marveldc)
marveldc.nodf <- round(nestednodf(d.marveldc)$statistic['NODF'])
marveldc.temp <- nestedtemp(d.marveldc)$statistic

#planets
nestednodf(d.planets)
nestedtemp(d.planets)
planets.nodf <- round(nestednodf(d.planets)$statistic['NODF'])
planets.temp <- nestedtemp(d.planets)$statistic


# 1.2.3. Building  simulated matrices: example #####

help(nullmodel)

#Stored parameter: 
#the number of simulated matrices
nsimul = 500

#setting the seed to reproduce the random bootstraps
set.seed(123)

#An example: generating a permutated history matrix using the r1 method:
null.history.r1 <- nullmodel(d.history, 'r1')
history.r1.simulations <- simulate(null.history.r1, nsim=nsimul)
#printing a sample matrix, n°4:
matrix4 <- history.r1.simulations[,,4]
matrix4


# 1.2.4. Bootstrapping analysis ####

#Are nestedness measures higher / lower than the simulated baselines?
#Some reminders: 
# *Low* temperature indicates *high* nestedness, whereas high nodf => high nestedness
# We preregistered an alpha of 0.005
# It's not abnormal for a bootstrapped p value to reach exactly 0.


# 1.2.5. history #####

null.history.r1 <- nullmodel(d.history, 'r1')
history.r1.simulations <- simulate(null.history.r1, nsim=nsimul)
print(history.r1.simulations[,,1])

null.history.curveball <- nullmodel(d.history, 'curveball')
history.curveball.simulations <- simulate(null.history.curveball, nsim=nsimul)
print(history.curveball.simulations[,,1])

history.r1.simulated.temperatures <- c()
history.r1.simulated.nodfs <- c()
history.curveball.simulated.temperatures <- c()
history.curveball.simulated.nodfs <- c()

for (i in 1:nsimul){
  #print(i)
  R1Matrix <-   history.r1.simulations[,,i]
  CurveballMatrix <- history.curveball.simulations[,,i]
  history.r1.simulated.temperatures[i] <- nestedtemp(R1Matrix)$statistic
  history.r1.simulated.nodfs[i] <- nestednodf(R1Matrix)$statistic['NODF']
  history.curveball.simulated.temperatures[i] <- nestedtemp(CurveballMatrix)$statistic
  history.curveball.simulated.nodfs[i] <- nestednodf(CurveballMatrix)$statistic['NODF']
}

#Bootstrapped significance tests

history.nodf.r1.p.value = sum(history.r1.simulated.nodfs  >= history.nodf) / length(history.r1.simulated.nodfs)
history.nodf.r1.z.value = (history.nodf - mean(history.r1.simulated.nodfs)) / sd(history.r1.simulated.nodfs)
history.nodf.r1.p.value
history.nodf.r1.z.value

history.nodf.curveball.p.value = sum(history.curveball.simulated.nodfs  >= history.nodf) / length(history.curveball.simulated.nodfs)
history.nodf.curveball.z.value = (history.nodf - mean(history.curveball.simulated.nodfs)) / sd(history.curveball.simulated.nodfs)
history.nodf.curveball.p.value
history.nodf.curveball.z.value

history.temperature.r1.p.value = sum(history.r1.simulated.temperatures  <= history.temp) / length(history.r1.simulated.temperatures)
history.temperature.r1.z.value = (history.temp - mean(history.r1.simulated.temperatures)) / sd(history.r1.simulated.temperatures)
history.temperature.r1.p.value
history.temperature.r1.z.value

history.temperature.curveball.p.value = sum(history.curveball.simulated.temperatures  <= history.temp) / length(history.curveball.simulated.temperatures)
history.temperature.curveball.z.value = (history.temp - mean(history.curveball.simulated.temperatures)) / sd(history.curveball.simulated.temperatures)
history.temperature.curveball.p.value
history.temperature.curveball.z.value


#plotting this

plot.title <- "history.bootstrapping.png"
png(file = plot.title)
par(mfrow=c(2,2))

#NODF 
plot(density(history.r1.simulated.nodfs), xlim = c(min(history.r1.simulated.nodfs), as.numeric(history.nodf+1)), 
      main="NODF - R1", xlab = "r1.simulated.nodfs")
abline(v=history.nodf, lwd=3, col='gold')
plot( density(history.curveball.simulated.nodfs), xlim = c(min(history.curveball.simulated.nodfs), as.numeric(history.nodf+1)),
      main="NODF - Curveball", xlab = "curveball.simulated.nodfs")
abline(v=history.nodf, lwd=3, col='gold')

#Temperature

plot( density(history.r1.simulated.temperatures), xlim = c(as.numeric(history.temp-1), max(history.r1.simulated.temperatures)),
      main="Temperature - R1",xlab = "r1.simulated.temps")
abline(v=history.temp, lwd=3, col='gold')
plot( density(history.curveball.simulated.temperatures), xlim = c(as.numeric(history.temp-1), max(history.curveball.simulated.temperatures)),
      main="Temperature - Curveball", xlab = "curveball.simulated.temps")
abline(v=history.temp, lwd=3, col='gold')

dev.off()


# 1.2.6. marveldc ####

null.marveldc.r1 <- nullmodel(d.marveldc, 'r1')
marveldc.r1.simulations <- simulate(null.marveldc.r1, nsim=nsimul)

null.marveldc.curveball <- nullmodel(d.marveldc, 'curveball')
marveldc.curveball.simulations <- simulate(null.marveldc.curveball, nsim=nsimul)

marveldc.r1.simulated.temperatures <- c()
marveldc.r1.simulated.nodfs <- c()
marveldc.curveball.simulated.temperatures <- c()
marveldc.curveball.simulated.nodfs <- c()

for (i in 1:nsimul){
  #print(i)
  R1Matrix <-   marveldc.r1.simulations[,,i]
  CurveballMatrix <- marveldc.curveball.simulations[,,i]
  marveldc.r1.simulated.temperatures[i] <- nestedtemp(R1Matrix)$statistic
  marveldc.r1.simulated.nodfs[i] <- nestednodf(R1Matrix)$statistic['NODF']
  marveldc.curveball.simulated.temperatures[i] <- nestedtemp(CurveballMatrix)$statistic
  marveldc.curveball.simulated.nodfs[i] <- nestednodf(CurveballMatrix)$statistic['NODF']
}

#Bootstrapped significance tests

marveldc.nodf.r1.p.value = sum(marveldc.r1.simulated.nodfs  >= marveldc.nodf) / length(marveldc.r1.simulated.nodfs)
marveldc.nodf.r1.z.value = (marveldc.nodf - mean(marveldc.r1.simulated.nodfs)) / sd(marveldc.r1.simulated.nodfs)
marveldc.nodf.r1.p.value
marveldc.nodf.r1.z.value

marveldc.nodf.curveball.p.value = sum(marveldc.curveball.simulated.nodfs  >= marveldc.nodf) / length(marveldc.curveball.simulated.nodfs)
marveldc.nodf.curveball.z.value = (marveldc.nodf - mean(marveldc.curveball.simulated.nodfs)) / sd(marveldc.curveball.simulated.nodfs)
marveldc.nodf.curveball.p.value
marveldc.nodf.curveball.z.value

marveldc.temperature.r1.p.value = sum(marveldc.r1.simulated.temperatures  <= marveldc.temp) / length(marveldc.r1.simulated.temperatures)
marveldc.temperature.r1.z.value = (marveldc.temp - mean(marveldc.r1.simulated.temperatures)) / sd(marveldc.r1.simulated.temperatures)
marveldc.temperature.r1.p.value
marveldc.temperature.r1.z.value

marveldc.temperature.curveball.p.value = sum(marveldc.curveball.simulated.temperatures  <= marveldc.temp) / length(marveldc.curveball.simulated.temperatures)
marveldc.temperature.curveball.z.value = (marveldc.temp - mean(marveldc.curveball.simulated.temperatures)) / sd(marveldc.curveball.simulated.temperatures)
marveldc.temperature.curveball.p.value
marveldc.temperature.curveball.z.value


#plotting this

plot.title <- "marveldc.bootstrapping.png"
png(file = plot.title)
par(mfrow=c(2,2))

#NODF 
plot( density(marveldc.r1.simulated.nodfs), xlim = c(min(marveldc.r1.simulated.nodfs), as.numeric(marveldc.nodf+1)), 
      main="NODF - R1", xlab = "r1.simulated.nodfs")
abline(v=marveldc.nodf, lwd=3, col='gold')
plot( density(marveldc.curveball.simulated.nodfs), xlim = c(min(marveldc.curveball.simulated.nodfs), as.numeric(marveldc.nodf+1)),
      main="NODF - Curveball", xlab = "curveball.simulated.nodfs")
abline(v=marveldc.nodf, lwd=3, col='gold')

#Temperature

plot( density(marveldc.r1.simulated.temperatures), xlim = c(as.numeric(marveldc.temp-1), max(marveldc.r1.simulated.temperatures)),
      main="Temperature - R1",xlab = "r1.simulated.temps")
abline(v=marveldc.temp, lwd=3, col='gold')
plot( density(marveldc.curveball.simulated.temperatures), xlim = c(as.numeric(marveldc.temp-1), max(marveldc.curveball.simulated.temperatures)),
      main="Temperature - Curveball", xlab = "curveball.simulated.temps")
abline(v=marveldc.temp, lwd=3, col='gold')

dev.off()


# 1.2.7. planets #####

null.planets.r1 <- nullmodel(d.planets, 'r1')
planets.r1.simulations <- simulate(null.planets.r1, nsim=nsimul)

null.planets.curveball <- nullmodel(d.planets, 'curveball')
planets.curveball.simulations <- simulate(null.planets.curveball, nsim=nsimul)

planets.r1.simulated.temperatures <- c()
planets.r1.simulated.nodfs <- c()
planets.curveball.simulated.temperatures <- c()
planets.curveball.simulated.nodfs <- c()

for (i in 1:nsimul){
  #print(i)
  R1Matrix <-   planets.r1.simulations[,,i]
  CurveballMatrix <- planets.curveball.simulations[,,i]
  planets.r1.simulated.temperatures[i] <- nestedtemp(R1Matrix)$statistic
  planets.r1.simulated.nodfs[i] <- nestednodf(R1Matrix)$statistic['NODF']
  planets.curveball.simulated.temperatures[i] <- nestedtemp(CurveballMatrix)$statistic
  planets.curveball.simulated.nodfs[i] <- nestednodf(CurveballMatrix)$statistic['NODF']
}

#Bootstrapped significance tests

planets.nodf.r1.p.value = sum(planets.r1.simulated.nodfs  >= planets.nodf) / length(planets.r1.simulated.nodfs)
planets.nodf.r1.z.value = (planets.nodf - mean(planets.r1.simulated.nodfs)) / sd(planets.r1.simulated.nodfs)
planets.nodf.r1.p.value
planets.nodf.r1.z.value

planets.nodf.curveball.p.value = sum(planets.curveball.simulated.nodfs  >= planets.nodf) / length(planets.curveball.simulated.nodfs)
planets.nodf.curveball.z.value = (planets.nodf - mean(planets.curveball.simulated.nodfs)) / sd(planets.curveball.simulated.nodfs)
planets.nodf.curveball.p.value
planets.nodf.curveball.z.value

planets.temperature.r1.p.value = sum(planets.r1.simulated.temperatures  <= planets.temp) / length(planets.r1.simulated.temperatures)
planets.temperature.r1.z.value = (planets.temp - mean(planets.r1.simulated.temperatures)) / sd(planets.r1.simulated.temperatures)
planets.temperature.r1.p.value
planets.temperature.r1.z.value

planets.temperature.curveball.p.value = sum(planets.curveball.simulated.temperatures  <= planets.temp) / length(planets.curveball.simulated.temperatures)
planets.temperature.curveball.z.value = (planets.temp - mean(planets.curveball.simulated.temperatures)) / sd(planets.curveball.simulated.temperatures)
planets.temperature.curveball.p.value
planets.temperature.curveball.z.value

#plotting this

plot.title <- "planets.bootstrapping.png"
png(file = plot.title)
par(mfrow=c(2,2))

#NODF 
plot( density(planets.r1.simulated.nodfs), xlim = c(min(planets.r1.simulated.nodfs), as.numeric(planets.nodf+1)), 
      main="NODF - R1", xlab = "r1.simulated.nodfs")
abline(v=planets.nodf, lwd=3, col='gold')
plot( density(planets.curveball.simulated.nodfs), xlim = c(min(planets.curveball.simulated.nodfs), as.numeric(planets.nodf+1)),
      main="NODF - Curveball", xlab = "curveball.simulated.nodfs")
abline(v=planets.nodf, lwd=3, col='gold')

#Temperature

plot( density(planets.r1.simulated.temperatures), xlim = c(as.numeric(planets.temp-1), max(planets.r1.simulated.temperatures)),
      main="Temperature - R1",xlab = "r1.simulated.temps")
abline(v=planets.temp, lwd=3, col='gold')
plot( density(planets.curveball.simulated.temperatures), xlim = c(as.numeric(planets.temp-1), max(planets.curveball.simulated.temperatures)),
      main="Temperature - Curveball", xlab = "curveball.simulated.temps")
abline(v=planets.temp, lwd=3, col='gold')

dev.off()


# 1.2.8. full dataset ######

null.full.r1 <- nullmodel(d.trivia, 'r1')
full.r1.simulations <- simulate(null.full.r1, nsim=nsimul)

null.full.curveball <- nullmodel(d.trivia, 'curveball')
full.curveball.simulations <- simulate(null.full.curveball, nsim=nsimul)

full.r1.simulated.temperatures <- c()
full.r1.simulated.nodfs <- c()
full.curveball.simulated.temperatures <- c()
full.curveball.simulated.nodfs <- c()

for (i in 1:nsimul){
  #print(i)
  R1Matrix <-   full.r1.simulations[,,i]
  CurveballMatrix <- full.curveball.simulations[,,i]
  full.r1.simulated.temperatures[i] <- nestedtemp(R1Matrix)$statistic
  full.r1.simulated.nodfs[i] <- nestednodf(R1Matrix)$statistic['NODF']
  full.curveball.simulated.temperatures[i] <- nestedtemp(CurveballMatrix)$statistic
  full.curveball.simulated.nodfs[i] <- nestednodf(CurveballMatrix)$statistic['NODF']
}

#Bootstrapped significance tests

full.nodf.r1.p.value = sum(full.r1.simulated.nodfs  >= full.nodf) / length(full.r1.simulated.nodfs)
full.nodf.r1.z.value = (full.nodf - mean(full.r1.simulated.nodfs)) / sd(full.r1.simulated.nodfs)
full.nodf.r1.p.value
full.nodf.r1.z.value

full.nodf.curveball.p.value = sum(full.curveball.simulated.nodfs  >= full.nodf) / length(full.curveball.simulated.nodfs)
full.nodf.curveball.z.value = (full.nodf - mean(full.curveball.simulated.nodfs)) / sd(full.curveball.simulated.nodfs)
full.nodf.curveball.p.value
full.nodf.curveball.z.value

full.temperature.r1.p.value = sum(full.r1.simulated.temperatures  <= full.temp) / length(full.r1.simulated.temperatures)
full.temperature.r1.z.value = (full.temp - mean(full.r1.simulated.temperatures)) / sd(full.r1.simulated.temperatures)
full.temperature.r1.p.value
full.temperature.r1.z.value

full.temperature.curveball.p.value = sum(full.curveball.simulated.temperatures  <= full.temp) / length(full.curveball.simulated.temperatures)
full.temperature.curveball.z.value = (full.temp - mean(full.curveball.simulated.temperatures)) / sd(full.curveball.simulated.temperatures)
full.temperature.curveball.p.value
full.temperature.curveball.z.value

#plotting this

plot.title <- "full.bootstrapping.png"
png(file = plot.title)
par(mfrow=c(2,2))

#NODF 
plot( density(full.r1.simulated.nodfs), xlim = c(min(full.r1.simulated.nodfs), as.numeric(full.nodf+1)), 
      main="NODF - R1", xlab = "r1.simulated.nodfs")
abline(v=full.nodf, lwd=3, col='gold')
plot( density(full.curveball.simulated.nodfs), xlim = c(min(full.curveball.simulated.nodfs), as.numeric(full.nodf+1)),
      main="NODF - Curveball", xlab = "curveball.simulated.nodfs")
abline(v=full.nodf, lwd=3, col='gold')

#Temperature

plot( density(full.r1.simulated.temperatures), xlim = c(as.numeric(full.temp-1), max(full.r1.simulated.temperatures)),
      main="Temperature - R1",xlab = "r1.simulated.temps")
abline(v=full.temp, lwd=3, col='gold')
plot( density(full.curveball.simulated.temperatures), xlim = c(as.numeric(full.temp-1), max(full.curveball.simulated.temperatures)),
      main="Temperature - Curveball", xlab = "curveball.simulated.temps")
abline(v=full.temp, lwd=3, col='gold')

dev.off()


# 1.2.9. Plotting nestedness #####

#Carfeul now because we're going to load the bipartite package. 
#Make sure you de-activate it before running the other sections ofthe analysis.

library(bipartite)

plot.title <- "nestedness.real.plots.png"
png(file = plot.title)
par(mfrow=c(2,2))

#full set
matrix = sortweb(d.trivia, sort.order="dec", sequence=NULL)
nodf = nestednodf(matrix, order=T, weighted=FALSE, wbinary=FALSE)
plot(nodf, names=F,  xlab="full dataset")

#history
matrix = sortweb(d.history, sort.order="dec", sequence=NULL)
nodf = nestednodf(matrix, order=T, weighted=FALSE, wbinary=FALSE)
plot(nodf, names=F, xlab="American History")

#marveldc
matrix = sortweb(d.marveldc, sort.order="dec", sequence=NULL)
nodf = nestednodf(matrix, order=T, weighted=FALSE, wbinary=FALSE)
plot(nodf, names=F, xlab="Super Heroes")

#planets
matrix = sortweb(d.planets, sort.order="dec", sequence=NULL)
#computing  nestedness with "vegan" package
nodf = nestednodf(matrix, order=T, weighted=FALSE, wbinary=FALSE)
plot(nodf, names=F, xlab="Planets")

dev.off()

#Ploting  generated history matrices to show the effects
#of r1 and curveball:

#curveball

plot.title <- "nestedness.curveball.plots.png"
png(file = plot.title)
par(mfrow=c(2,2))

#full set
matrix = sortweb( full.curveball.simulations[,,12], sort.order="dec", sequence=NULL)
nodf = nestednodf(matrix, order=T, weighted=FALSE, wbinary=FALSE)
plot(nodf, names=F,  xlab="full dataset, curveball")

#history
matrix = sortweb( history.curveball.simulations[,,12], sort.order="dec", sequence=NULL)
nodf = nestednodf(matrix, order=T, weighted=FALSE, wbinary=FALSE)
plot(nodf, names=F, xlab="American History, curveball")

#marveldc
matrix = sortweb( marveldc.curveball.simulations[,,12], sort.order="dec", sequence=NULL)
nodf = nestednodf(matrix, order=T, weighted=FALSE, wbinary=FALSE)
plot(nodf, names=F, xlab="Super Heroes, curveball")

#planets
matrix = sortweb( planets.curveball.simulations[,,12], sort.order="dec", sequence=NULL)
#computing  nestedness with "vegan" package
nodf = nestednodf(matrix, order=T, weighted=FALSE, wbinary=FALSE)
plot(nodf, names=F, xlab="Planets, curveball")

dev.off()

#r1

plot.title <- "nestedness.r1.plots.png"
png(file = plot.title)
par(mfrow=c(2,2))

#full set
matrix = sortweb( full.r1.simulations[,,12], sort.order="dec", sequence=NULL)
nodf = nestednodf(matrix, order=T, weighted=FALSE, wbinary=FALSE)
plot(nodf, names=F,  xlab="full dataset, r1")

#history
matrix = sortweb( history.r1.simulations[,,12], sort.order="dec", sequence=NULL)
nodf = nestednodf(matrix, order=T, weighted=FALSE, wbinary=FALSE)
plot(nodf, names=F, xlab="American History, r1")

#marveldc
matrix = sortweb( marveldc.r1.simulations[,,12], sort.order="dec", sequence=NULL)
nodf = nestednodf(matrix, order=T, weighted=FALSE, wbinary=FALSE)
plot(nodf, names=F, xlab="Super Heroes, r1")

#planets
matrix = sortweb( planets.r1.simulations[,,12], sort.order="dec", sequence=NULL)
#computing  nestedness with "vegan" package
nodf = nestednodf(matrix, order=T, weighted=FALSE, wbinary=FALSE)
plot(nodf, names=F, xlab="Planets, r1")

dev.off()


#### 1.3. DATA MANIPULATION ####
# 1.3.O. Computing performance by theme ####

# we remove irrelevant data-frames from Nestedness analyses
rm(d.trivia)

# we compute the performance of the participants in each theme

study1_bytheme <- study1_final %>% mutate(theme="")
study1_bytheme$theme[study1_bytheme$question<=15]="solarsystem"
study1_bytheme$theme[study1_bytheme$question>30]="superheroes"
study1_bytheme$theme[study1_bytheme$theme==""]="americanhistory"

study1_bytheme_group <- study1_bytheme %>% 
  dplyr::select(-question) %>%
  group_by(id, theme) %>%
  dplyr::summarise(performance_by_theme=1-mean(score))

# we pivot wider 

study1_bytheme_wide <- study1_bytheme_group %>%
  pivot_wider(names_from=theme, values_from = performance_by_theme)

study1_full_final_theme <- full_join(study1_final, study1_bytheme_wide)

study1_full_theme <- full_join(study1_full_final_theme, study1_bytheme)

rm(study1_bytheme_group, study1_bytheme, study1_bytheme_wide, study1_full_final_theme)


# 1.3.1. Computing difficulty ####

# we remove irrelevant data-frames from Nestedness analyses

# we compute the difficulty of the questions (1-ratio of good responses, for each question)
study1_difficulty <- study1_final %>% 
  dplyr::select(question, score) %>%
  group_by(question) %>%
  dplyr::summarise(difficulty1=1-mean(score))

# we add this difficulty score to the theme dataset for future analyses

study1_full_theme_questions <- full_join(study1_full_theme, study1_difficulty)
rm(study1_full_theme)

# we isolate each condition
study1_difficulty_solar_system <- study1_difficulty %>% filter(question <= 15)
study1_difficulty_american_history <- study1_difficulty %>% filter(question > 15 & question < 31)
study1_difficulty_super_heroes <- study1_difficulty %>% filter(question>30)

rm(study1_difficulty)

# 1.3.2. Computing  performance ####
# I previously run the DATA CLEANING of STUDY 1 and use study1_final, which was the final dataset before turning it into a matrice

study1_performance_question <- study1_final %>% dplyr::select(id, question, score)

# I compute the performance score of each participant (average score of each participant for each condition)

study1_performance_participant_solar_system <- study1_performance_question %>%
  filter(question == 1 |question == 2 |question == 3 |question == 4 |question == 5 |question == 6 |question == 7 |question == 8 |question == 9 |question == 10 |question == 11 |question == 12 |question == 13 |question == 14 |question ==15) %>%
  group_by(id) %>% 
  dplyr::summarise(perf=mean(score))

study1_performance_participant_american_history <- study1_performance_question %>%
  filter(question == 16 |question == 17 |question == 18 |question == 19 |question == 20 |question == 21 |question == 22 |question == 23 |question == 24 |question == 25 |question == 26 |question == 27 |question == 28 |question == 29 |question ==30) %>%
  group_by(id) %>% 
  dplyr::summarise(perf=mean(score))

study1_performance_participant_super_heroes <- study1_performance_question %>%
  filter(question == 31 |question == 32 |question == 33 |question == 34 |question == 35 |question == 36 |question == 37 |question == 38 |question == 39 |question == 40 |question == 41 |question == 42 |question == 43 |question == 44 |question ==45) %>%
  group_by(id) %>% 
  dplyr::summarise(perf=mean(score))

# Then, for each question, I select the subset of participants who got this question right and compute the average performance score of those participants
# Average overall performance of the participants that got the question right

u1 <- study1_performance_question %>% filter(question==1, score==1) %>%
  full_join(study1_performance_participant_solar_system) %>% drop_na(score) %>% group_by(question) %>% summarise(question=1, perf=mean(perf))
u2 <- study1_performance_question %>% filter(question==2, score==1) %>%
  full_join(study1_performance_participant_solar_system) %>% drop_na(score) %>% group_by(question) %>% summarise(question=2, perf=mean(perf))
u3 <- study1_performance_question %>% filter(question==3, score==1) %>%
  full_join(study1_performance_participant_solar_system) %>% drop_na(score) %>% group_by(question) %>% summarise(question=3, perf=mean(perf))
u4 <- study1_performance_question %>% filter(question==4, score==1) %>%
  full_join(study1_performance_participant_solar_system) %>% drop_na(score) %>% group_by(question) %>% summarise(question=4,perf=mean(perf))
u5 <- study1_performance_question %>% filter(question==5, score==1) %>%
  full_join(study1_performance_participant_solar_system) %>% drop_na(score) %>% group_by(question) %>% summarise(question=5,perf=mean(perf))
u6 <- study1_performance_question %>% filter(question==6, score==1) %>%
  full_join(study1_performance_participant_solar_system) %>% drop_na(score) %>% group_by(question) %>% summarise(question=6,perf=mean(perf))
u7 <- study1_performance_question %>% filter(question==7, score==1) %>%
  full_join(study1_performance_participant_solar_system) %>% drop_na(score) %>% group_by(question) %>% summarise(question=7,perf=mean(perf))
u8 <- study1_performance_question %>% filter(question==8, score==1) %>%
  full_join(study1_performance_participant_solar_system) %>% drop_na(score) %>% group_by(question) %>% summarise(question=8,perf=mean(perf))
u9 <- study1_performance_question %>% filter(question==9, score==1) %>%
  full_join(study1_performance_participant_solar_system) %>% drop_na(score) %>% group_by(question) %>% summarise(question=9,perf=mean(perf))
u10 <- study1_performance_question %>% filter(question==10, score==1) %>%
  full_join(study1_performance_participant_solar_system) %>% drop_na(score) %>% group_by(question) %>% summarise(question=10,perf=mean(perf))
u11 <- study1_performance_question %>% filter(question==11, score==1) %>%
  full_join(study1_performance_participant_solar_system) %>% drop_na(score) %>% group_by(question) %>% summarise(question=11,perf=mean(perf))
u12 <- study1_performance_question %>% filter(question==12, score==1) %>%
  full_join(study1_performance_participant_solar_system) %>% drop_na(score) %>% group_by(question) %>% summarise(question=12,perf=mean(perf))
u13 <- study1_performance_question %>% filter(question==13, score==1) %>%
  full_join(study1_performance_participant_solar_system) %>% drop_na(score) %>% group_by(question) %>% summarise(question=13,perf=mean(perf))
u14 <- study1_performance_question %>% filter(question==14, score==1) %>%
  full_join(study1_performance_participant_solar_system) %>% drop_na(score) %>% group_by(question) %>% summarise(question=14,perf=mean(perf))
u15 <- study1_performance_question %>% filter(question==15, score==1) %>%
  full_join(study1_performance_participant_solar_system) %>% drop_na(score) %>% group_by(question) %>% summarise(question=15,perf=mean(perf))
u16 <- study1_performance_question %>% filter(question==16, score==1) %>%
  full_join(study1_performance_participant_american_history) %>% drop_na(score) %>% group_by(question) %>% summarise(question=16,perf=mean(perf))
u17 <- study1_performance_question %>% filter(question==17, score==1) %>%
  full_join(study1_performance_participant_american_history) %>% drop_na(score) %>% group_by(question) %>% summarise(question=17,perf=mean(perf))
u18 <- study1_performance_question %>% filter(question==18, score==1) %>%
  full_join(study1_performance_participant_american_history) %>% drop_na(score) %>% group_by(question) %>% summarise(question=18,perf=mean(perf))
u19 <- study1_performance_question %>% filter(question==19, score==1) %>%
  full_join(study1_performance_participant_american_history) %>% drop_na(score) %>% group_by(question) %>% summarise(question=19,perf=mean(perf))
u20 <- study1_performance_question %>% filter(question==20, score==1) %>%
  full_join(study1_performance_participant_american_history) %>% drop_na(score) %>% group_by(question) %>% summarise(question=20,perf=mean(perf))
u21 <- study1_performance_question %>% filter(question==21, score==1) %>%
  full_join(study1_performance_participant_american_history) %>% drop_na(score) %>% group_by(question) %>% summarise(question=21,perf=mean(perf))
u22 <- study1_performance_question %>% filter(question==22, score==1) %>%
  full_join(study1_performance_participant_american_history) %>% drop_na(score) %>% group_by(question) %>% summarise(question=22,perf=mean(perf))
u23 <- study1_performance_question %>% filter(question==23, score==1) %>%
  full_join(study1_performance_participant_american_history) %>% drop_na(score) %>% group_by(question) %>% summarise(question=23,perf=mean(perf))
u24 <- study1_performance_question %>% filter(question==24, score==1) %>%
  full_join(study1_performance_participant_american_history) %>% drop_na(score) %>% group_by(question) %>% summarise(question=24,perf=mean(perf))
u25 <- study1_performance_question %>% filter(question==25, score==1) %>%
  full_join(study1_performance_participant_american_history) %>% drop_na(score) %>% group_by(question) %>% summarise(question=25,perf=mean(perf))
u26 <- study1_performance_question %>% filter(question==26, score==1) %>%
  full_join(study1_performance_participant_american_history) %>% drop_na(score) %>% group_by(question) %>% summarise(question=26,perf=mean(perf))
u27 <- study1_performance_question %>% filter(question==27, score==1) %>%
  full_join(study1_performance_participant_american_history) %>% drop_na(score) %>% group_by(question) %>% summarise(question=27,perf=mean(perf))
u28 <- study1_performance_question %>% filter(question==28, score==1) %>%
  full_join(study1_performance_participant_american_history) %>% drop_na(score) %>% group_by(question) %>% summarise(question=28,perf=mean(perf))
u29 <- study1_performance_question %>% filter(question==29, score==1) %>%
  full_join(study1_performance_participant_american_history) %>% drop_na(score) %>% group_by(question) %>% summarise(question=29,perf=mean(perf))
u30 <- study1_performance_question %>% filter(question==30, score==1) %>%
  full_join(study1_performance_participant_american_history) %>% drop_na(score) %>% group_by(question) %>% summarise(question=30,perf=mean(perf))
u31 <- study1_performance_question %>% filter(question==31, score==1) %>%
  full_join(study1_performance_participant_super_heroes) %>% drop_na(score) %>% group_by(question) %>% summarise(question=31,perf=mean(perf))
u32 <- study1_performance_question %>% filter(question==32, score==1) %>%
  full_join(study1_performance_participant_super_heroes) %>% drop_na(score) %>% group_by(question) %>% summarise(question=32,perf=mean(perf))
u33 <- study1_performance_question %>% filter(question==33, score==1) %>%
  full_join(study1_performance_participant_super_heroes) %>% drop_na(score) %>% group_by(question) %>% summarise(question=33,perf=mean(perf))
u34 <- study1_performance_question %>% filter(question==34, score==1) %>%
  full_join(study1_performance_participant_super_heroes) %>% drop_na(score) %>% group_by(question) %>% summarise(question=34,perf=mean(perf))
u35 <- study1_performance_question %>% filter(question==35, score==1) %>%
  full_join(study1_performance_participant_super_heroes) %>% drop_na(score) %>% group_by(question) %>% summarise(question=35,perf=mean(perf))
u36 <- study1_performance_question %>% filter(question==36, score==1) %>%
  full_join(study1_performance_participant_super_heroes) %>% drop_na(score) %>% group_by(question) %>% summarise(question=36,perf=mean(perf))
u37 <- study1_performance_question %>% filter(question==37, score==1) %>%
  full_join(study1_performance_participant_super_heroes) %>% drop_na(score) %>% group_by(question) %>% summarise(question=37,perf=mean(perf))
u38 <- study1_performance_question %>% filter(question==38, score==1) %>%
  full_join(study1_performance_participant_super_heroes) %>% drop_na(score) %>% group_by(question) %>% summarise(question=38,perf=mean(perf))
u39 <- study1_performance_question %>% filter(question==39, score==1) %>%
  full_join(study1_performance_participant_super_heroes) %>% drop_na(score) %>% group_by(question) %>% summarise(question=39,perf=mean(perf))
u40 <- study1_performance_question %>% filter(question==40, score==1) %>%
  full_join(study1_performance_participant_super_heroes) %>% drop_na(score) %>% group_by(question) %>% summarise(question=40,perf=mean(perf))
u41 <- study1_performance_question %>% filter(question==41, score==1) %>%
  full_join(study1_performance_participant_super_heroes) %>% drop_na(score) %>% group_by(question) %>% summarise(question=41,perf=mean(perf))
u42 <- study1_performance_question %>% filter(question==42, score==1) %>%
  full_join(study1_performance_participant_super_heroes) %>% drop_na(score) %>% group_by(question) %>% summarise(question=42,perf=mean(perf))
u43 <- study1_performance_question %>% filter(question==43, score==1) %>%
  full_join(study1_performance_participant_super_heroes) %>% drop_na(score) %>% group_by(question) %>% summarise(question=43,perf=mean(perf))
u44 <- study1_performance_question %>% filter(question==44, score==1) %>%
  full_join(study1_performance_participant_super_heroes) %>% drop_na(score) %>% group_by(question) %>% summarise(question=44,perf=mean(perf))
u45 <- study1_performance_question %>% filter(question==45, score==1) %>%
  full_join(study1_performance_participant_super_heroes) %>% drop_na(score) %>% group_by(question) %>% summarise(question=45,perf=mean(perf))

# I merge all the previous datasets together per condition

study1_performance_participant_question_correct_solar_system <- bind_rows(u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15)
study1_performance_participant_question_correct_american_history <- bind_rows(u16, u17, u18, u19, u20, u21, u22, u23, u24, u25, u26, u27, u28, u29, u30)
study1_performance_participant_question_correct_super_heroes <- bind_rows(u31, u32, u33, u34, u35, u36, u37, u38, u39, u40, u41, u42, u43, u44, u45)

#renaming the columns with proper names
names(study1_performance_participant_question_correct_solar_system)[2]="average_performance"
names(study1_performance_participant_question_correct_american_history)[2]="average_performance"
names(study1_performance_participant_question_correct_super_heroes)[2]="average_performance"

#removing irrelevant data-frames
rm(u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15)
rm(u16, u17, u18, u19, u20, u21, u22, u23, u24, u25, u26, u27, u28, u29, u30)
rm(u31, u32, u33, u34, u35, u36, u37, u38, u39, u40, u41, u42, u43, u44, u45)
rm(study1_performance_participant_solar_system, study1_performance_participant_american_history, study1_performance_participant_super_heroes)
rm(study1_performance_question)

#merging all datasets
study1_difficulty_performance_solar_system <- full_join(study1_difficulty_solar_system, study1_performance_participant_question_correct_solar_system)
study1_difficulty_performance_american_history <- full_join(study1_difficulty_american_history, study1_performance_participant_question_correct_american_history)
study1_difficulty_performance_super_heroes <- full_join(study1_difficulty_super_heroes, study1_performance_participant_question_correct_super_heroes)

#removing irrelevant data-frames
rm(study1_difficulty_solar_system, study1_performance_participant_question_correct_solar_system, study1_difficulty_american_history, study1_performance_participant_question_correct_american_history, study1_difficulty_super_heroes, study1_performance_participant_question_correct_super_heroes)

study1_all <- bind_rows(study1_difficulty_performance_solar_system, study1_difficulty_performance_american_history, study1_difficulty_performance_super_heroes)


#### 1.4. STATISTICAL ANALYSES ####
# 1.4.0. Solar system ####
ggplot_solarsystem <- ggplot(data=study1_difficulty_performance_solar_system, aes(x=difficulty1, y=average_performance, label=question)) + 
  geom_point(col="deepskyblue4", size=3) + 
  labs(title = "Solar System", x="", y="Mean knowledgeability of participants \n who correctly answered the question") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))  + 
  theme(text = element_text(size = 14))+ 
  ggrepel::geom_text_repel(aes(label = question), col="deepskyblue4") + geom_smooth(method="lm", color="coral3", se=FALSE) + 
  annotate("text", x=0.87, y=0.35, col="coral3", size=3, label="ß=0.39\np<.001\nR2=.89") + ylim(0.3,0.8)+ xlim(0,0.9)
ggplot_solarsystem
summary(lm(data=study1_difficulty_performance_solar_system, average_performance ~ difficulty1))

# 1.4.1. American history ####
# For the next analysis, I remove an outlier which is due to the question 24: one participant only got this question right, so the average performance is in fact the performance of this unique participant

ggplot_americanhistory <- ggplot(data=filter(study1_difficulty_performance_american_history, difficulty1<0.95), aes(x=difficulty1, y=average_performance, label=question)) + 
  geom_point(col="deepskyblue4",size=3) + 
  labs(title = "American history", x="Difficulty score of the question", y="Mean knowledgeability of participants \n who correctly answered the question") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(text = element_text(size = 14)) + 
  ggrepel::geom_text_repel(aes(label = question), col="deepskyblue4") + geom_smooth(method="lm", color="coral3", se=FALSE) + 
  annotate("text", x=0.87, y=0.35, col="coral3", size=3, label="ß=0.44\np<.001\nR2=.91")+ ylim(0.3,0.8)+ xlim(0,0.9)
ggplot_americanhistory
summary(lm(data=filter(study1_difficulty_performance_american_history, difficulty1<0.95), average_performance ~ difficulty1))

# 1.4.2. Super heroes ####

ggplot_superheroes <-ggplot(data=study1_difficulty_performance_super_heroes, aes(x=difficulty1, y=average_performance, label=question)) + 
  geom_point(col="deepskyblue4",size=3) + 
  labs(title = "Super heroes", x="", y="Mean knowledgeability of participants \n who correctly answered the question") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))+ 
  theme(text = element_text(size = 14))+ 
  ggrepel::geom_text_repel(aes(label = question), col="deepskyblue4") + geom_smooth(method="lm", color="coral3", se=FALSE) + 
  annotate("text", x=0.87, y=0.35, col="coral3", size=3, label="ß=0.41\np<.001\nR2=.92")+ ylim(0.3,0.8)+ xlim(0,0.9)
ggplot_superheroes
summary(lm(data=study1_difficulty_performance_super_heroes, average_performance ~ difficulty1))

library("cowplot")
plot_grid(ggplot_solarsystem, ggplot_americanhistory+ theme(axis.text.y = element_blank(),
                                                            axis.ticks.y = element_blank(),
                                                            axis.title.y = element_blank() ), 
          ggplot_superheroes+ theme(axis.text.y = element_blank(),
                                    axis.ticks.y = element_blank(),
                                    axis.title.y = element_blank() ), nrow=1, align = "v")

rm(ggplot_solarsystem,ggplot_americanhistory,ggplot_superheroes)

## STUDY 2 ###


#### 2. STUDY 2 ####

#### 2.1. DATA CLEANING ####
# 2.1.0. Cleaning the raw data ####

# I have to run DATA CLEANING AND KAPPA ANALYSIS (2. to 4.) and DATA MANIPULATION (1. and 2.)

# I import the dataset of the second study
rawdata_study2 <- read_csv("raw_study2.csv")

# I remove useless rows, renaming columns, adding id column
names(rawdata_study2)[names(rawdata_study2)=="Q52"] <- "free_consent"
names(rawdata_study2)[names(rawdata_study2)=="Q248"] <- "prolifics_ID"
names(rawdata_study2)[names(rawdata_study2)=="Q53"] <- "attention_check"
names(rawdata_study2)[names(rawdata_study2)=="Q232_1"] <- "exploratory_question"
data_study2 <- rawdata_study2 %>%
  slice(3:nrow(rawdata_study2))
data_study2$id=1:nrow(data_study2)


# I remove participants who failed attention check and failed completing the study, and isolating questions columns
study2_relevant_data <- data_study2[,-c(1:18)]%>% filter(attention_check!="No") %>% drop_na(exploratory_question)

# I remove irrelevant data-frames 
rm(rawdata_study2, data_study2)

# I partition the table into one table per VI and rename the columns
# Note: because of how the Qualtrics form was built, I had to manually identify from the Qualtrics form how were the questions ordered within each VI block

study2_vi1 <- study2_relevant_data[c(278, 7:20)]
names(study2_vi1) <- c('id', 'v1 32', 'v1 33', 'v1 34', 'v1 35', 'v1 36', 'v1 37', 'v1 38', 'v1 39', 'v1 40', 'v1 41', 'v1 42', 'v1 43', 'v1 44', 'v1 45')

study2_vi2 <- study2_relevant_data[c(278, 25:38)]
names(study2_vi2) <- c('id', 'v2 31', 'v2 33', 'v2 34', 'v2 35', 'v2 36', 'v2 37', 'v2 38', 'v2 39', 'v2 40', 'v2 41', 'v2 42', 'v2 43', 'v2 44', 'v2 45')

study2_vi3 <- study2_relevant_data[c(278, 43:56)]
names(study2_vi3) <- c('id', 'v3 32',	'v3 31',	'v3 34',	'v3 35',	'v3 36',	'v3 37',	'v3 38',	'v3 39',	'v3 40',	'v3 41',	'v3 42',	'v3 43',	'v3 44',	'v3 45')

study2_vi4 <- study2_relevant_data[c(278, 61:74)]
names(study2_vi4) <- c('id','v4 32',	'v4 33',	'v4 31',	'v4 35',	'v4 36',	'v4 37',	'v4 38',	'v4 39',	'v4 40',	'v4 41',	'v4 42',	'v4 43',	'v4 44',	'v4 45')

study2_vi5 <- study2_relevant_data[c(278, 79:92)]
names(study2_vi5) <- c('id', 'v5 32',	'v5 33',	'v5 34',	'v5 31',	'v5 36',	'v5 37',	'v5 38',	'v5 39',	'v5 40',	'v5 41',	'v5 42',	'v5 43',	'v5 44',	'v5 45')

study2_vi6 <- study2_relevant_data[c(278, 97:110)]
names(study2_vi6) <- c('id', 'v6 32',	'v6 33',	'v6 34',	'v6 31',	'v6 35',	'v6 37',	'v6 38',	'v6 39',	'v6 40',	'v6 41',	'v6 42',	'v6 43', 'v6 44',	'v6 45')

study2_vi7 <- study2_relevant_data[c(278, 115:128)]
names(study2_vi7) <- c('id', 'v7 32',	'v7 33',	'v7 34',	'v7 31',	'v7 36',	'v7 35',	'v7 38',	'v7 39',	'v7 40',	'v7 41',	'v7 42',	'v7 43',	'v7 44',	'v7 45')

study2_vi8 <- study2_relevant_data[c(278, 133:146)]
names(study2_vi8) <- c('id', 'v8 32',	'v8 33',	'v8 34',	'v8 31',	'v8 36',	'v8 37',	'v8 39',	'v8 35',	'v8 40',	'v8 41',	'v8 42',	'v8 43',	'v8 44',	'v8 45')

study2_vi9 <- study2_relevant_data[c(278, 151:164)]
names(study2_vi9) <- c('id', 'v9 32', 'v9 33', 'v9 34', 'v9 31',	'v9 36', 'v9 37',	'v9 38',	'v9 35', 'v9 40',	'v9 41',	'v9 42',	'v9 43',	'v9 44',	'v9 45')

study2_vi10 <- study2_relevant_data[c(278, 169:182)]
names(study2_vi10) <- c('id', 'v10 32',	'v10 33',	'v10 34',	'v10 31',	'v10 36',	'v10 37',	'v10 38',	'v10 39',	'v10 35',	'v10 41', 'v10 42',	'v10 43',	'v10 44',	'v10 45')

study2_vi11 <- study2_relevant_data[c(278, 187:200)]
names(study2_vi11) <- c('id', 'v11 32',	'v11 33',	'v11 34',	'v11 31',	'v11 36',	'v11 37',	'v11 38',	'v11 39',	'v11 35',	'v11 40', 'v11 42',	'v11 43',	'v11 44',	'v11 45')

study2_vi12 <- study2_relevant_data[c(278, 205:218)]
names(study2_vi12) <- c('id', 'v12 32',	'v12 33',	'v12 34',	'v12 31',	'v12 36',	'v12 37',	'v12 38',	'v12 39',	'v12 35',	'v12 41',	'v12 40',	'v12 43', 'v12 44',	'v12 45')

study2_vi13 <- study2_relevant_data[c(278, 223:236)]
names(study2_vi13) <- c('id', 'v13 32',	'v13 33',	'v13 34', 'v13 31',	'v13 36',	'v13 37',	'v13 38',	'v13 39',	'v13 35',	'v13 41',	'v13 42',	'v13 40',	'v13 44','v13 45')

study2_vi14 <- study2_relevant_data[c(278, 241:254)]
names(study2_vi14) <- c('id', 'v14 32', 'v14 33',	'v14 34',	'v14 31',	'v14 36','v14 37',	'v14 38',	'v14 39',	'v14 35',	'v14 41',	'v14 42',	'v14 43','v14 40',	'v14 45')

study2_vi15 <- study2_relevant_data[c(278, 259:272)]
names(study2_vi15) <- c('id', 'v15 32',	'v15 33',	'v15 34',	'v15 31',	'v15 36',	'v15 37',	'v15 38',	'v15 39',	'v15 35',	'v15 41',	'v15 42',	'v15 43',	'v15 44',	'v15 40')

# I merge all the datasets

study2_all <- merge(merge(merge(merge(merge(merge(merge(merge(merge(merge(merge(merge(merge(merge(
  study2_vi1, study2_vi2),
  study2_vi3),
  study2_vi4),
  study2_vi5),
  study2_vi6),
  study2_vi7),
  study2_vi8),
  study2_vi9),
  study2_vi10),
  study2_vi11),
  study2_vi12),
  study2_vi13),
  study2_vi14),
  study2_vi15)

# I remove irrelevant data-frames 
rm(study2_vi1, study2_vi2, study2_vi3, study2_vi4, study2_vi5, study2_vi6, study2_vi7, study2_vi8, study2_vi9, study2_vi10, study2_vi11, study2_vi12, study2_vi13, study2_vi14, study2_vi15)

# I delete irrelevant columns and pivot longer the table 
# 'question' is the question that the Virtual Individual got right
# 'question_inferred' are the questions that participants had to evaluate for each Virtual Individual

study2_all_long <- study2_all %>% pivot_longer(cols = -id, names_to=c("question", "question_inferred"), names_sep=" ") %>%
  drop_na(value)

# I delete v in the vi column and turn the variable into a numeric variable
study2_all_long$question <- gsub("[^0-9.]", "",  study2_all_long$question)
study2_all_long$question <- as.numeric(study2_all_long$question)
study2_all_long$question_inferred <- as.numeric(study2_all_long$question_inferred)
study2_all_long$value <- as.numeric(study2_all_long$value)

# For consistence with previous data, I add 30, because question 1 in STUDY 2 is in fact question 31 in STUDY 1
study2_all_long$question = study2_all_long$question + 30

# I remove irrelevant data-frames 

rm(study2_all)

#1. Computing difficulty score 2 for the 'super heroes' condition

# I group by question_inferred and compute the associated difficulty score as the ratio of good responses for each question
study2_difficulty <- study2_all_long %>% dplyr::select(question_inferred, value) %>% 
  group_by(question_inferred) %>% dplyr::summarise(sum=sum(value==1),n=n()) %>% 
  mutate(difficulty2= 1-sum/n) %>%
  dplyr::select(question_inferred, difficulty2)

# 'question_inferred' becomes 'question', to associate to each question the difficulty score 2

names(study2_difficulty)[1] = "question"
study2_difficulty$question <- as.numeric(study2_difficulty$question)

# 2.1.1. Computing the average estimated performance score of the virtual individuals ####

# I compute the average performance score of each vi according to the participants who evaluated them, as the ratio of good answers they were reported to get right
study2_performance <- study2_all_long %>% dplyr::select(question, value) %>% 
  group_by(question) %>% 
  dplyr::summarise(sum=sum(value==1),n=n())%>% 
  mutate(average_inferred_performance=sum/n) %>%
  dplyr::select(question, average_inferred_performance)

# I add the difficulty score 2 of the 'super heroes' condition

study2_difficulty_performance_superheroes <- full_join(study2_performance, study2_difficulty)
rm(study2_performance, study2_difficulty)

# I add the difficulty score 1 of the 'super heroes' conditions (study 1)

study2_superheroes_full <- full_join(study2_difficulty_performance_superheroes, study1_difficulty_performance_super_heroes)
rm(study2_difficulty_performance_superheroes, study1_difficulty_performance_super_heroes)

# 2.1.2. Computing the estimated performance score of the virtual individuals ####

# I compute the performance score of each vi by each  participants, as the ratio of good answers they were reported to get right

study2_performance_byparticipant <- study2_all_long %>% dplyr::select(id, question, value) %>% 
  group_by(id, question) %>% 
  dplyr::summarise(sum=sum(value==1),n=n())%>% 
  mutate(inferred_performance=sum/n) %>% 
  dplyr::select(id, question, inferred_performance)


# I add the difficulty scores of the 'super heroes' condition

study2_superheroes_full_byparticipant <- full_join(study2_performance_byparticipant, study2_superheroes_full)

rm(study2_performance_byparticipant)



#### 2.2. STATISTICAL ANALYSES ####
library(ggplot2)

# Correlation between the two difficulty scores 
ggplot(data=study2_superheroes_full, aes(x=difficulty1, y=difficulty2)) + geom_point(col="deepskyblue4", size=5) + 
  labs(x="Real difficulty of the question \n (difficulty score 1)", y="Inferred difficulty of the question \n (difficulty score 2)") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))  + theme(text = element_text(size = 20))
summary(lm(data=study2_superheroes_full, difficulty2~ difficulty1))

# Correlations with average inferred performance (15 datapoints)

# Correlation between inferred performance score and difficulty score 1
ggplot(data=study2_superheroes_full, aes(x=difficulty1, y=average_inferred_performance)) + geom_point(col="deepskyblue4", size=5) + 
  labs(x="Difficulty Score 1 of the question \n associated to the Virtual Individual", y="Inferred Performance of the Virtual Individual") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))  + theme(text = element_text(size = 20))
summary(lm(data=study2_superheroes_full, average_inferred_performance ~ difficulty1))

# Correlation between inferred performance score and difficulty score 2
ggplot(data=study2_superheroes_full, aes(x=difficulty2, y=average_inferred_performance)) + geom_point(col="deepskyblue4", size=5) + 
  labs(x="Difficulty Score 2 of the question \n associated to the Virtual Individual", y="Inferred Performance of the Virtual Individual") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))  + theme(text = element_text(size = 20))
summary(lm(data=study2_superheroes_full, average_inferred_performance ~ difficulty2))

# Correlations with average inferred performance (500 datapoints)

# null model

library(lmerTest)
library(performance)
library(see)

null_model <- lmer(data=study2_superheroes_full_byparticipant, inferred_performance ~ (1|id))
summary(null_model)

# comparing null_model and test_model_1

test_model_1 <- lmer(data=study2_superheroes_full_byparticipant, inferred_performance ~ difficulty1 + (1|id))
summary(test_model_1)
ggplot(data=study2_superheroes_full_byparticipant, aes(x=difficulty1, y=inferred_performance)) + geom_point(position="jitter")

compare_performance(null_model, test_model_1, rank=TRUE)
plot(compare_performance(null_model, test_model_1, rank=TRUE))

AIC(null_model, test_model_1)
# comparing null_model and test_model_2

test_model_2 <- lmer(data=study2_superheroes_full_byparticipant, inferred_performance ~ difficulty2 + (1|id))
summary(test_model_2)
ggplot(data=study2_superheroes_full_byparticipant, aes(x=difficulty2, y=inferred_performance)) + geom_point(position="jitter")

compare_performance(null_model, test_model_2, rank=TRUE)
plot(compare_performance(null_model, test_model_2, rank=TRUE))
AIC(null_model, test_model_2)

rm(null_model, test_model_2,test_model_1)

#### 2.3. EXPLORATORY ANALYSES ####

# 2.3.0. Accuracy and overstimation ####

# I compute the correlations of inferred and actual performance scores based on questions

ggplot(data=study2_superheroes_full, aes(x=average_inferred_performance, y=average_performance, label = question)) + 
  geom_point(col="deepskyblue4", size=5)+ 
  ggrepel::geom_text_repel(aes(label = question)) +
  labs(x="Inferred performance", y="Real performance") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))  + theme(text = element_text(size = 20))

summary(lm(data=study2_superheroes_full, average_inferred_performance~ average_performance)) 

# I compute the Accuracy Score and Overestimation Score

study2_superheroes_full_byparticipant$abs_diff=abs(study2_superheroes_full_byparticipant$inferred_performance-study2_superheroes_full_byparticipant$average_performance)
study2_superheroes_full_byparticipant$diff=study2_superheroes_full_byparticipant$inferred_performance-study2_superheroes_full_byparticipant$average_performance

study2_superheroes_accuracy <- study2_superheroes_full_byparticipant %>% group_by(id) %>% dplyr::summarise(accuracy_score=(sum(abs_diff))/5)

study2_superheroes_overestimation <- study2_superheroes_full_byparticipant %>% group_by(id) %>% dplyr::summarise(overestimation_score=(sum(diff))/5)

study2_superheroes_accuracy_overestimation <- full_join(study2_superheroes_accuracy, study2_superheroes_overestimation)

rm(study2_superheroes_accuracy,study2_superheroes_overestimation)

# I take the autoscore in the raw data

study2_autoscore <- study2_relevant_data %>% dplyr::select(id, exploratory_question)
names(study2_autoscore)[2]="autoscore"

study2_exploratory_full <- full_join(study2_superheroes_accuracy_overestimation, study2_autoscore)
rm(study2_superheroes_accuracy_overestimation, study2_autoscore, study2_relevant_data)

study2_exploratory_full$autoscore <- as.numeric(study2_exploratory_full$autoscore)

# I test the correlation between autoscore and accuracy and overstimation scores

ggplot(data=study2_exploratory_full, aes(x=autoscore, y=accuracy_score)) + 
  geom_point()+ geom_smooth(method="lm", se=FALSE)
summary(lm(data=study2_exploratory_full, accuracy_score~ autoscore)) 

ggplot(data=study2_exploratory_full, aes(x=autoscore, y=overestimation_score)) + 
  geom_point()+ geom_smooth(method="lm", se=FALSE) 
summary(lm(data=study2_exploratory_full, overestimation_score~ autoscore)) 

# 2.3.1. Nestedness compatibility ####

# I compute the difficulty of each question according to each real participant 
study2_all_long_difficulty <- study2_all_long %>% group_by(id, question_inferred) %>%
  dplyr::mutate(difficulty_question_inferred= 1-sum(value==1)/n())

# I add a column for the difficulty of the question that characterizes the VI

study2_all_long_unique <-study2_all_long_difficulty %>% select(id, question_inferred, difficulty_question_inferred) %>%
 unique() 

names(study2_all_long_unique)[2] = "question"
names(study2_all_long_unique)[3] = "difficulty_question_vi"

study2_all_long_full_difficulty <- full_join(study2_all_long_difficulty, study2_all_long_unique, by = c("id", "question")) %>%
  drop_na(value)

# I compute the nestedness compatibility of each response

study2_nested_compatible <- study2_all_long_full_difficulty %>%
  filter (difficulty_question_inferred <= difficulty_question_vi & value == 1 | difficulty_question_inferred > difficulty_question_vi & value == 2) %>%
  mutate(nestedness_compatibility = 1)
  
study2_nested_incompatible <- study2_all_long_full_difficulty %>%
  filter(difficulty_question_inferred <= difficulty_question_vi & value == 2 | difficulty_question_inferred > difficulty_question_vi & value == 1) %>%
  mutate(nestedness_compatibility = 0)

study2_all_long_nestedness <- full_join(study2_nested_compatible, study2_nested_incompatible)

#Removing irrelevant dataframes
remove(study2_all_long, study2_nested_compatible, study2_nested_incompatible, study2_all_long_difficulty, study2_all_long_full_difficulty, study2_all_long_unique)


## glmer model #

library(lmerTest)
library(performance)
library(see)

study2_glmer_model <- glmer(data=study2_all_long_nestedness, nestedness_compatibility ~ difficulty_question_inferred + (1|id) + (1|question) + (1|question_inferred), family = binomial(link="logit"))
summary(study2_glmer_model)
rm(study2_glmer_model)

# Test Olivier #

study2_all_long_nestedness <- study2_all_long_nestedness %>%  group_by(id) %>% mutate(subject.average.NC = mean(nestedness_compatibility))
all.subject.averages <- study2_all_long_nestedness[,c(1,8)]
all.subject.averages <- unique(all.subject.averages)
all.subject.averages
hist(all.subject.averages$subject.average.NC)
wilcox.test(all.subject.averages$subject.average.NC, mu = 0.5, alternative = "greater")
rm(all.subject.averages)

## Other Analysis ##

#In this section, I try to identify which questions are the more nestedness compatible ones 
#and if difficulty can predict nestedness compatibility

#Plots to explore the data 
ggplot(data=study2_all_long_nestedness, aes(x=difficulty_question_inferred, y= nestedness_compatibility)) + 
  geom_point(position="jitter") +
  labs(x="Difficulty of the inferred question", y="Nestedness compatibility")

ggplot(data=study2_all_long_nestedness, aes(x=difficulty_question_vi, y= nestedness_compatibility)) + 
  geom_point(position="jitter") +
  labs(x="Difficulty of the question associated to the VI", y="Nestedness compatibility")

#I compute the mean nestedness compatibility and mean difficulty for each of 15 questions, averaging on the answers given by all real participants

study2_nestedness_per_question <- study2_all_long_nestedness %>% group_by(question) %>%
  dplyr::summarise(avg_nestedness_per_question = mean(nestedness_compatibility), std_nestedness = sd(nestedness_compatibility), avg_difficulty_inferred = sum(value == 1)/n()) %>%
  ungroup()

#graph to visualize which questions are the most nested

study2_nestedness_per_question %>% dplyr::summarise(mean(avg_nestedness_per_question))

ggplot(data=study2_nestedness_per_question, aes(x = factor(question), y = avg_nestedness_per_question)) + geom_col(fill = '#00CCCC') + 
  geom_errorbar(aes(ymin = avg_nestedness_per_question - std_nestedness, ymax = 1, width = 0.1)) + geom_hline(yintercept = 0.808, color = 'red')

#I plot nestedness VS difficulty to see how the two variables evoluate 

ggplot(data=study2_nestedness_per_question, aes(x = avg_difficulty_inferred, y = avg_nestedness_per_question)) + geom_point() + geom_text(label = study2_nestedness_per_question$question, nudge_x = 0.002, nudge_y = 0.002)

summary(lm(data = study2_nestedness_per_question, avg_nestedness_per_question ~ avg_difficulty_inferred))

rm(study2_glmer_model)

#### 3. STUDY 3 ####
##### 3.1. DATA CLEANING ####
# 3.1.0. Cleaning the raw data ####
# I have to run DATA CLEANING AND KAPPA ANALYSIS (2. to 4.) and DATA MANIPULATION (1. and 2.)

# I import the dataset of the second study
rawdata_study3 <- read_csv("raw_study3.csv")

# I remove useless rows, renaming columns, adding id column
names(rawdata_study3)[names(rawdata_study3)=="Q248"] <- "prolifics_ID"
names(rawdata_study3)[names(rawdata_study3)=="autoscore_1"] <- "autoscore"

data_study3 <- rawdata_study3 %>%
  slice(3:nrow(rawdata_study3))
data_study3$id=1:nrow(data_study3)


# I remove participants who failed attention check and failed completing the study, and isolating questions columns
study3_relevant_data <- data_study3[,-c(1:18)]%>% filter(attention_check!="No") %>% drop_na(autoscore)

# I remove irrelevant data-frames 
rm(rawdata_study3, data_study3)

# I partition the table into one table per VI and rename the columns

study3_vi1 <- study3_relevant_data[c(350, 7:20)]
names(study3_vi1) <- c('id', 'v1 2', 'v1 3', 'v1 4', 'v1 5', 'v1 6', 'v1 7', 'v1 8', 'v1 9', 'v1 10', 'v1 11', 'v1 12', 'v1 13', 'v1 14', 'v1 15')

study3_vi2 <- study3_relevant_data[c(350, 25:38)]
names(study3_vi2) <- c('id', 'v2 1', 'v2 3', 'v2 4', 'v2 5', 'v2 6', 'v2 7', 'v2 8', 'v2 9', 'v2 10', 'v2 11', 'v2 12', 'v2 13', 'v2 14', 'v2 15')

study3_vi3 <- study3_relevant_data[c(350, 43:56)]
names(study3_vi3) <- c('id', 'v3 1',	'v3 2',	'v3 4',	'v3 5',	'v3 6',	'v3 7',	'v3 8',	'v3 9',	'v3 10',	'v3 11',	'v3 12',	'v3 13',	'v3 14',	'v3 15')

study3_vi4 <- study3_relevant_data[c(350, 61:74)]
names(study3_vi4) <- c('id','v4 1',	'v4 2',	'v4 3',	'v4 5',	'v4 6',	'v4 7',	'v4 8',	'v4 9',	'v4 10',	'v4 11',	'v4 12',	'v4 13',	'v4 14',	'v4 15')

study3_vi5 <- study3_relevant_data[c(350, 79:92)]
names(study3_vi5) <- c('id', 'v5 1',	'v5 2',	'v5 3',	'v5 4',	'v5 6',	'v5 7',	'v5 8',	'v5 9',	'v5 10',	'v5 11',	'v5 12',	'v5 13',	'v5 14',	'v5 15')

study3_vi6 <- study3_relevant_data[c(350, 97:110)]
names(study3_vi6) <- c('id', 'v6 1',	'v6 2',	'v6 3',	'v6 4',	'v6 5',	'v6 7',	'v6 8',	'v6 9',	'v6 10',	'v6 11',	'v6 12',	'v6 13', 'v6 14',	'v6 15')

study3_vi7 <- study3_relevant_data[c(350, 115:128)]
names(study3_vi7) <- c('id', 'v7 1',	'v7 2',	'v7 3',	'v7 4',	'v7 5',	'v7 6',	'v7 8',	'v7 9',	'v7 10',	'v7 11',	'v7 12',	'v7 13',	'v7 14',	'v7 15')

study3_vi8 <- study3_relevant_data[c(350, 133:146)]
names(study3_vi8) <- c('id', 'v8 1',	'v8 2',	'v8 3',	'v8 4',	'v8 5',	'v8 6',	'v8 7',	'v8 9',	'v8 10',	'v8 11',	'v8 12',	'v8 13',	'v8 14',	'v8 15')

study3_vi9 <- study3_relevant_data[c(350, 151:164)]
names(study3_vi9) <- c('id', 'v9 1', 'v9 2', 'v9 3', 'v9 4',	'v9 5', 'v9 6',	'v9 7',	'v9 8', 'v9 10',	'v9 11',	'v9 12',	'v9 13',	'v9 14',	'v9 15')

study3_vi10 <- study3_relevant_data[c(350, 169:182)]
names(study3_vi10) <- c('id', 'v10 1',	'v10 2',	'v10 3',	'v10 4',	'v10 5',	'v10 6',	'v10 7',	'v10 8',	'v10 9',	'v10 11', 'v10 12',	'v10 13',	'v10 14',	'v10 15')

study3_vi11 <- study3_relevant_data[c(350, 187:200)]
names(study3_vi11) <- c('id', 'v11 1',	'v11 2',	'v11 3',	'v11 4',	'v11 5',	'v11 6',	'v11 7',	'v11 8',	'v11 9',	'v11 10', 'v11 12',	'v11 13',	'v11 14',	'v11 15')

study3_vi12 <- study3_relevant_data[c(350, 205:218)]
names(study3_vi12) <- c('id', 'v12 1',	'v12 2',	'v12 3',	'v12 4',	'v12 5',	'v12 6',	'v12 7',	'v12 8',	'v12 9',	'v12 10',	'v12 11',	'v12 13', 'v12 14',	'v12 15')

study3_vi13 <- study3_relevant_data[c(350, 223:236)]
names(study3_vi13) <- c('id', 'v13 1',	'v13 2',	'v13 3', 'v13 4',	'v13 5',	'v13 6',	'v13 7',	'v13 8',	'v13 9',	'v13 10',	'v13 11',	'v13 12',	'v13 14','v13 15')

study3_vi14 <- study3_relevant_data[c(350, 241:254)]
names(study3_vi14) <- c('id', 'v14 1', 'v14 2',	'v14 3',	'v14 4',	'v14 5','v14 6',	'v14 7',	'v14 8',	'v14 9',	'v14 10',	'v14 11',	'v14 12','v14 13',	'v14 15')

study3_vi15 <- study3_relevant_data[c(350, 259:272)]
names(study3_vi15) <- c('id', 'v15 1',	'v15 2',	'v15 3',	'v15 4',	'v15 5',	'v15 6',	'v15 7',	'v15 8',	'v15 9',	'v15 10',	'v15 11',	'v15 12',	'v15 13',	'v15 14')

# I merge all the datasets

study3_all <- merge(merge(merge(merge(merge(merge(merge(merge(merge(merge(merge(merge(merge(merge(
  study3_vi1, study3_vi2),
  study3_vi3),
  study3_vi4),
  study3_vi5),
  study3_vi6),
  study3_vi7),
  study3_vi8),
  study3_vi9),
  study3_vi10),
  study3_vi11),
  study3_vi12),
  study3_vi13),
  study3_vi14),
  study3_vi15)

# I remove irrelevant data-frames 
rm(study3_vi1, study3_vi2, study3_vi3, study3_vi4, study3_vi5, study3_vi6, study3_vi7, study3_vi8, study3_vi9, study3_vi10, study3_vi11, study3_vi12, study3_vi13, study3_vi14, study3_vi15)

# I delete irrelevant columns and pivot longer the table 
# 'question' is the question that the Virtual Individual got right
# 'question_inferred' are the questions that participants had to evaluate for each Virtual Individual

study3_all_long <- study3_all %>% pivot_longer(cols = -id, names_to=c("question", "question_inferred"), names_sep=" ") %>%
  drop_na(value)

# I delete v in the vi column and turn the variable into a numeric variable
study3_all_long$question <- gsub("[^0-9.]", "",  study3_all_long$question)
study3_all_long$question <- as.numeric(study3_all_long$question)
study3_all_long$question_inferred <-as.numeric(study3_all_long$question_inferred)
study3_all_long$value <- as.numeric(study3_all_long$value)

# I remove irrelevant data-frames 

rm(study3_all)

# 3.1.1. Computing difficulty score 2 for the 'solar system' condition ####

# I group by question_inferred and compute the associated difficulty score as the ratio of good responses for each question

study3_difficulty <- study3_all_long %>% dplyr::select(question_inferred, value) %>% 
  group_by(question_inferred) %>% 
  dplyr::summarise(difficulty2=1-mean(value==1))

# 'question_inferred' becomes 'question', to associate to each question the difficulty score 2

names(study3_difficulty)[1] = "question"
study3_difficulty$question <- as.numeric(study3_difficulty$question)

# 3.1.2. Computing the average estimated performance score of the virtual individuals ####

# I compute the average performance score of each vi according to the participants who evaluated them, as the ratio of good answers they were reported to get right
study3_performance <- study3_all_long %>% dplyr::select(question, value) %>% 
  group_by(question) %>% 
  dplyr::summarise(average_inferred_performance=mean(value==1))

# I add the difficulty score 2 of the 'solarsystem' condition

study3_difficulty_performance_solarsystem <- full_join(study3_performance, study3_difficulty)
rm(study3_performance, study3_difficulty)

# I add the difficulty score 1 of the 'solar system' conditions (study 1)

study3_solarsystem_full <- full_join(study3_difficulty_performance_solarsystem, study1_all[1:15,])
rm(study3_difficulty_performance_solarsystem, study1_difficulty_performance_solar_system)

# 3.1.3. Computing the estimated performance score of the virtual individuals ####

# I compute the performance score of each vi by each  participants, as the ratio of good answers they were reported to get right

study3_performance_byparticipant <- study3_all_long %>% dplyr::select(id, question, value) %>% 
  group_by(id, question) %>% 
  dplyr::summarise(sum=sum(value==1),n=n())%>% 
  mutate(inferred_performance=sum/n) %>% 
  dplyr::select(id, question, inferred_performance)


# I add the difficulty scores of the 'solarsystem' condition

study3_solarsystem_full_byparticipant <- full_join(study3_performance_byparticipant, study3_solarsystem_full)

rm(study3_performance_byparticipant)

# 3.1.4. Preparing data for coding for objective performance score (not necessary to run) ####

# Selecting relevant data

study3_forcoding_relevant <- study3_relevant_data %>%
  dplyr::select(id, Q1, Q2, Q3, Q4, Q5, Q6, Q7, Q8, Q9, Q10, Q11, Q12, Q13, Q14, Q15)

# Isolating each question from Q1 to Q45 and the responses of all participants to this question and then collapsing all the tables together

db1 <- study3_forcoding_relevant %>% dplyr::select(id, Q1) %>% pivot_longer(cols=Q1, names_repair = "unique")
db2 <- study3_forcoding_relevant %>% dplyr::select(id, Q2) %>% pivot_longer(cols=Q2, names_repair = "unique")
db3 <- study3_forcoding_relevant %>% dplyr::select(id,  Q3) %>% pivot_longer(cols=Q3, names_repair = "unique")
db4 <- study3_forcoding_relevant %>% dplyr::select(id, Q4) %>% pivot_longer(cols=Q4, names_repair = "unique") 
db5 <- study3_forcoding_relevant %>% dplyr::select(id, Q5) %>% pivot_longer(cols=Q5, names_repair = "unique")
db6 <- study3_forcoding_relevant %>% dplyr::select(id, Q6) %>% pivot_longer(cols=Q6, names_repair = "unique")
db7 <- study3_forcoding_relevant %>% dplyr::select(id, Q7) %>% pivot_longer(cols=Q7, names_repair = "unique")
db8 <- study3_forcoding_relevant %>% dplyr::select(id, Q8) %>% pivot_longer(cols=Q8, names_repair = "unique")
db9 <- study3_forcoding_relevant %>% dplyr::select(id, Q9) %>% pivot_longer(cols=Q9, names_repair = "unique")
db10 <- study3_forcoding_relevant %>% dplyr::select(id, Q10) %>% pivot_longer(cols=Q10, names_repair = "unique")
db11 <- study3_forcoding_relevant %>% dplyr::select(id, Q11) %>% pivot_longer(cols=Q11, names_repair = "unique")
db12 <- study3_forcoding_relevant %>% dplyr::select(id, Q12) %>% pivot_longer(cols=Q12, names_repair = "unique")
db13 <- study3_forcoding_relevant %>% dplyr::select(id, Q13) %>% pivot_longer(cols=Q13, names_repair = "unique")
db14 <- study3_forcoding_relevant %>% dplyr::select(id, Q14) %>% pivot_longer(cols=Q14, names_repair = "unique")
db15 <- study3_forcoding_relevant %>% dplyr::select(id, Q15) %>% pivot_longer(cols=Q15, names_repair = "unique")

study3_to_be_rated_data <- bind_rows(db1, db2, db3, db4, db5, db6, db7, db8, db9, db10, db11, db12, db13, db14, db15)
names(study3_to_be_rated_data)[2] = "question"
names(study3_to_be_rated_data)[3] = "answer"

#removing irrelevant data-frames

rm(study3_forcoding_relevant)
rm(db1, db2, db3, db4, db5, db6, db7, db8, db9, db10, db11, db12, db13, db14, db15)

# I sample half of the dataset to attribute to one coder

#library(writexl)
#write_xlsx(study3_to_be_rated_data,"study3_nestedness_coding")
rm(study3_to_be_rated_data)
#



# 3.1.5. Aggregating databases after coding ####

library(readr)
library(tidyverse)
library(dplyr)
library("readxl")

study3_coded <- read_xlsx("study3_coding.xlsx") 


# 3.1.4. Final data cleaning ####

# I replace the coding 2 (that we used when participants responsded 'I don't know') by 0 ('I don't knwow' treated as a 'bad response'
study3_coded$code[study3_coded$code == 2] <- 0

# I select the relevant columns
study3_final_objective_score <- study3_coded %>%
  dplyr::select(id, question, code)

study3_final_objective_score$question <- gsub("[^0-9.]", "",  study3_final_objective_score$question)
study3_final_objective_score$question <- as.numeric(study3_final_objective_score$question)

#removing irrelevant data-frames 
rm(study3_coded)

# I compute the performance score of each participant (average score of each participant for each condition)

study3_objective_performance_score <- study3_final_objective_score %>%
  group_by(id) %>% 
  dplyr::summarise(perf=mean(code))
rm(study3_final_objective_score)


#### 3.2. STATISTICAL ANALYSES ####
library(ggplot2)

# Correlation between the two difficulty scores 
ggplot(data=study3_solarsystem_full, aes(x=difficulty1, y=difficulty2)) + geom_point(col="deepskyblue4", size=5) + 
  labs(x="Real difficulty of the question \n (difficulty score 1)", y="Inferred difficulty of the question \n (difficulty score 2)") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))  + theme(text = element_text(size = 20))
summary(lm(data=study3_solarsystem_full, difficulty2~ difficulty1))

# Correlations with average inferred performance (15 datapoints)

# Correlation between inferred performance score and difficulty score 1
ggplot(data=study3_solarsystem_full, aes(x=difficulty1, y=average_inferred_performance)) + geom_point(col="deepskyblue4", size=5) + 
  labs(x="Difficulty Score 1 of the question \n associated to the Virtual Individual", y="Inferred performance of the Virtual Individual") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))  + theme(text = element_text(size = 20))
summary(lm(data=study3_solarsystem_full, average_inferred_performance ~ difficulty1))

# Correlation between inferred performance score and difficulty score 2
ggplot(data=study3_solarsystem_full, aes(x=difficulty2, y=average_inferred_performance)) + geom_point(col="deepskyblue4", size=5) + 
  labs(x="Difficulty Score 2 of the question \n associated to the Virtual Individual", y="Inferred performance of the Virtual Individual") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))  + theme(text = element_text(size = 20))
summary(lm(data=study3_solarsystem_full, average_inferred_performance ~ difficulty2))


# Correlations with average inferred performance (500 datapoints)

# null model

library(lmerTest)
library(performance)
library(see)

null_model <- lmer(data=study3_solarsystem_full_byparticipant, inferred_performance ~ (1|id))
summary(null_model)

# comparing null_model and test_model_1

test_model_1 <- lmer(data=study3_solarsystem_full_byparticipant, inferred_performance ~ difficulty1 + (1|id))
summary(test_model_1)
ggplot(data=study3_solarsystem_full_byparticipant, aes(x=difficulty1, y=inferred_performance)) + geom_point(position="jitter")

compare_performance(null_model, test_model_1, rank=TRUE)
plot(compare_performance(null_model, test_model_1, rank=TRUE))
AIC(null_model, test_model_1)

# comparing null_model and test_model_2

test_model_2 <- lmer(data=study3_solarsystem_full_byparticipant, inferred_performance ~ difficulty2 + (1|id))
summary(test_model_2)
ggplot(data=study3_solarsystem_full_byparticipant, aes(x=difficulty2, y=inferred_performance)) + geom_point(position="jitter")

compare_performance(null_model, test_model_2, rank=TRUE)
plot(compare_performance(null_model, test_model_2, rank=TRUE))
AIC(null_model, test_model_2)

rm(test_model_1, test_model_2, null_model)

#### 3.3. EXPLORATORY ANALYSES ####
# 3.3.0. Accuracy and overstimation ####

# I compute the correlations of inferred and actual performance scores based on questions

ggplot(data=study3_solarsystem_full, aes(x=average_inferred_performance, y=average_performance, label = question)) + 
  geom_point(col="deepskyblue4", size=5)+  
  ggrepel::geom_text_repel(aes(label = question)) +
  labs(x="Inferred performance", y="Real performance") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))  + theme(text = element_text(size = 20))

summary(lm(data=study3_solarsystem_full, average_performance ~ average_inferred_performance )) 

# I compute the Accuracy Score and Overestimation Score

study3_solarsystem_full_byparticipant$abs_diff=abs(study3_solarsystem_full_byparticipant$inferred_performance-study3_solarsystem_full_byparticipant$average_performance)
study3_solarsystem_full_byparticipant$diff=study3_solarsystem_full_byparticipant$inferred_performance-study3_solarsystem_full_byparticipant$average_performance

study3_solarsystem_accuracy <- study3_solarsystem_full_byparticipant %>% group_by(id) %>% dplyr::summarise(accuracy_score=(sum(abs_diff))/5)

study3_solarsystem_overestimation <- study3_solarsystem_full_byparticipant %>% group_by(id) %>% dplyr::summarise(overestimation_score=(sum(diff))/5)

study3_solarsystem_accuracy_overestimation <- full_join(study3_solarsystem_accuracy, study3_solarsystem_overestimation)

rm(study3_solarsystem_accuracy,study3_solarsystem_overestimation)

# I take the autoscore in the raw data

study3_autoscore <- study3_relevant_data %>% dplyr::select(id, autoscore)

study3_exploratory <- full_join(study3_solarsystem_accuracy_overestimation, study3_autoscore)
rm(study3_solarsystem_accuracy_overestimation, study3_autoscore, study3_relevant_data)

study3_exploratory$autoscore <- as.numeric(study3_exploratory$autoscore)

study3_exploratory_2<-full_join(study3_exploratory, study3_solarsystem_full_byparticipant)

study3_exploratory_full <- full_join(study3_exploratory_2, study3_objective_performance_score)
rm(study3_exploratory_2, study3_objective_performance_score)

study3_exploratory_full$autoscore = study3_exploratory_full$autoscore/15

names(study3_exploratory_full)[names(study3_exploratory_full)=="autoscore"] <- "subjective_perf"
names(study3_exploratory_full)[names(study3_exploratory_full)=="perf"] <- "objective_perf"

rm(study3_exploratory)

# I test the correlation between subjective_perf and accuracy and overstimation scores

ggplot(data=study3_exploratory_full, aes(x=subjective_perf, y=accuracy_score)) + 
  geom_point()+ geom_smooth(method="lm", se=FALSE) +
  labs(x="Subjective performance of the participant", y="Accuracy of the participant") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + theme(text = element_text(size = 20))
summary(lm(data=study3_exploratory_full, accuracy_score~ subjective_perf)) 
cor.test(study3_exploratory_full$subjective_perf, study3_exploratory_full$accuracy_score)

ggplot(data=study3_exploratory_full, aes(x=subjective_perf, y=overestimation_score)) + 
  geom_point()+ geom_smooth(method="lm", se=FALSE) 
summary(lm(data=study3_exploratory_full, overestimation_score~ subjective_perf)) 

# I test the correlations between subjective and objective score #

ggplot(data=study3_exploratory_full, aes(x=subjective_perf, y=objective_perf)) + 
  geom_point()+ geom_smooth(method="lm", se=FALSE) +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + theme(text = element_text(size = 20))
summary(lm(data=study3_exploratory_full, objective_perf~ subjective_perf)) 
cor.test(study3_exploratory_full$subjective_perf, study3_exploratory_full$objective_perf)

# I test the correlation between objective_perf and accuracy and overstimation scores

ggplot(data=study3_exploratory_full, aes(x=objective_perf, y=accuracy_score)) + 
  geom_point()+ geom_smooth(method="lm", se=FALSE) +
  labs(x="Objective performance of the participant", y="Accuracy of the participant") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + theme(text = element_text(size = 20))
summary(lm(data=study3_exploratory_full, accuracy_score~ objective_perf)) 
cor.test(study3_exploratory_full$accuracy_score, study3_exploratory_full$objective_perf)

ggplot(data=study3_exploratory_full, aes(x=objective_perf, y=overestimation_score)) + 
  geom_point()+ geom_smooth(method="lm", se=FALSE) 
summary(lm(data=study3_exploratory_full, overestimation_score~ objective_perf)) 

# I compute overestimation_perf and test correlations with overstimation score #

study3_exploratory_full$overestimation_perf = study3_exploratory_full$subjective_perf - study3_exploratory_full$objective_perf

ggplot(data=study3_exploratory_full, aes(x=overestimation_perf, y=overestimation_score)) + 
  geom_point()+ geom_smooth(method="lm", se=FALSE) 
summary(lm(data=study3_exploratory_full, overestimation_score~ overestimation_perf)) 

# 3.3.1. Nestedness compatibility ####

# I compute the difficulty of each question according to each real participant, i.e. at the intra-individual level 

study3_all_long_difficulty <- study3_all_long %>% group_by(id, question_inferred) %>%
  dplyr::mutate(difficulty_question_inferred= 1-sum(value==1)/n())

# I add a column for the difficulty of the question that characterizes the VI

study3_all_long_unique <-study3_all_long_difficulty %>% select(id, question_inferred, difficulty_question_inferred) %>%
  unique() 

names(study3_all_long_unique)[2] = "question"
names(study3_all_long_unique)[3] = "difficulty_question_vi"

study3_all_long_full_difficulty <- full_join(study3_all_long_difficulty, study3_all_long_unique, by = c("id", "question")) %>%
  drop_na(value)

# I compute the nestedness compatibility of each response

study3_nested_compatible <- study3_all_long_full_difficulty %>%
  filter (difficulty_question_inferred <= difficulty_question_vi & value == 1 | difficulty_question_inferred > difficulty_question_vi & value == 2) %>%
  mutate(nestedness_compatibility = 1)

study3_nested_incompatible <- study3_all_long_full_difficulty %>%
  filter(difficulty_question_inferred <= difficulty_question_vi & value == 2 | difficulty_question_inferred > difficulty_question_vi & value == 1) %>%
  mutate(nestedness_compatibility = 0)

study3_all_long_nestedness <- full_join(study3_nested_compatible, study3_nested_incompatible)

#Removing irrelevant dataframes

remove(study3_all_long, study3_nested_compatible, study3_nested_incompatible, study3_all_long_difficulty, study3_all_long_full_difficulty, study3_all_long_unique)


## glmer model ##

library(lmerTest)
library(performance)
library(see)

study3_glmer_model <- glmer(data=study3_all_long_nestedness, nestedness_compatibility ~ difficulty_question_inferred + (1|id) + (1|question) + (1|question_inferred),  family = binomial(link="logit"))
summary(study3_glmer_model)

## Test Olivier

study3_all_long_nestedness <- study3_all_long_nestedness %>%  group_by(id) %>% mutate(subject.average.NC = mean(nestedness_compatibility))
all.subject.averages <- study3_all_long_nestedness[,c(1,8)]
all.subject.averages <- unique(all.subject.averages)
all.subject.averages
hist(all.subject.averages$subject.average.NC)
wilcox.test(all.subject.averages$subject.average.NC, mu = 0.5, alternative = "greater")
rm(all.subject.averages)

## Other Analysis ##

#In this section, I try to identify which questions are the more nestedness compatible ones 
#and if difficulty can predict nestedness compatibility

#Plots to explore the data

ggplot(data=study3_all_long_nestedness, aes(x=difficulty_question_inferred, y= nestedness_compatibility)) + 
  geom_point(position= "jitter") +
  labs(x="Difficulty of the inferred question", y="Nestedness compatibility")

ggplot(data=study3_all_long_nestedness, aes(x=difficulty_question_vi, y= nestedness_compatibility)) + 
  geom_point(position= "jitter") +
  labs(x="Difficulty of the question associated to the VI", y="Nestedness compatibility")

#I compute the mean nestedness compatibility and mean difficulty for each of 15 questions, averaging on all the answers given by real participants

study3_nestedness_per_question <- study3_all_long_nestedness %>% group_by(question) %>%
  summarize(avg_nestedness_per_question = mean(nestedness_compatibility), std_nestedness = sd(nestedness_compatibility), avg_difficulty_inferred = sum(value == 1)/n()) %>%
  ungroup()

#graph to visualize which questions are the most nested

study3_nestedness_per_question %>% summarize(mean(avg_nestedness_per_question))

ggplot(data=study3_nestedness_per_question, aes(x = factor(question), y = avg_nestedness_per_question)) + geom_col(fill = '#00CCCC') + 
  geom_errorbar(aes(ymin = avg_nestedness_per_question - std_nestedness, ymax = 1, width = 0.1)) + geom_hline(yintercept = 0.860, color = 'red')

#I plot nestedness VS difficulty to see how the two variables evoluate 

ggplot(data=study3_nestedness_per_question, aes(x = avg_difficulty_inferred, y = avg_nestedness_per_question)) + geom_point() + geom_text(label = study3_nestedness_per_question$question, nudge_x = 0.002, nudge_y = 0.002)

summary(lm(data = study3_nestedness_per_question, avg_nestedness_per_question ~ avg_difficulty_inferred))

# 3.3.3. Correlations between performances inter-domaines ####
ggplot(data=filter(study1_full_theme_questions), aes(x=solarsystem, y=superheroes)) + 
  geom_point() + 
  labs(x="Performance on Solar System", y="Performance on Super Heroes")
summary(lm(data=study1_full_theme_questions, superheroes~ solarsystem)) 

library("PerformanceAnalytics")

chart.Correlation(select(study1_full_theme_questions, americanhistory:superheroes), histogram=TRUE, pch=19)

# 3.3.4. Correlations between questions and performances intra-domaines ####

ggplot(data=filter(study1_full_theme_questions), aes(x=solarsystem, y=superheroes)) + 
  geom_point() + 
  labs(x="Performance on Solar System", y="Performance on Super Heroes")

library(lme4)

summary(lm(data=filter(study1_full_theme_questions, theme=="solarsystem"), superheroes ~ score*difficulty1)) 

summary(lm(data=filter(study1_full_theme_questions, theme=="americanhistory"), solarsystem ~ score+difficulty1)) 

summary(lm(data=filter(study1_full_theme_questions, theme=="superheroes"), americanhistory ~ score+difficulty1)) 








#### 3.4. TEST ####

test <- study1_full_theme_questions %>% dplyr::select(id, americanhistory,solarsystem,superheroes) %>% unique()

summary(lm(data=test, americanhistory ~ solarsystem))
summary(lm(data=test, superheroes ~ solarsystem))
summary(lm(data=test, americanhistory ~ superheroes))


#### 4. STUDY 4 ####
#### 4.1. DATA CLEANING ####

# I open raw data #

raw_data_study_4 <- read_csv('raw_study4.csv')

# I remove attention check failing and irrelevant raws #

raw_data_study_4 <- raw_data_study_4 %>%
  slice(3:nrow(raw_data_study_4)) %>%
  dplyr::select(id_prolific:question_31_40) %>%
  filter(id_prolific!="62695575c92e508d9232fd1a", id_prolific!="611eb4e2eb8483596b39d46b", id_prolific!="5d1c31bab6a81c001af88564", id_prolific!="630767b6bab20a202f64eba7", id_prolific!="636175f664f2e9a62c0450cf", id_prolific!="5dd864447beb29822b6c1e86") %>%
  dplyr::select(-check)

raw_data_study_4$id=1:nrow(raw_data_study_4)


# I select relevant columns #

raw_data_study_4 <- raw_data_study_4 %>% dplyr::select(id, assertion_7_13:question_31_40)

# I pivot longer #

study4_1 <- raw_data_study_4 %>% pivot_longer(cols=assertion_7_13:question_31_40) %>% drop_na(value)

# I change 2 by 0 : 1=yes, 0=no #

study4_1$value[study4_1$value==2]=0

# I correct a mistake from Qualtrics #

study4_1$name[study4_1$name=="question 40_33"]="question_40_33"

# I separate and rename columns, and turn them into numeric variables #
 
study4_2 <- separate(study4_1, col=name, sep="_", into=c("condition", "question_known", "question_unknown"))

names(study4_2)[5]="answer"

study4_2$question_known <- as.numeric(study4_2$question_known)
study4_2$question_unknown <- as.numeric(study4_2$question_unknown)
study4_2$answer <- as.numeric(study4_2$answer)

rm(study4_1, raw_data_study_4)

# I add themes #

study4_2$theme=ifelse(study4_2$question_known==7|study4_2$question_known==10|study4_2$question_known==13, "solarsystem", ifelse(study4_2$question_known==29|study4_2$question_known==16|study4_2$question_known==30, "americanhistory", "superheroes"))

# I add difficulty scores from study 1 #
# I have to run 1.1.2, 1.1.3, 1.1.4,  and then the whole 1.3 (1.3.0 to 1.3.2.)

rm(study1_difficulty_performance_american_history, study1_difficulty_performance_solar_system, study1_difficulty_performance_super_heroes, study1_final, study1_full_theme_questions)

study1_difficulty_forstudy4 <- study1_all %>% dplyr::select(question, difficulty1)
names(study1_difficulty_forstudy4)[1]="question_known"
names(study1_difficulty_forstudy4)[2]="question_known_difficulty"

study1_difficulty_forstudy4_2 <- study1_all %>% dplyr::select(question, difficulty1)
names(study1_difficulty_forstudy4_2)[1]="question_unknown"
names(study1_difficulty_forstudy4_2)[2]="question_unknown_difficulty"

study4_3 <- full_join(study4_2, study1_difficulty_forstudy4) %>% drop_na(id)

study4_4 <- full_join(study4_3,study1_difficulty_forstudy4_2) %>% drop_na(id)

rm(study1_all, study1_difficulty_forstudy4, study1_difficulty_forstudy4_2, study4_2, study4_3)

# I add difficulty category #

study4_4$difficulty_category_question_known=ifelse(study4_4$question_known==7|study4_4$question_known==29|study4_4$question_known==33, "easy", ifelse(study4_4$question_known==13|study4_4$question_known==16|study4_4$question_known==40, "medium", "hard"))

study4_4$difficulty_category_question_unknown=ifelse(study4_4$question_unknown==7|study4_4$question_unknown==29|study4_4$question_unknown==33, "easy", ifelse(study4_4$question_unknown==13|study4_4$question_unknown==16|study4_4$question_unknown==40, "medium", "hard"))

study4_4$condition <- as.factor(study4_4$condition)
study4_4$answer <- as.factor(study4_4$answer)
str(study4_4$answer)
str(study4_4$condition)


study4_all <- study4_4 

rm(study4_4)

str(study4_all$condition)
str(study4_all$answer)

#### 4.2. STATISTICAL ANALYSIS ####

library(lmerTest)
library(performance)
library(see)

is.factor(study4_all$condition)
is.numeric(study4_all$question_known_difficulty)
is.numeric(study4_all$question_unknown_difficulty)
is.numeric(study4_all$id)

study4_all$id <- as.factor(study4_all$id)
study4_all$theme <- as.factor(study4_all$theme)
study4_all$question_unknown <- as.factor(study4_all$question_unknown)
study4_all$question_known <- as.factor(study4_all$question_known)


null_model <- glmer(data=study4_all,family=binomial(link="logit"),  answer ~ 1 + (1|id) + (1|theme:question_known) + (1|theme:question_unknown) + question_known_difficulty + question_unknown_difficulty)
summary(null_model)

null_model_simplified <-  glmer(data=study4_all,family=binomial(link="logit"),  answer ~ 1 + (1|id) + (1|theme) + question_known_difficulty + question_unknown_difficulty)
summary(null_model_simplified)

test_model <-  glmer(data=study4_all,family=binomial(link="logit"),  answer ~ 1 + (1|id) + (1|theme:question_known) + (1|theme:question_unknown) + question_known_difficulty + question_unknown_difficulty + condition)
summary(test_model)

test_model_simplified <-  glmer(data=study4_all,family=binomial(link="logit"),  answer ~ 1 + (1|id) + (1|theme) + question_known_difficulty + question_unknown_difficulty+ condition)
summary(test_model_simplified)

test_model_2 <-  glmer(data=study4_all,family=binomial(link="logit"),  answer ~ 1 + (1|id) + (1|theme:question_known) + (1|theme:question_unknown) + (1|difficulty_category_question_known) + (1|difficulty_category_question_unknown) + condition)
summary(test_model_2)

test_model_2_simplified <-  glmer(data=study4_all,family=binomial(link="logit"),  answer ~ 1 + (1|id) + (1|theme) + (1|difficulty_category_question_known) + (1|difficulty_category_question_unknown) + condition)
summary(test_model_2_simplified)

AIC(null_model, test_model)

AIC(null_model_simplified, test_model_simplified)

# ploting #

library('jtools')

effect_plot(test_model_2_simplified, pred = condition, interval = TRUE, plot.points = FALSE)

# ploting residuals

library(arm)

plot(residuals(test_model))

help(binnedplot)
help(residuals)

binnedplot(fitted(test_model), 
           residuals(test_model_simplified, type = "response"), 
           nclass = NULL, 
           xlab = "Expected Values", 
           ylab = "Average residual", 
           main = "Binned residual plot", 
           cex.pts = 0.8, 
           col.pts = 1, 
           col.int = "gray")


is.factor(study4_all$difficulty_category_question_unknown)
study4_all$difficulty_category_question_unknown <- as.factor(study4_all$difficulty_category_question_unknown)

study4_all$difficulty_category_question_unknown <- is.factor(study4_all$difficulty_category_question_unknown)
test_model_slopes <-  glmer(data=study4_all,family=binomial(link="logit"),  answer ~ 1 + (1|id) + (1|difficulty_category_question_unknown) + difficulty_category_question_unknown*condition)
summary(test_model_slopes)

test_model_slopes_noid <-  glmer(data=study4_all,family=binomial(link="logit"),  answer ~ 1  + (1|difficulty_category_question_unknown) + difficulty_category_question_unknown*condition)
summary(test_model_slopes)

binnedplot(fitted(interaction_model), 
           residuals(interaction_model, type = "response"), 
           nclass = NULL, 
           xlab = "Expected Values", 
           ylab = "Average residual", 
           main = "Binned residual plot", 
           cex.pts = 0.8, 
           col.pts = 1, 
           col.int = "gray")

#### 4.3. REPLICATION ####
# 4.3.1. Power analysis ####

#Turning the analysis into a t test and using a generalist package

#First turning analysis into t test

#Making ure there is only one data point per participant

d_ttest <- study4_all %>% group_by(id) %>% mutate(participant.average = mean(as.numeric(answer)))

d_ttest$participant.average

d_ttest <- dplyr::select(d_ttest, c(id, condition, participant.average))

d_ttest <- unique(d_ttest)

colnames(d_ttest)

t.test(d_ttest[d_ttest$condition == "assertion",]$participant.average, d_ttest[d_ttest$condition == "question",]$participant.average, paired = FALSE)

#Effect size

effect.size <- (mean(d_ttest[d_ttest$condition == "assertion",]$participant.average) - mean(d_ttest[d_ttest$condition == "question",]$participant.average)) / sd(d_ttest$participant.average)
effect.size

#Now for the power analysis on this:

library(pwr)

dim(d_ttest)
power.analysis <- pwr.t.test(595, 0.18, 0.05, type = "two.sample", alternative="two.sided")
power.analysis

power.analysis <- pwr.t.test(d = 0.18,sig.level = 0.05, power = 0.8, type = "two.sample", alternative="two.sided")
power.analysis

rm(power.analysis, d_ttest, effect.size)

# 4.3.2. Data cleaning

# 4.3.2. Data cleaning ####

# I open raw data #

raw_data_study_4_replication <- read_csv('raw_study4_replication.csv')

# I remove attention check failing and irrelevant raws #

raw_data_study_4_replication <- raw_data_study_4_replication %>%
  slice(3:nrow(raw_data_study_4_replication))  %>% dplyr::select(id_prolific:question_43_45)

raw_data_study_4_replication$id=1:nrow(raw_data_study_4_replication)

raw_data_study4_replication_failed_attention_check <- raw_data_study_4_replication %>%
  filter(check!="I pay attention")

raw_data_study_4_replication <- raw_data_study_4_replication %>%
  filter(id_prolific!="5e8a26247ad7526bf85abbff")

rm(raw_data_study4_replication_failed_attention_check)

# I select relevant columns #

raw_data_study_4_replication <- raw_data_study_4_replication %>% dplyr::select(id, assertion_36_31:question_43_45)

# I pivot longer #

study4_1 <- raw_data_study_4_replication %>% pivot_longer(cols=assertion_36_31:question_43_45) %>% drop_na(value)

# I change 2 by 0 : 1=yes, 0=no #

study4_1$value[study4_1$value==2]=0

# I separate and rename columns, and turn them into numeric variables #

study4_2 <- separate(study4_1, col=name, sep="_", into=c("condition", "question_known", "question_unknown"))

names(study4_2)[5]="answer"

study4_2$question_known <- as.numeric(study4_2$question_known)
study4_2$question_unknown <- as.numeric(study4_2$question_unknown)
study4_2$answer <- as.numeric(study4_2$answer)

rm(study4_1, raw_data_study_4_replication)

# I add expe #

study4_2$expe=ifelse(study4_2$question_known==36|study4_2$question_known==32|study4_2$question_known==33, "easy", ifelse(study4_2$question_known==39|study4_2$question_known==44|study4_2$question_known==43, "difficult", ""))

# I add difficulty scores from study 1 #
# I have to run 1.1.2, 1.1.3, 1.1.4,  and then the whole 1.3 (1.3.0 to 1.3.2.)

study1_difficulty_forstudy4 <- study2_superheroes_full %>% dplyr::select(question, difficulty1)
names(study1_difficulty_forstudy4)[1]="question_known"
names(study1_difficulty_forstudy4)[2]="question_known_difficulty"

study1_difficulty_forstudy4_2 <- study2_superheroes_full %>% dplyr::select(question, difficulty1)
names(study1_difficulty_forstudy4_2)[1]="question_unknown"
names(study1_difficulty_forstudy4_2)[2]="question_unknown_difficulty"

study4_3 <- full_join(study4_2, study1_difficulty_forstudy4) %>% drop_na(id)

study4_4 <- full_join(study4_3,study1_difficulty_forstudy4_2) %>% drop_na(id)

rm(study1_all, study1_difficulty_forstudy4, study1_difficulty_forstudy4_2, study4_2, study4_3)

# I add difficulty category #


study4_4$condition <- as.factor(study4_4$condition)
study4_4$answer <- as.factor(study4_4$answer)
levels(study4_4$answer) <- c("0", "1")
str(study4_4$answer)
str(study4_4$condition)

study4_all_replication <- study4_4 

rm(study4_4)

# 4.3.3. Statistical analyses ####

library(lmerTest)
library(performance)
library(see)

is.factor(study4_all_replication$condition)
is.numeric(study4_all_replication$question_known_difficulty)
is.numeric(study4_all_replication$question_unknown_difficulty)
is.numeric(study4_all_replication$id)

study4_all_replication$id <- as.factor(study4_all_replication$id)
study4_all_replication$condition <- as.factor(study4_all_replication$condition)
study4_all_replication$question_unknown <- as.factor(study4_all_replication$question_unknown)
study4_all_replication$question_known <- as.factor(study4_all_replication$question_known)

## Easy study

null_model_easy <- glmer(data=filter(study4_all_replication, expe=="easy"),family=binomial(link="logit"),  answer ~ 1 + (1|id) + (1|question_known) + (1|question_unknown) +  question_unknown_difficulty)
summary(null_model_easy)

null_model_easy_2 <- glmer(data=filter(study4_all_replication, expe=="easy"),family=binomial(link="logit"),  answer ~ 1 + (1|id) + (1|question_known) + (1|question_unknown) + question_known_difficulty + question_unknown_difficulty)
summary(null_model_easy_2)

AIC(null_model_easy, null_model_easy_2)


# We drop question_known_difficulty as planned and keep null_model_easy
rm(null_model_easy_2)

test_model_easy <- glmer(data=filter(study4_all_replication, expe=="easy"),family=binomial(link="logit"),  answer ~ 1 + (1|id) + (1|question_known) + (1|question_unknown) + question_unknown_difficulty + condition)
summary(test_model_easy)

AIC(null_model_easy, test_model_easy)

## Difficult study

null_model_difficult <- glmer(data=filter(study4_all_replication, expe=="difficult"),family=binomial(link="logit"),  answer ~ 1 + (1|id) + (1|question_known) + (1|question_unknown) + question_unknown_difficulty)
summary(null_model_difficult)

null_model_difficult_2 <- glmer(data=filter(study4_all_replication, expe=="difficult"),family=binomial(link="logit"),  answer ~ 1 + (1|id) + (1|question_known) + (1|question_unknown) + question_known_difficulty + question_unknown_difficulty)
summary(null_model_difficult_2)

AIC(null_model_difficult, null_model_difficult_2)

# We drop question_known_difficulty as planned and keep null_model_easy
rm(null_model_difficult_2)

test_model_difficult <- glmer(data=filter(study4_all_replication, expe=="difficult"),family=binomial(link="logit"),  answer ~ 1 + (1|id) + (1|question_known) + (1|question_unknown) + question_unknown_difficulty + condition)
summary(test_model_difficult)

AIC(null_model_difficult, test_model_difficult)

# ploting #

library('jtools')

effect_plot(test_model_easy, pred = condition, interval = TRUE, plot.points = FALSE)
effect_plot(test_model_difficult, pred = condition, interval = TRUE, plot.points = FALSE)


# ploting residuals

library(arm)

plot(residuals(test_model_easy))
plot(residuals(test_model_difficult))

library('arm')

binnedplot(fitted(test_model_easy), 
           residuals(test_model_easy, type = "response"), 
           nclass = NULL, 
           xlab = "Expected Values", 
           ylab = "Average residual", 
           main = "Binned residual plot", 
           cex.pts = 0.8, 
           col.pts = 1, 
           col.int = "gray")

binnedplot(fitted(test_model_difficult), 
           residuals(test_model_difficult, type = "response"), 
           nclass = NULL, 
           xlab = "Expected Values", 
           ylab = "Average residual", 
           main = "Binned residual plot", 
           cex.pts = 0.8, 
           col.pts = 1, 
           col.int = "gray")


