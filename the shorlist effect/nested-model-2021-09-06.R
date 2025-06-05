#Cultural selection positively impacts cultural nestedness: a model

#Olivier Morin

#Accompanying the paper:
#The Shortlist Effect: Contributions to nestedness can reveal cultural selection
#by Olivier Morin & Oleg Sobchuk

#This script provides a simulation of cultural nestedness under various conditions 
#involving various degrees of cultural selection.

#Some of the results of this simulation are presented in Fig. 2 and Table 1 of the main paper.

#Table of contents #####

#1. The adoption function
#describes the core of the model, a function to predict the adoption of items in collections of varying sizes.

#2. Content-based selection increases nestedness: the intuitive argument
#A toy model to illustrate our point intuitively.

#2.1. The Shortlist Effect: the adoption function when collection sizes and item appeals change 
#2.2. Content-based selection and the shortlist effect
#2.3. How content-based selection can increase nestedness in an intuitive sense, due to the shortlist effect 

#3. Content-based selection increases nestedness: the simulations
#A set of simulations showing that nestedness, measured as NODF, is increased by content-based selection
#when the shortlist effect is at play.

#3.1. Setting the parameters 
#3.2. Generating sets of collections
#3.4. Building a series of simulations
#3.5. Results: Correlation between Nestedness and content-based selection 


#0. Packages 

library("groundhog")
groundhog.day="2021-09-06"
groundhog.library(vegan,groundhog.day, tolerate.R.version='3.6.0')
groundhog.library(fabricatr, groundhog.day,  tolerate.R.version='3.6.0')
groundhog.library(ppcor, groundhog.day, tolerate.R.version='3.6.0')
groundhog.library(gplots, groundhog.day, tolerate.R.version='3.6.0')
groundhog.library(wesanderson, groundhog.day, tolerate.R.version='3.6.0')


#1. The adoption function ######

#To learn more about the adoption function please see the main paper.
#The adoption function models the adoption of an item into a collection, as a function of
#3 parameters: 
#collection capacity: How much room a collection has for adopting items;
#item prevalence: How prevalent an item is: how likely it is to be encountered;
#item appeal: How likely an item is likely to be adopted by small collections
#Each of these three parameters is represented by a figure between 0 and 1.

#The function includes two constants:
#e modulates matrix fill, the basic probability that collections adopt items;
#s modulates the shortlist effect, the extent to which appealing items are favoured in small collections.

#A simple, non-normalised version of the adoption function returns values that are not bounded
adoption.function <- function(capacity, prevalence, appeal){
  e*capacity + e*prevalence - (s*(1-appeal)*(1-capacity)) }

#This normalised version returns an adoption probability, comprised between 0 and 1
#for any positive k and s and for any capacity, prevalence, appeal comprised between 0 and 1
adoption.function <- function(capacity, prevalence, appeal){  
  (e*capacity + e*prevalence - (s*(1-appeal)*(1-capacity)) + s) / (a*2 + s)}


#2. Content-based selection increases nestedness: the intuitive argument #######

#For this section we set the constant e and s at an arbitrary value of 1. 
#Later, we'll use simulations to show that our intuitive result obtains
#for a borad range of values of e and s

e = 1
s = 1

#2.1. The Shortlist Effect: the adoption function when collection sizes and item appeals change #####

#Here we show how the shortlist effect works: 
#We assume a population of items with identical prevalences (prevalence = 1)
fixed.prevalence = 0.5
#A set of 10 collections with capacity ranging from 0.1 to 1
collections.set <- seq(0.1,1,0.1)
#And varying appeals. The following graph shows how 
#high-appeal items are favoured over low-appeal items, to the extent that collection capacity is small:

d <- as.data.frame(cbind(
  collections.set,
  adoption.function(collections.set,fixed.prevalence,0.1),
  adoption.function(collections.set,fixed.prevalence,0.2),
  adoption.function(collections.set,fixed.prevalence,0.3),
  adoption.function(collections.set,fixed.prevalence,0.4),
  adoption.function(collections.set,fixed.prevalence,0.5),
  adoption.function(collections.set,fixed.prevalence,0.6),
  adoption.function(collections.set,fixed.prevalence,0.7),
  adoption.function(collections.set,fixed.prevalence,0.8),
  adoption.function(collections.set,fixed.prevalence,0.9)
))
d
ggplot(d, aes(x=collections.set)) +   geom_line(aes(y = V2), color = "blue") + 
  geom_line(aes(y = V3), color="black") +
  geom_line(aes(y = V4), color="black") +
  geom_line(aes(y = V5), color="black") +
  geom_line(aes(y = V6), color="black") +
  geom_line(aes(y = V7), color="black") +
  geom_line(aes(y = V8), color="black") +
  geom_line(aes(y = V9), color="black") +
  geom_line(aes(y = V10), color="red") +  
  scale_x_continuous(name="collection capacity kc", limits=c(0, 1)) +
  scale_y_continuous(name="adoption function P", limits=c(0, 1))

#Compare the red line (high-appeal item) with the blue line (low-appeal item):
#The adoption probability is higher overall for the red item, and also less dependent on collection size.

#We illustrate this with a heatmap plot, showing the probability of item adoption as a function 
#of collection capacity and item prevalence 
#(x axis: items from lowest to highest prevalence; y axis: collections from lowest to highest capacity)

#Building a matrix of probabilities for the same items and collections:
capacities.vector <- seq(1.00,0.00,-0.1)
items.vector <- as.vector(seq(1.00,0.00,-0.1))
probabilities.matrix.selection <- matrix(NA, nrow = length(capacities.vector), ncol = length(items.vector))
probabilities.matrix.selection
for (i in 1:length(capacities.vector)){
  for (j in 1:length(items.vector)){
    probabilities.matrix.selection[i,j] <- adoption.function(capacity = capacities.vector[i], prevalence =fixed.prevalence, appeal= items.vector[j])
  }
}
#the heatmap:
pal <- wes_palette("Zissou1", 21, type = "continuous")
heatmap.2(probabilities.matrix.selection,dendrogram = "none", Colv  = FALSE, Rowv = FALSE,trace ="none",  col=pal,breaks=seq(0,1,0.04545455))

#This heatmap is the top map in Fig. 2 of our paper.

#2.2. Content-based selection and the shortlist effect #####

#This section considers what happens when appeal and prevalence are perfectly correlated:
#High-appeal items are highly prevalent,  low-appeal items are not prevalent.
#In other words we have perfect content-based selection.

##As before we have a set of 10 collections with capacity ranging from 0.1 to 1 (collections.set)
#And varying appeals. The following graph shows how 
#high-appeal items are favoured over low-appeal items, to the extent that collection capacity is small:

d.prime <- as.data.frame(cbind(
  collections.set,
  adoption.function(collections.set,0.1,0.1),
  adoption.function(collections.set,0.2,0.2),
  adoption.function(collections.set,0.3, 0.3),
  adoption.function(collections.set,0.4,0.4),
  adoption.function(collections.set,0.5, 0.5),
  adoption.function(collections.set,0.6, 0.6),
  adoption.function(collections.set,0.7, 0.7),
  adoption.function(collections.set,0.8, 0.8),
  adoption.function(collections.set,0.9, 0.9)
))

ggplot(d.prime, aes(x=collections.set)) +   geom_line(aes(y = V2), color = "blue") + 
  geom_line(aes(y = V3), color="black") +
  geom_line(aes(y = V4), color="black") +
  geom_line(aes(y = V5), color="black") +
  geom_line(aes(y = V6), color="black") +
  geom_line(aes(y = V7), color="black") +
  geom_line(aes(y = V8), color="black") +
  geom_line(aes(y = V9), color="black") +
  geom_line(aes(y = V10), color="red") +  
  scale_x_continuous(name="collection capacity", limits=c(0, 1)) +
  scale_y_continuous(name="adoption probability", limits=c(0, 1))

#Under content based selection, the shortlist effect still obtains:
#items that have high appeal and high prevalence (like the red item, compared to the blue one)
#are both more likely to be adopted, and less sensitive to fluctuations in collection capacity.
#Compared to the previous simulation, where there was no content-based selection
#the blue item is also much less likely to be adopted, overall.

#We illustrate this with a heatmap plot, showing the probability of item adoption as a function 
#of collection capacity and item prevalence, 
capacities.vector <- seq(1.00,0.00,-0.1)
items.vector <- as.vector(seq(1.00,0.00,-0.1))

probabilities.matrix.selection <- matrix(NA, nrow = length(capacities.vector), ncol = length(items.vector))
probabilities.matrix.selection
for (i in 1:length(capacities.vector)){
  for (j in 1:length(items.vector)){
    probabilities.matrix.selection[i,j] <- adoption.function(capacity = capacities.vector[i], prevalence =items.vector[j], appeal= items.vector[j])
  }
}
pal <- wes_palette("Zissou1", 21, type = "continuous")
heatmap.2(probabilities.matrix.selection,dendrogram = "none", Colv  = FALSE, Rowv = FALSE,trace ="none",  col=pal,breaks=seq(0,1,0.04545455))

#This heatmap is the bottom map in Fig. 2 of our paper.

#2.3. How content-based selection can increase nestedness in an intuitive sense, due to the shortlist effect ######

#INtuitively, a set of collections is perfectly nested if all the items that occur in small collections
#also occur in large collections. It is not nested if many items occur in small-size collections but not in big ones.
##Let us say an item is "unnested" if it occurs in a collection of capacity 0, but *not* in a collection of capacity 1.

#The probability of occurring in a collection of size 1 is the same regardless of appeal.
#It only depends on an item's prevalence.
#To show this, we take the adoption function:
adoption.function <- function(capacity, prevalence, appeal){  
  (e*capacity + e*prevalence - (s*(1-appeal)*(1-capacity)) + s) / (a*2 + s)}
#By setting capacity to 1, we get:
adoption.function.k1<- function(prevalence, appeal){  
  (e + e*prevalence - (s*(1-appeal)*(1-1)) + s) / (e*2 + s)}
#Which simplifies to:
adoption.function.k1<- function(prevalence){  
  (e + e*prevalence + s) / (e*2 + s)}

#In contrast, the probability of occurring in a collection of size 0 depends on prevalence *and* appeal.
#To show this, we take the adoption function:
adoption.function <- function(capacity, prevalence, appeal){  
  (e*capacity + e*prevalence - (s*(1-appeal)*(1-capacity)) + s) / (e*2 + s)}
#By setting capacity to 0, we get:
adoption.function.k0 <- function(prevalence, appeal){
  (e*prevalence - s*(1-appeal) + s) / (e*2 + s)}

#Now we can compute the probability for an item to be "unnested":
#To be present in a collection of size 0 but *not* in a collection of size 1.
unnested.function <- function(prevalence, appeal){
  adoption.function.k0(prevalence, appeal) * (1-(adoption.function.k1(prevalence)))}

#When prevalence and appeal are both high, the probability of being unnested is very low
#(because the item either appears nowhere or appears everywhere)
unnested.function(prevalence = 0.1, appeal = 0.1)
unnested.function(prevalence = 0.9,appeal = 0.9)
#When prevalence and appeal are moderate, the probability of being unnested is stronger:
unnested.function(prevalence = 0.5, appeal = 0.5)
#But it is highest when an item has low prevalence but high appeal: 
#These "hidden gems" are likely to occur in small collections (thanks to the shortlist effect)
#But not much more likely to occur in bigger collections:
unnested.function(prevalence = 0.1, appeal = 0.9)
#Such "hidden gems" cannot occur if content-based selection is strong. 
#By definition, it makes sure that high-appeal items also have high prevalence.

#We can show this by simulating a series of items whose appeal is *not* correlated with their prevalence. 
#Their prevalence varies but their appeal is always 1.
x <- seq(0,1,0.1)
unselected.items  <- as.data.frame(cbind(x,1))
unselected.items$unnesting.prob <- unnested.function(prevalence=x,appeal=1)
colnames(unselected.items) <- c("item prevalence", "item appeal", "probability of being unnested")
unselected.items
#On average, these items' probability of being unnested is 7.2 % 
mean(unselected.items$'probability of being unnested')

#Now we take a series of items whose appeal is perfectly correlated with (equal to) their prevalence. 
x <- seq(0,1,0.1)
selected.items  <- as.data.frame(cbind(x,x))
selected.items$unnesting.prob <- unnested.function(prevalence=x,appeal=x)
colnames(selected.items) <- c("item prevalence", "item appeal", "probability of being unnested")
selected.items
#On average, these items' probability of being unnested is twice lower: 3.33% 
mean(selected.items$'probability of being unnested')

#We now generalise this result to a broad range of values of a and s

e.values <- c(0.001,0.25,0.5,0.75,1)
s.values <- c(0.001,0.25,0.5,0.75,1)
for (i in 1:length(e.values)) {
  for (j in 1:length(s.values)) {
    print("value of e:")
    print(a.values[i])
    print("value of s:")
    print(s.values[j])
    e = e.values[i]
    s = s.values[j]
#    presence.function.k1 <- function(prevalence){  
#      (e + e*prevalence +s) / (e*2+s)}
#    presence.function.k0 <- function(prevalence, appeal){
#      (e*prevalence - s*(1-appeal) + s) / (e*2 + s)}
 #   unnested.function <- function(prevalence, appeal){
#    presence.function.k0(prevalence, appeal) * (1-(presence.function.k1(prevalence)))}
    x <- seq(0,1,0.1)
    y <- seq(0,1,0.1)
    decor.items <- as.data.frame(cbind(x,1))
    decor.items$unnesting.prob <- unnested.function(prevalence=x,appeal=1)
    mean(decor.items$unnesting.prob)
    cor.items <- as.data.frame(cbind(x,y))
    cor.items$unnesting.prob <- unnested.function(prevalence=x,appeal=y)
    mean(cor.items$unnesting.prob)
    print("Difference in the items' average probability of being unnested:")
    print(mean(decor.items$unnesting.prob) - mean(cor.items$unnesting.prob))
  }
}

#The difference in the items' average probability of being unnested is always positive:
#Unnesting is more likely when appeal and prevalence are not correlated,
#less likely when they are.
#The difference, however, is extremely small when the s parameter is much smaller than e,
#i.e., when the shortlist effect is negligible.

#The next section shows this more systematically, using the standard NODF measure for nestedness.

#3. Content-based selection increases nestedness: the simulations #####

#This simulation is the one described in the main paper, in particular in the Methods section.

#3.1. Setting the parameters #######

#The size of our square matrix. It determines the maximal size that a collection can be, and also the number of items
n = 200
#And the number of simulations we'll be running
n.simulations <- 30

#(For a smaller running time, these figures can be reduced.)

#3.2. Generating sets of collections ####

#Each collection is defined by a carrying capacity, drawn from a continuous distribution
capacities.sequence <- seq(0,1,0.005)
set.seed(42)
collections.carrying.capacities <- sample(capacities.sequence,n,replace=TRUE)
hist(collections.carrying.capacities)

#3.3. The set of items ####

#Each item has an appeal value and a prevalence value.
#Each of these two variables follows a continuous distribution. 
#The correlation between the two variables, called "selection.parameter', varies.

#First I generate a prevalence value for each of the n items. Following the empirical distribution
#the distribution is continuous rather than normal. 
#(A truncated power-law distribution would be closer to the empirical data but would raise problems when generating the
#correlated appeals distributions.)

frequencies.sequence <- seq(0,1,0.005)
set.seed(43)
item.frequencies <- sample(frequencies.sequence,n,replace=TRUE)
hist(item.frequencies)

#Next, we generate a distribution of appeal values. 
#An item's appeal may depend on its prevalence value, in different ways. 

#This creates a set of appeal values that is uncorrelated from the items' frequencies
# Generate a correlated variable using fabricatr variable generation
set.seed(123)
item.appealsCor0 <- correlate(given = item.frequencies, rho = 0.0001,rnorm,
                                         draw_count, mean = 0.5, sd = 0.15)

correlation.0 <- cor(item.frequencies,item.appealsCor0, method = "spearman")
correlation.0
plot(item.frequencies,item.appealsCor0)
mean(item.appealsCor0)

#This creates a set of appeal values that is moderately correlated to the items' frequencies
set.seed(123)
item.appealsCor25 <-  correlate(given = item.frequencies, rho = 0.25,rnorm,
                                         draw_count, mean = 0.5, sd = 0.15)
correlation.25 <- cor(item.frequencies,item.appealsCor25, method = "spearman")
correlation.25
plot(item.frequencies,item.appealsCor25)
mean(item.appealsCor25)

#Correlation: .50
set.seed(123)
item.appealsCor50 <-  correlate(given = item.frequencies, rho = 0.5,rnorm,
                                  draw_count, mean = 0.5, sd = 0.15)
correlation.50 <- cor(item.frequencies,item.appealsCor50, method = "spearman")
correlation.50
plot(item.frequencies,item.appealsCor50)
mean(item.appealsCor50)

#Correlation: 75
set.seed(123)
item.appealsCor75 <-  correlate(given = item.frequencies, rho = 0.75,rnorm,
                                   draw_count, mean = 0.5, sd = 0.15)
correlation.75 <- cor(item.frequencies,item.appealsCor75, method = "spearman")
correlation.75
plot(item.frequencies,item.appealsCor75)
mean(item.appealsCor75)

#Correlation: 1
set.seed(123)
item.appealsCor1 <-  correlate(given = item.frequencies, rho = 1,rnorm,
                                draw_count, mean = 0.5, sd = 0.15)
correlation.1<- cor(item.frequencies,item.appealsCor1, method = "spearman")
correlation.1
plot(item.frequencies,item.appealsCor1)
mean(item.appealsCor1)

#3.4. Building a series of simulations ######

#Building the dataframe where the results will be stored
correlation.values <- c(0,0.25,0.50,0.75,1)
s.values  <- c(0.0000001, 0.001,0.01, 0.1,0.25,0.5, 0.75,1) 
e.values <- c(0.0000001,0.001,0.01, 0.1,0.25,0.5, 0.75,1)
results.table <- data.frame(matrix(NA, nrow = (length(correlation.values)*length(e.values)*length(a.values)), ncol = 8))
colnames(results.table) <- c("correlation.value","s.value", "e.value", "average.NODF", "min.NODF", "max.NODF","average.fill","nestedness.significance")
head(results.table)

draw.vector <- seq(0,1,0.005)

counter <- 0

for (i in 1:length(correlation.values)){
  # print(i)
  for(j in 1:length(s.values)){
    # print(j)
      for(l in 1:length(e.values)) {
      counter <- counter +1
      print(counter)
      fill.vector <- c()
      nodf.vector <- c()
      nestedness.significance <- c()
      correlation.values[i]
      #print(frequency.appeal.correlation)  #all good
      s <- s.values[j]
      e <- e.values[l]
      
      adoption.function <- function(capacity, frequency, appeal){  
        ( e*capacity + e*frequency - (s*(1-appeal)*(1-capacity))+s) /(e*2+s)}
      
      #Determining the distribution of appeal values   
      
      if(correlation.values[i] == 0.00) { items.appeal <- item.appealsCor0  }
      if(correlation.values[i] == 0.25) {  items.appeal <-item.appealsCor25 }
      if(correlation.values[i] == 0.50) { items.appeal <- item.appealsCor50 }
      if(correlation.values[i] == 0.75) { items.appeal <- item.appealsCor75 }
      if(correlation.values[i] == 1.00) { items.appeal <- item.appealsCor1 }
      
      for (z in 1:n.simulations) {
        m <- data.frame(matrix(NA, nrow = n, ncol = n))
        for (a in 1:n){
          for (b in 1:n){
            draw <-  sample(draw.vector,1, replace = TRUE)
            item.presence.probability <-  adoption.function(capacity = collections.carrying.capacities[a],frequency =item.frequencies[b], appeal=items.appeal[b])
            m[a,b] <- as.numeric(draw < item.presence.probability)
                   }
        }
        
        #Outputting NODF & fill  
        nodf.m <- nestednodf(m, order=T, weighted=FALSE, wbinary=FALSE)
        #   print(nodf.m)
        nodf.vector[z] <- as.numeric(nodf.m$statistic[3])
        fill.vector[z] <- as.numeric(nodf.m$fill)
        
        #Outputting significance,  for the first simulated matrix of the series only
        if(z ==1){
          #Significance test for nestedness
          nodf.sig <-  oecosimu(m, nestfun=nestednodf, method="r1", nsimul=200, alt="greater")
          nestedness.significance  <-  nodf.sig$oecosimu$pval[1]  
          
        }
              }
    
      results.table[counter,]$correlation.value <- correlation.values[i]
      results.table[counter,]$s.value <- s
      results.table[counter,]$e.value <-  e
      results.table[counter,]$average.NODF <-  mean(nodf.vector)
      results.table[counter,]$min.NODF <-  min(nodf.vector)
      results.table[counter,]$max.NODF <-  max(nodf.vector)
      results.table[counter,]$average.fill  <- mean(fill.vector)
      results.table[counter,]$nestedness.significance <- nestedness.significance
      }
       }
  }


write.csv(results.table, "simulation.csv")
head(results.table)


####3.5. Results: Correlation between Nestedness and content-based selection ########

# When s = 0.0001... #####
s.00000001.table <-  results.table[results.table$s.value == 0.0000001,]
s.00000001.table
plot(s.00000001.table[results.table$e.value == 0.0000001,]$correlation.value, s.00000001.table[results.table$e.value == 0.0000001,]$average.NODF)
pcor.test(s.00000001.table[s.00000001.table$e.value == 0.0000001,]$correlation.value, s.00000001.table[s.00000001.table$e.value == 0.0000001,]$average.NODF, s.00000001.table[s.00000001.table$e.value == 0.0000001,]$average.fill)
cor(s.00000001.table[s.00000001.table$e.value == 0.0000001,]$correlation.value, s.00000001.table[s.00000001.table$e.value == 0.0000001,]$average.NODF, method = "pearson")

plot(s.00000001.table[results.table$e.value == 0.001,]$correlation.value, s.00000001.table[results.table$e.value == 0.001,]$average.NODF)
pcor.test(s.00000001.table[s.00000001.table$e.value == 0.001,]$correlation.value, s.00000001.table[s.00000001.table$e.value == 0.001,]$average.NODF, s.00000001.table[s.00000001.table$e.value == 0.001,]$average.fill)
cor(s.00000001.table[s.00000001.table$e.value == 0.001,]$correlation.value, s.00000001.table[s.00000001.table$e.value == 0.001,]$average.NODF, method = "pearson")

plot(s.00000001.table[results.table$e.value == 0.01,]$correlation.value, s.00000001.table[results.table$e.value == 0.01,]$average.NODF)
pcor.test(s.00000001.table[s.00000001.table$e.value == 0.01,]$correlation.value, s.00000001.table[s.00000001.table$e.value == 0.01,]$average.NODF, s.00000001.table[s.00000001.table$e.value == 0.01,]$average.fill)
cor(s.00000001.table[s.00000001.table$e.value == 0.01,]$correlation.value, s.00000001.table[s.00000001.table$e.value == 0.01,]$average.NODF, method = "pearson")

plot(s.00000001.table[results.table$e.value == 0.1,]$correlation.value, s.00000001.table[results.table$e.value == 0.1,]$average.NODF)
pcor.test(s.00000001.table[s.00000001.table$e.value == 0.1,]$correlation.value, s.00000001.table[s.00000001.table$e.value == 0.1,]$average.NODF, s.00000001.table[s.00000001.table$e.value == 0.1,]$average.fill)
cor(s.00000001.table[s.00000001.table$e.value == 0.1,]$correlation.value, s.00000001.table[s.00000001.table$e.value == 0.1,]$average.NODF, method = "pearson")

plot(s.00000001.table[results.table$e.value == 0.25,]$correlation.value, s.00000001.table[results.table$e.value == 0.25,]$average.NODF)
pcor.test(s.00000001.table[s.00000001.table$e.value == 0.25,]$correlation.value, s.00000001.table[s.00000001.table$e.value == 0.25,]$average.NODF, s.00000001.table[s.00000001.table$e.value == 0.25,]$average.fill)
cor(s.00000001.table[s.00000001.table$e.value == 0.25,]$correlation.value, s.00000001.table[s.00000001.table$e.value == 0.25,]$average.NODF, method = "pearson")

plot(s.00000001.table[results.table$e.value == 0.5,]$correlation.value, s.00000001.table[results.table$e.value == 0.5,]$average.NODF)
pcor.test(s.00000001.table[s.00000001.table$e.value == 0.5,]$correlation.value, s.00000001.table[s.00000001.table$e.value == 0.5,]$average.NODF, s.00000001.table[s.00000001.table$e.value == 0.5,]$average.fill)
cor(s.00000001.table[s.00000001.table$e.value == 0.5,]$correlation.value, s.00000001.table[s.00000001.table$e.value == 0.5,]$average.NODF, method = "pearson")

plot(s.00000001.table[results.table$e.value == 0.75,]$correlation.value, s.00000001.table[results.table$e.value == 0.75,]$average.NODF)
pcor.test(s.00000001.table[s.00000001.table$e.value == 0.75,]$correlation.value, s.00000001.table[s.00000001.table$e.value == 0.75,]$average.NODF, s.00000001.table[s.00000001.table$e.value == 0.75,]$average.fill)
cor(s.00000001.table[s.00000001.table$e.value == 0.75,]$correlation.value, s.00000001.table[s.00000001.table$e.value == 0.75,]$average.NODF, method = "pearson")

plot(s.00000001.table[results.table$e.value == 1,]$correlation.value, s.00000001.table[results.table$e.value == 1,]$average.NODF)
pcor.test(s.00000001.table[s.00000001.table$e.value == 1,]$correlation.value, s.00000001.table[s.00000001.table$e.value == 1,]$average.NODF, s.00000001.table[s.00000001.table$e.value == 1,]$average.fill)
cor(s.00000001.table[s.00000001.table$e.value == 1,]$correlation.value, s.00000001.table[s.00000001.table$e.value == 1,]$average.NODF, method = "pearson")

# When s = 0.001... #####
s.0001.table <-  results.table[results.table$s.value == 0.001,]
s.0001.table
plot(s.0001.table[results.table$e.value == 0.0000001,]$correlation.value, s.0001.table[results.table$e.value == 0.0000001,]$average.NODF)
pcor.test(s.0001.table[s.0001.table$e.value == 0.0000001,]$correlation.value, s.0001.table[s.0001.table$e.value == 0.0000001,]$average.NODF, s.0001.table[s.0001.table$e.value == 0.0000001,]$average.fill)
cor(s.0001.table[s.0001.table$e.value == 0.0000001,]$correlation.value, s.0001.table[s.0001.table$e.value == 0.0000001,]$average.NODF, method = "pearson")

plot(s.0001.table[results.table$e.value == 0.001,]$correlation.value, s.0001.table[results.table$e.value == 0.001,]$average.NODF)
pcor.test(s.0001.table[s.0001.table$e.value == 0.001,]$correlation.value, s.0001.table[s.0001.table$e.value == 0.001,]$average.NODF, s.0001.table[s.0001.table$e.value == 0.001,]$average.fill)
cor(s.0001.table[s.0001.table$e.value == 0.001,]$correlation.value, s.0001.table[s.0001.table$e.value == 0.001,]$average.NODF, method = "pearson")

plot(s.0001.table[results.table$e.value == 0.01,]$correlation.value, s.0001.table[results.table$e.value == 0.01,]$average.NODF)
pcor.test(s.0001.table[s.0001.table$e.value == 0.01,]$correlation.value, s.0001.table[s.0001.table$e.value == 0.01,]$average.NODF, s.0001.table[s.0001.table$e.value == 0.01,]$average.fill)
cor(s.0001.table[s.0001.table$e.value == 0.01,]$correlation.value, s.0001.table[s.0001.table$e.value == 0.01,]$average.NODF, method = "pearson")

plot(s.0001.table[results.table$e.value == 0.1,]$correlation.value, s.0001.table[results.table$e.value == 0.1,]$average.NODF)
pcor.test(s.0001.table[s.0001.table$e.value == 0.1,]$correlation.value, s.0001.table[s.0001.table$e.value == 0.1,]$average.NODF, s.0001.table[s.0001.table$e.value == 0.1,]$average.fill)
cor(s.0001.table[s.0001.table$e.value == 0.1,]$correlation.value, s.0001.table[s.0001.table$e.value == 0.1,]$average.NODF, method = "pearson")

plot(s.0001.table[results.table$e.value == 0.25,]$correlation.value, s.0001.table[results.table$e.value == 0.25,]$average.NODF)
pcor.test(s.0001.table[s.0001.table$e.value == 0.25,]$correlation.value, s.0001.table[s.0001.table$e.value == 0.25,]$average.NODF, s.0001.table[s.0001.table$e.value == 0.25,]$average.fill)
cor(s.0001.table[s.0001.table$e.value == 0.25,]$correlation.value, s.0001.table[s.0001.table$e.value == 0.25,]$average.NODF, method = "pearson")

plot(s.0001.table[results.table$e.value == 0.5,]$correlation.value, s.0001.table[results.table$e.value == 0.5,]$average.NODF)
pcor.test(s.0001.table[s.0001.table$e.value == 0.5,]$correlation.value, s.0001.table[s.0001.table$e.value == 0.5,]$average.NODF, s.0001.table[s.0001.table$e.value == 0.5,]$average.fill)
cor(s.0001.table[s.0001.table$e.value == 0.5,]$correlation.value, s.0001.table[s.0001.table$e.value == 0.5,]$average.NODF, method = "pearson")

plot(s.0001.table[results.table$e.value == 0.75,]$correlation.value, s.0001.table[results.table$e.value == 0.75,]$average.NODF)
pcor.test(s.0001.table[s.0001.table$e.value == 0.75,]$correlation.value, s.0001.table[s.0001.table$e.value == 0.75,]$average.NODF, s.0001.table[s.0001.table$e.value == 0.75,]$average.fill)
cor(s.0001.table[s.0001.table$e.value == 0.75,]$correlation.value, s.0001.table[s.0001.table$e.value == 0.75,]$average.NODF, method = "pearson")

plot(s.0001.table[results.table$e.value == 1,]$correlation.value, s.0001.table[results.table$e.value == 1,]$average.NODF)
pcor.test(s.0001.table[s.0001.table$e.value == 1,]$correlation.value, s.0001.table[s.0001.table$e.value == 1,]$average.NODF, s.0001.table[s.0001.table$e.value == 1,]$average.fill)
cor(s.0001.table[s.0001.table$e.value == 1,]$correlation.value, s.0001.table[s.0001.table$e.value == 1,]$average.NODF, method = "pearson")

# When s = 0.01... #####
s.001.table <-  results.table[results.table$s.value == 0.01,]
s.001.table
plot(s.001.table[results.table$e.value == 0.0000001,]$correlation.value, s.001.table[results.table$e.value == 0.0000001,]$average.NODF)
pcor.test(s.001.table[s.001.table$e.value == 0.0000001,]$correlation.value, s.001.table[s.001.table$e.value == 0.0000001,]$average.NODF, s.001.table[s.001.table$e.value == 0.0000001,]$average.fill)
cor(s.001.table[s.001.table$e.value == 0.0000001,]$correlation.value, s.001.table[s.001.table$e.value == 0.0000001,]$average.NODF, method = "pearson")

plot(s.001.table[results.table$e.value == 0.001,]$correlation.value, s.001.table[results.table$e.value == 0.001,]$average.NODF)
pcor.test(s.001.table[s.001.table$e.value == 0.001,]$correlation.value, s.001.table[s.001.table$e.value == 0.001,]$average.NODF, s.001.table[s.001.table$e.value == 0.001,]$average.fill)
cor(s.001.table[s.001.table$e.value == 0.001,]$correlation.value, s.001.table[s.001.table$e.value == 0.001,]$average.NODF, method = "pearson")

plot(s.001.table[results.table$e.value == 0.01,]$correlation.value, s.001.table[results.table$e.value == 0.01,]$average.NODF)
pcor.test(s.001.table[s.001.table$e.value == 0.01,]$correlation.value, s.001.table[s.001.table$e.value == 0.01,]$average.NODF, s.001.table[s.001.table$e.value == 0.01,]$average.fill)
cor(s.001.table[s.001.table$e.value == 0.01,]$correlation.value, s.001.table[s.001.table$e.value == 0.01,]$average.NODF, method = "pearson")

plot(s.001.table[results.table$e.value == 0.1,]$correlation.value, s.001.table[results.table$e.value == 0.1,]$average.NODF)
pcor.test(s.001.table[s.001.table$e.value == 0.1,]$correlation.value, s.001.table[s.001.table$e.value == 0.1,]$average.NODF, s.001.table[s.001.table$e.value == 0.1,]$average.fill)
cor(s.001.table[s.001.table$e.value == 0.1,]$correlation.value, s.001.table[s.001.table$e.value == 0.1,]$average.NODF, method = "pearson")

plot(s.001.table[results.table$e.value == 0.25,]$correlation.value, s.001.table[results.table$e.value == 0.25,]$average.NODF)
pcor.test(s.001.table[s.001.table$e.value == 0.25,]$correlation.value, s.001.table[s.001.table$e.value == 0.25,]$average.NODF, s.001.table[s.001.table$e.value == 0.25,]$average.fill)
cor(s.001.table[s.001.table$e.value == 0.25,]$correlation.value, s.001.table[s.001.table$e.value == 0.25,]$average.NODF, method = "pearson")

plot(s.001.table[results.table$e.value == 0.5,]$correlation.value, s.001.table[results.table$e.value == 0.5,]$average.NODF)
pcor.test(s.001.table[s.001.table$e.value == 0.5,]$correlation.value, s.001.table[s.001.table$e.value == 0.5,]$average.NODF, s.001.table[s.001.table$e.value == 0.5,]$average.fill)
cor(s.001.table[s.001.table$e.value == 0.5,]$correlation.value, s.001.table[s.001.table$e.value == 0.5,]$average.NODF, method = "pearson")

plot(s.001.table[results.table$e.value == 0.75,]$correlation.value, s.001.table[results.table$e.value == 0.75,]$average.NODF)
pcor.test(s.001.table[s.001.table$e.value == 0.75,]$correlation.value, s.001.table[s.001.table$e.value == 0.75,]$average.NODF, s.001.table[s.001.table$e.value == 0.75,]$average.fill)
cor(s.001.table[s.001.table$e.value == 0.75,]$correlation.value, s.001.table[s.001.table$e.value == 0.75,]$average.NODF, method = "pearson")

plot(s.001.table[results.table$e.value == 1,]$correlation.value, s.001.table[results.table$e.value == 1,]$average.NODF)
pcor.test(s.001.table[s.001.table$e.value == 1,]$correlation.value, s.001.table[s.001.table$e.value == 1,]$average.NODF, s.001.table[s.001.table$e.value == 1,]$average.fill)
cor(s.001.table[s.001.table$e.value == 1,]$correlation.value, s.001.table[s.001.table$e.value == 1,]$average.NODF, method = "pearson")

# When s = 0.1... #####
s.01.table <-  results.table[results.table$s.value == 0.1,]
s.01.table
plot(s.01.table[results.table$e.value == 0.0000001,]$correlation.value, s.01.table[results.table$e.value == 0.0000001,]$average.NODF)
pcor.test(s.01.table[s.01.table$e.value == 0.0000001,]$correlation.value, s.01.table[s.01.table$e.value == 0.0000001,]$average.NODF, s.01.table[s.01.table$e.value == 0.0000001,]$average.fill)
cor(s.01.table[s.01.table$e.value == 0.0000001,]$correlation.value, s.01.table[s.01.table$e.value == 0.0000001,]$average.NODF, method = "pearson")

plot(s.01.table[results.table$e.value == 0.001,]$correlation.value, s.01.table[results.table$e.value == 0.001,]$average.NODF)
pcor.test(s.01.table[s.01.table$e.value == 0.001,]$correlation.value, s.01.table[s.01.table$e.value == 0.001,]$average.NODF, s.01.table[s.01.table$e.value == 0.001,]$average.fill)
cor(s.01.table[s.01.table$e.value == 0.001,]$correlation.value, s.01.table[s.01.table$e.value == 0.001,]$average.NODF, method = "pearson")

plot(s.01.table[results.table$e.value == 0.01,]$correlation.value, s.01.table[results.table$e.value == 0.01,]$average.NODF)
pcor.test(s.01.table[s.01.table$e.value == 0.01,]$correlation.value, s.01.table[s.01.table$e.value == 0.01,]$average.NODF, s.01.table[s.01.table$e.value == 0.01,]$average.fill)
cor(s.01.table[s.01.table$e.value == 0.01,]$correlation.value, s.01.table[s.01.table$e.value == 0.01,]$average.NODF, method = "pearson")

plot(s.01.table[results.table$e.value == 0.1,]$correlation.value, s.01.table[results.table$e.value == 0.1,]$average.NODF)
pcor.test(s.01.table[s.01.table$e.value == 0.1,]$correlation.value, s.01.table[s.01.table$e.value == 0.1,]$average.NODF, s.01.table[s.01.table$e.value == 0.1,]$average.fill)
cor(s.01.table[s.01.table$e.value == 0.1,]$correlation.value, s.01.table[s.01.table$e.value == 0.1,]$average.NODF, method = "pearson")

plot(s.01.table[results.table$e.value == 0.25,]$correlation.value, s.01.table[results.table$e.value == 0.25,]$average.NODF)
pcor.test(s.01.table[s.01.table$e.value == 0.25,]$correlation.value, s.01.table[s.01.table$e.value == 0.25,]$average.NODF, s.01.table[s.01.table$e.value == 0.25,]$average.fill)
cor(s.01.table[s.01.table$e.value == 0.25,]$correlation.value, s.01.table[s.01.table$e.value == 0.25,]$average.NODF, method = "pearson")

plot(s.01.table[results.table$e.value == 0.5,]$correlation.value, s.01.table[results.table$e.value == 0.5,]$average.NODF)
pcor.test(s.01.table[s.01.table$e.value == 0.5,]$correlation.value, s.01.table[s.01.table$e.value == 0.5,]$average.NODF, s.01.table[s.01.table$e.value == 0.5,]$average.fill)
cor(s.01.table[s.01.table$e.value == 0.5,]$correlation.value, s.01.table[s.01.table$e.value == 0.5,]$average.NODF, method = "pearson")

plot(s.01.table[results.table$e.value == 0.75,]$correlation.value, s.01.table[results.table$e.value == 0.75,]$average.NODF)
pcor.test(s.01.table[s.01.table$e.value == 0.75,]$correlation.value, s.01.table[s.01.table$e.value == 0.75,]$average.NODF, s.01.table[s.01.table$e.value == 0.75,]$average.fill)
cor(s.01.table[s.01.table$e.value == 0.75,]$correlation.value, s.01.table[s.01.table$e.value == 0.75,]$average.NODF, method = "pearson")

plot(s.01.table[results.table$e.value == 1,]$correlation.value, s.01.table[results.table$e.value == 1,]$average.NODF)
pcor.test(s.01.table[s.01.table$e.value == 1,]$correlation.value, s.01.table[s.01.table$e.value == 1,]$average.NODF, s.01.table[s.01.table$e.value == 1,]$average.fill)
cor(s.01.table[s.01.table$e.value == 1,]$correlation.value, s.01.table[s.01.table$e.value == 1,]$average.NODF, method = "pearson")


# When s = 0.25... #####
s.025.table <-  results.table[results.table$s.value == 0.25,]
s.025.table
plot(s.025.table[results.table$e.value == 0.0000001,]$correlation.value, s.025.table[results.table$e.value == 0.0000001,]$average.NODF)
pcor.test(s.025.table[s.025.table$e.value == 0.0000001,]$correlation.value, s.025.table[s.025.table$e.value == 0.0000001,]$average.NODF, s.025.table[s.025.table$e.value == 0.0000001,]$average.fill)
cor(s.025.table[s.025.table$e.value == 0.0000001,]$correlation.value, s.025.table[s.025.table$e.value == 0.0000001,]$average.NODF, method = "pearson")

plot(s.025.table[results.table$e.value == 0.001,]$correlation.value, s.025.table[results.table$e.value == 0.001,]$average.NODF)
pcor.test(s.025.table[s.025.table$e.value == 0.001,]$correlation.value, s.025.table[s.025.table$e.value == 0.001,]$average.NODF, s.025.table[s.025.table$e.value == 0.001,]$average.fill)
cor(s.025.table[s.025.table$e.value == 0.001,]$correlation.value, s.025.table[s.025.table$e.value == 0.001,]$average.NODF, method = "pearson")

plot(s.025.table[results.table$e.value == 0.01,]$correlation.value, s.025.table[results.table$e.value == 0.01,]$average.NODF)
pcor.test(s.025.table[s.025.table$e.value == 0.01,]$correlation.value, s.025.table[s.025.table$e.value == 0.01,]$average.NODF, s.025.table[s.025.table$e.value == 0.01,]$average.fill)
cor(s.025.table[s.025.table$e.value == 0.01,]$correlation.value, s.025.table[s.025.table$e.value == 0.01,]$average.NODF, method = "pearson")

plot(s.025.table[results.table$e.value == 0.1,]$correlation.value, s.025.table[results.table$e.value == 0.1,]$average.NODF)
pcor.test(s.025.table[s.025.table$e.value == 0.1,]$correlation.value, s.025.table[s.025.table$e.value == 0.1,]$average.NODF, s.025.table[s.025.table$e.value == 0.1,]$average.fill)
cor(s.025.table[s.025.table$e.value == 0.1,]$correlation.value, s.025.table[s.025.table$e.value == 0.1,]$average.NODF, method = "pearson")

plot(s.025.table[results.table$e.value == 0.25,]$correlation.value, s.025.table[results.table$e.value == 0.25,]$average.NODF)
pcor.test(s.025.table[s.025.table$e.value == 0.25,]$correlation.value, s.025.table[s.025.table$e.value == 0.25,]$average.NODF, s.025.table[s.025.table$e.value == 0.25,]$average.fill)
cor(s.025.table[s.025.table$e.value == 0.25,]$correlation.value, s.025.table[s.025.table$e.value == 0.25,]$average.NODF, method = "pearson")

plot(s.025.table[results.table$e.value == 0.5,]$correlation.value, s.025.table[results.table$e.value == 0.5,]$average.NODF)
pcor.test(s.025.table[s.025.table$e.value == 0.5,]$correlation.value, s.025.table[s.025.table$e.value == 0.5,]$average.NODF, s.025.table[s.025.table$e.value == 0.5,]$average.fill)
cor(s.025.table[s.025.table$e.value == 0.5,]$correlation.value, s.025.table[s.025.table$e.value == 0.5,]$average.NODF, method = "pearson")

plot(s.025.table[results.table$e.value == 0.75,]$correlation.value, s.025.table[results.table$e.value == 0.75,]$average.NODF)
pcor.test(s.025.table[s.025.table$e.value == 0.75,]$correlation.value, s.025.table[s.025.table$e.value == 0.75,]$average.NODF, s.025.table[s.025.table$e.value == 0.75,]$average.fill)
cor(s.025.table[s.025.table$e.value == 0.75,]$correlation.value, s.025.table[s.025.table$e.value == 0.75,]$average.NODF, method = "pearson")

plot(s.025.table[results.table$e.value == 1,]$correlation.value, s.025.table[results.table$e.value == 1,]$average.NODF)
pcor.test(s.025.table[s.025.table$e.value == 1,]$correlation.value, s.025.table[s.025.table$e.value == 1,]$average.NODF, s.025.table[s.025.table$e.value == 1,]$average.fill)
cor(s.025.table[s.025.table$e.value == 1,]$correlation.value, s.025.table[s.025.table$e.value == 1,]$average.NODF, method = "pearson")


# When s = 0.5... #####
s.05.table <-  results.table[results.table$s.value == 0.5,]
s.05.table
plot(s.05.table[results.table$e.value == 0.0000001,]$correlation.value, s.05.table[results.table$e.value == 0.0000001,]$average.NODF)
pcor.test(s.05.table[s.05.table$e.value == 0.0000001,]$correlation.value, s.05.table[s.05.table$e.value == 0.0000001,]$average.NODF, s.05.table[s.05.table$e.value == 0.0000001,]$average.fill)
cor(s.05.table[s.05.table$e.value == 0.0000001,]$correlation.value, s.05.table[s.05.table$e.value == 0.0000001,]$average.NODF, method = "pearson")

plot(s.05.table[results.table$e.value == 0.001,]$correlation.value, s.05.table[results.table$e.value == 0.001,]$average.NODF)
pcor.test(s.05.table[s.05.table$e.value == 0.001,]$correlation.value, s.05.table[s.05.table$e.value == 0.001,]$average.NODF, s.05.table[s.05.table$e.value == 0.001,]$average.fill)
cor(s.05.table[s.05.table$e.value == 0.001,]$correlation.value, s.05.table[s.05.table$e.value == 0.001,]$average.NODF, method = "pearson")

plot(s.05.table[results.table$e.value == 0.01,]$correlation.value, s.05.table[results.table$e.value == 0.01,]$average.NODF)
pcor.test(s.05.table[s.05.table$e.value == 0.01,]$correlation.value, s.05.table[s.05.table$e.value == 0.01,]$average.NODF, s.05.table[s.05.table$e.value == 0.01,]$average.fill)
cor(s.05.table[s.05.table$e.value == 0.01,]$correlation.value, s.05.table[s.05.table$e.value == 0.01,]$average.NODF, method = "pearson")

plot(s.05.table[results.table$e.value == 0.1,]$correlation.value, s.05.table[results.table$e.value == 0.1,]$average.NODF)
pcor.test(s.05.table[s.05.table$e.value == 0.1,]$correlation.value, s.05.table[s.05.table$e.value == 0.1,]$average.NODF, s.05.table[s.05.table$e.value == 0.1,]$average.fill)
cor(s.05.table[s.05.table$e.value == 0.1,]$correlation.value, s.05.table[s.05.table$e.value == 0.1,]$average.NODF, method = "pearson")

plot(s.05.table[results.table$e.value == 0.25,]$correlation.value, s.05.table[results.table$e.value == 0.25,]$average.NODF)
pcor.test(s.05.table[s.05.table$e.value == 0.25,]$correlation.value, s.05.table[s.05.table$e.value == 0.25,]$average.NODF, s.05.table[s.05.table$e.value == 0.25,]$average.fill)
cor(s.05.table[s.05.table$e.value == 0.25,]$correlation.value, s.05.table[s.05.table$e.value == 0.25,]$average.NODF, method = "pearson")

plot(s.05.table[results.table$e.value == 0.5,]$correlation.value, s.05.table[results.table$e.value == 0.5,]$average.NODF)
pcor.test(s.05.table[s.05.table$e.value == 0.5,]$correlation.value, s.05.table[s.05.table$e.value == 0.5,]$average.NODF, s.05.table[s.05.table$e.value == 0.5,]$average.fill)
cor(s.05.table[s.05.table$e.value == 0.5,]$correlation.value, s.05.table[s.05.table$e.value == 0.5,]$average.NODF, method = "pearson")

plot(s.05.table[results.table$e.value == 0.75,]$correlation.value, s.05.table[results.table$e.value == 0.75,]$average.NODF)
pcor.test(s.05.table[s.05.table$e.value == 0.75,]$correlation.value, s.05.table[s.05.table$e.value == 0.75,]$average.NODF, s.05.table[s.05.table$e.value == 0.75,]$average.fill)
cor(s.05.table[s.05.table$e.value == 0.75,]$correlation.value, s.05.table[s.05.table$e.value == 0.75,]$average.NODF, method = "pearson")

plot(s.05.table[results.table$e.value == 1,]$correlation.value, s.05.table[results.table$e.value == 1,]$average.NODF)
pcor.test(s.05.table[s.05.table$e.value == 1,]$correlation.value, s.05.table[s.05.table$e.value == 1,]$average.NODF, s.05.table[s.05.table$e.value == 1,]$average.fill)
cor(s.05.table[s.05.table$e.value == 1,]$correlation.value, s.05.table[s.05.table$e.value == 1,]$average.NODF, method = "pearson")


# When s = 0.75... #####
s.075.table <-  results.table[results.table$s.value == 0.75,]
s.075.table
plot(s.075.table[results.table$e.value == 0.0000001,]$correlation.value, s.075.table[results.table$e.value == 0.0000001,]$average.NODF)
pcor.test(s.075.table[s.075.table$e.value == 0.0000001,]$correlation.value, s.075.table[s.075.table$e.value == 0.0000001,]$average.NODF, s.075.table[s.075.table$e.value == 0.0000001,]$average.fill)
cor(s.075.table[s.075.table$e.value == 0.0000001,]$correlation.value, s.075.table[s.075.table$e.value == 0.0000001,]$average.NODF, method = "pearson")

plot(s.075.table[results.table$e.value == 0.001,]$correlation.value, s.075.table[results.table$e.value == 0.001,]$average.NODF)
pcor.test(s.075.table[s.075.table$e.value == 0.001,]$correlation.value, s.075.table[s.075.table$e.value == 0.001,]$average.NODF, s.075.table[s.075.table$e.value == 0.001,]$average.fill)
cor(s.075.table[s.075.table$e.value == 0.001,]$correlation.value, s.075.table[s.075.table$e.value == 0.001,]$average.NODF, method = "pearson")

plot(s.075.table[results.table$e.value == 0.01,]$correlation.value, s.075.table[results.table$e.value == 0.01,]$average.NODF)
pcor.test(s.075.table[s.075.table$e.value == 0.01,]$correlation.value, s.075.table[s.075.table$e.value == 0.01,]$average.NODF, s.075.table[s.075.table$e.value == 0.01,]$average.fill)
cor(s.075.table[s.075.table$e.value == 0.01,]$correlation.value, s.075.table[s.075.table$e.value == 0.01,]$average.NODF, method = "pearson")

plot(s.075.table[results.table$e.value == 0.1,]$correlation.value, s.075.table[results.table$e.value == 0.1,]$average.NODF)
pcor.test(s.075.table[s.075.table$e.value == 0.1,]$correlation.value, s.075.table[s.075.table$e.value == 0.1,]$average.NODF, s.075.table[s.075.table$e.value == 0.1,]$average.fill)
cor(s.075.table[s.075.table$e.value == 0.1,]$correlation.value, s.075.table[s.075.table$e.value == 0.1,]$average.NODF, method = "pearson")

plot(s.075.table[results.table$e.value == 0.25,]$correlation.value, s.075.table[results.table$e.value == 0.25,]$average.NODF)
pcor.test(s.075.table[s.075.table$e.value == 0.25,]$correlation.value, s.075.table[s.075.table$e.value == 0.25,]$average.NODF, s.075.table[s.075.table$e.value == 0.25,]$average.fill)
cor(s.075.table[s.075.table$e.value == 0.25,]$correlation.value, s.075.table[s.075.table$e.value == 0.25,]$average.NODF, method = "pearson")

plot(s.075.table[results.table$e.value == 0.5,]$correlation.value, s.075.table[results.table$e.value == 0.5,]$average.NODF)
pcor.test(s.075.table[s.075.table$e.value == 0.5,]$correlation.value, s.075.table[s.075.table$e.value == 0.5,]$average.NODF, s.075.table[s.075.table$e.value == 0.5,]$average.fill)
cor(s.075.table[s.075.table$e.value == 0.5,]$correlation.value, s.075.table[s.075.table$e.value == 0.5,]$average.NODF, method = "pearson")

plot(s.075.table[results.table$e.value == 0.75,]$correlation.value, s.075.table[results.table$e.value == 0.75,]$average.NODF)
pcor.test(s.075.table[s.075.table$e.value == 0.75,]$correlation.value, s.075.table[s.075.table$e.value == 0.75,]$average.NODF, s.075.table[s.075.table$e.value == 0.75,]$average.fill)
cor(s.075.table[s.075.table$e.value == 0.75,]$correlation.value, s.075.table[s.075.table$e.value == 0.75,]$average.NODF, method = "pearson")

plot(s.075.table[results.table$e.value == 1,]$correlation.value, s.075.table[results.table$e.value == 1,]$average.NODF)
pcor.test(s.075.table[s.075.table$e.value == 1,]$correlation.value, s.075.table[s.075.table$e.value == 1,]$average.NODF, s.075.table[s.075.table$e.value == 1,]$average.fill)
cor(s.075.table[s.075.table$e.value == 1,]$correlation.value, s.075.table[s.075.table$e.value == 1,]$average.NODF, method = "pearson")


# When s = 1... #####
s.1.table <-  results.table[results.table$s.value == 1,]
s.1.table
plot(s.1.table[results.table$e.value == 0.0000001,]$correlation.value, s.1.table[results.table$e.value == 0.0000001,]$average.NODF)
pcor.test(s.1.table[s.1.table$e.value == 0.0000001,]$correlation.value, s.1.table[s.1.table$e.value == 0.0000001,]$average.NODF, s.1.table[s.1.table$e.value == 0.0000001,]$average.fill)
cor(s.1.table[s.1.table$e.value == 0.0000001,]$correlation.value, s.1.table[s.1.table$e.value == 0.0000001,]$average.NODF, method = "pearson")

plot(s.1.table[results.table$e.value == 0.001,]$correlation.value, s.1.table[results.table$e.value == 0.001,]$average.NODF)
pcor.test(s.1.table[s.1.table$e.value == 0.001,]$correlation.value, s.1.table[s.1.table$e.value == 0.001,]$average.NODF, s.1.table[s.1.table$e.value == 0.001,]$average.fill)
cor(s.1.table[s.1.table$e.value == 0.001,]$correlation.value, s.1.table[s.1.table$e.value == 0.001,]$average.NODF, method = "pearson")

plot(s.1.table[results.table$e.value == 0.01,]$correlation.value, s.1.table[results.table$e.value == 0.01,]$average.NODF)
pcor.test(s.1.table[s.1.table$e.value == 0.01,]$correlation.value, s.1.table[s.1.table$e.value == 0.01,]$average.NODF, s.1.table[s.1.table$e.value == 0.01,]$average.fill)
cor(s.1.table[s.1.table$e.value == 0.01,]$correlation.value, s.1.table[s.1.table$e.value == 0.01,]$average.NODF, method = "pearson")

plot(s.1.table[results.table$e.value == 0.1,]$correlation.value, s.1.table[results.table$e.value == 0.1,]$average.NODF)
pcor.test(s.1.table[s.1.table$e.value == 0.1,]$correlation.value, s.1.table[s.1.table$e.value == 0.1,]$average.NODF, s.1.table[s.1.table$e.value == 0.1,]$average.fill)
cor(s.1.table[s.1.table$e.value == 0.1,]$correlation.value, s.1.table[s.1.table$e.value == 0.1,]$average.NODF, method = "pearson")

plot(s.1.table[results.table$e.value == 0.25,]$correlation.value, s.1.table[results.table$e.value == 0.25,]$average.NODF)
pcor.test(s.1.table[s.1.table$e.value == 0.25,]$correlation.value, s.1.table[s.1.table$e.value == 0.25,]$average.NODF, s.1.table[s.1.table$e.value == 0.25,]$average.fill)
cor(s.1.table[s.1.table$e.value == 0.25,]$correlation.value, s.1.table[s.1.table$e.value == 0.25,]$average.NODF, method = "pearson")

plot(s.1.table[results.table$e.value == 0.5,]$correlation.value, s.1.table[results.table$e.value == 0.5,]$average.NODF)
pcor.test(s.1.table[s.1.table$e.value == 0.5,]$correlation.value, s.1.table[s.1.table$e.value == 0.5,]$average.NODF, s.1.table[s.1.table$e.value == 0.5,]$average.fill)
cor(s.1.table[s.1.table$e.value == 0.5,]$correlation.value, s.1.table[s.1.table$e.value == 0.5,]$average.NODF, method = "pearson")

plot(s.1.table[results.table$e.value == 0.75,]$correlation.value, s.1.table[results.table$e.value == 0.75,]$average.NODF)
pcor.test(s.1.table[s.1.table$e.value == 0.75,]$correlation.value, s.1.table[s.1.table$e.value == 0.75,]$average.NODF, s.1.table[s.1.table$e.value == 0.75,]$average.fill)
cor(s.1.table[s.1.table$e.value == 0.75,]$correlation.value, s.1.table[s.1.table$e.value == 0.75,]$average.NODF, method = "pearson")

plot(s.1.table[results.table$e.value == 1,]$correlation.value, s.1.table[results.table$e.value == 1,]$average.NODF)
pcor.test(s.1.table[s.1.table$e.value == 1,]$correlation.value, s.1.table[s.1.table$e.value == 1,]$average.NODF, s.1.table[s.1.table$e.value == 1,]$average.fill)
cor(s.1.table[s.1.table$e.value == 1,]$correlation.value, s.1.table[s.1.table$e.value == 1,]$average.NODF, method = "pearson")

