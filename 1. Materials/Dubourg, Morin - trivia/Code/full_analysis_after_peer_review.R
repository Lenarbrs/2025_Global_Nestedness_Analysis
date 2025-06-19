setwd("/Users/edgardubourg/Documents/Recherche/Travaux/Articles/Expertise Evaluation/Trivia Knowledge and Nestedness/V4")

#0. Loading packages ####

library(readr) #to open files
library(tidyverse) #for data wrangling
library(lmerTest) #for regressions
library(performance) #for model comparison
library(ggplot2) #data viz
library(vegan) #nestedness analysis & matrix permutations

#1. Data cleaning ####

rawdata <- read_csv("rawdata.csv")
summary(rawdata)
head(rawdata)

# Remove the first two rows
rawdata <- rawdata[-c(1, 2), ]

# Delete the first 19 columns
rawdata <- rawdata[, -c(1:19)]

# Rename the last column to "id_participant"
colnames(rawdata)[ncol(rawdata)] <- "id_participant"

# Move the "id_participant" column to the first position
rawdata <- rawdata %>% select(id_participant, everything())

#2. Attention check ####

# Isolate rows where participants failed the attention check
failed_attention_check <- rawdata %>%
  filter(!attention_check %in% c("I pay attention", "I pay attention.", "i pay attention"))

# Remove rows from rawdata where participants failed the attention check
rawdata <- rawdata %>%
  filter(attention_check %in% c("I pay attention", "I pay attention.", "i pay attention"))

# Delete the attention_check column from rawdata
rawdata <- select(rawdata, -attention_check)

# Remove failed_attention_check dataset
rm(failed_attention_check)

# Keep only id_participant and columns starting with v*answer
filtered_data <- rawdata %>%
  select(id_participant, starts_with("vi1_answer"), starts_with("v2_answer"), 
         starts_with("v3_answer"), starts_with("v4_answer"), starts_with("v5_answer"),
         starts_with("v6_answer"), starts_with("v7_answer"), starts_with("v8_answer"),
         starts_with("v9_answer"), starts_with("v10_answer"), starts_with("v11_answer"),
         starts_with("v12_answer"), starts_with("v13_answer"), starts_with("v14_answer"),
         starts_with("v15_answer"), starts_with("v16_answer"), starts_with("v17_answer"),
         starts_with("v18_answer"), starts_with("v19_answer"), starts_with("v20_answer"),
         starts_with("v21_answer"), starts_with("v22_answer"), starts_with("v23_answer"),
         starts_with("v24_answer"), starts_with("v25_answer"), starts_with("v26_answer"),
         starts_with("v27_answer"), starts_with("v28_answer"), starts_with("v29_answer"),
         starts_with("v30_answer"), starts_with("v31_answer"), starts_with("v32_answer"),
         starts_with("v33_answer"), starts_with("v34_answer"), starts_with("v35_answer"),
         starts_with("v36_answer"), starts_with("v37_answer"), starts_with("v38_answer"),
         starts_with("v39_answer"), starts_with("v40_answer"), starts_with("v41_answer"),
         starts_with("v42_answer"), starts_with("v43_answer"), starts_with("v44_answer"),
         starts_with("v45_answer"))

# Rename columns by removing "_answer" and "#1"
filtered_data <- filtered_data %>%
  rename_with(~ gsub("_answer", "", .), .cols = everything()) %>%
  rename_with(~ gsub("#1", "", .), .cols = everything())

# Pivot the dataset to a longer format
long_data <- filtered_data %>%
  pivot_longer(
    -id_participant, # Exclude id_participant from the pivoting
    names_to = "original_name", # Temporarily store original column names
    values_to = "evaluation" # Store values in 'evaluation' column
  ) %>% drop_na(evaluation)

rm(filtered_data)

# Creating columns for VI and question evaluated
long_data <- long_data %>%
  mutate(original_name = gsub("vi?", "", original_name, perl = TRUE)) %>%
  separate(original_name, into = c("question_virtual_individual", "question_evaluated"), sep = "_")

long_data$question_virtual_individual <- as.numeric(long_data$question_virtual_individual)
long_data$question_evaluated <- as.numeric(long_data$question_evaluated)
long_data$evaluation <- as.numeric(long_data$evaluation)

# Adjust 'question_evaluated' based on 'question_virtual_individual' 
#(because on Qualtrics, all questions evaluated were numbered from 1 to 15 for each theme)
long_data <- long_data %>%
  mutate(
    question_virtual_individual = as.integer(question_virtual_individual), # Ensure this column is numeric
    question_evaluated = as.integer(question_evaluated), # Convert to integer to perform arithmetic operations
    question_evaluated = case_when(
      question_virtual_individual >= 1 & question_virtual_individual <= 15 ~ question_evaluated,
      question_virtual_individual >= 16 & question_virtual_individual <= 30 ~ question_evaluated + 15,
      question_virtual_individual > 30 ~ question_evaluated + 30,
      TRUE ~ question_evaluated # Default case, should not be reached due to the conditions covering all possibilities
    )
  )

# Change all occurrences of "2" to "0" in the 'evaluation' column
long_data <- long_data %>%
  mutate(
    evaluation = ifelse(evaluation == "2", "0", evaluation)
  )

long_data$evaluation <- as.integer(long_data$evaluation)

# Create exclusion criteria table (people who changed the pre-ticked answer "1" when the question evaluated is the one answered correctly by the VI
exclusion_criteria <- long_data %>%
  filter(evaluation == "0" & question_virtual_individual == question_evaluated)

# Get unique participant from exclusion_criteria
exclude_participants <- unique(exclusion_criteria$id_participant)

# Exclude rows from long_data and rawdata for those participants
long_data <- long_data %>%
  filter(!(id_participant %in% exclude_participants))

rawdata <- rawdata %>%
  filter(!(id_participant %in% exclude_participants))

rm(exclude_participants, exclusion_criteria)

summary(long_data)

#3. Trivia answer scoring ####

# Create new dataset with id_participant and all answers
answers_trivia_participant <- rawdata %>%
  select(id_participant, starts_with("q"))

# Pivot dataset to long format and remove NA answers
participant_answers_trivia_long <- answers_trivia_participant %>%
  pivot_longer(
    -id_participant, # Exclude the id_participant column from the pivot
    names_to = "id_question", # New column for the id_question identifiers
    values_to = "answer" # New column for the participant answers
  ) %>%
  mutate(id_question = as.integer(sub("q", "", id_question))) %>% # Remove "q" and convert to integer
  drop_na(answer) # Remove rows with NA in the answer column

summary(participant_answers_trivia_long)

rm(answers_trivia_participant)

# Define correct answers with variations

  # Theme 1: Solar system
correct_answers <- list(
  "Mercury" = c("mercury", "Mercury", "Merc", "mecury", "Mecury", "Mercury.", "Mercruy", "The first planet of the solar system is Mercury."),
  "Neil Armstrong" = c("neil armstrong", "Niel Armstrong", "Neil Armstrong (Buzz should be mentioned here too)", "niel armstrong", "NEIL  AMSTRONG", "Neil Armstron", "Niel Armstrong", "niel armstrong", "Neal Armstrong", "Neil Armstron", "Neil Armstrong.", "NEIL ARSTRONG", "Armstrong?", "Neil Armstrong", "armstrong", "Neil A.", "Neil       armstrong", "Mr. Armstrong", "Neil Amstrong"),
  "Venus" = c("venus", "Venus is the only planet in the solar system that rotates clockwise (also known as retrograde rotation).", "Venus", "Venus is the only planet in the solar system that rotates clockwise."),
  "Saturn" = c("saturn", "saturn's hexagon", "Saturn", "Saturn has a hexagon-shaped cloud formation on its north pole.", "satrun", "Saturn is the planet with a hexagon-shaped cloud formation on its north pole."),
  "365" = c("365", "365 days", "365.25", "365.25 days", "364.25", "365 days (and a quarter that gives us leap years...)", "365.3", "365.", "365 days.", "365 or 366 in leap years", "Approximately 365.25 days", "364.25 days", "365.26"),
  "Mercury and Venus" = c("mercury and venus", "Mercury and Venus", "venus and mercury", "Venus and Mercury", "mercury venus", "venus mercury", "mercury, venus", "venus, mercury", "Mecury and venus", "mecury and vernus", "Mercury or venus", "Mercury & Venus", "The only two planets in our solar system that do not possess any moons are Mercury and Venus.", "Mercury and Venus.", "Mercury/Venus", "Mercury and Venus.", "Mercury & Venus", "Mercury and Venus.", "Mercury   Venus"),
  "the Moon" = c("the moon", "Moon", "moon", "the  moon", "the moom","Earth's only natural satellite is the Moon.", "the Moon", "Luna", "Moon.", "The Moon (Luna)", "the moon?", "The moon/Luna", "The moon.", "Moon/Luna", "the Moon.", "Moom", "our moon"),
  "Earth" = c("earth", "Earth", "the earth", "The Earth", "ear", "Eath", "Esrth", "Earth is the only habitable (livable) planet in our solar system.", "Planet Earth", "Earth.", "Earth is considered the only habitable (livable) planet of our solar system."),
  "5" = c("5", "4.5", "4.6 billion years", "4billion", "4.61", "4billion years", "4.5 BILLION YEARS", "4.7 billion", "4.6billion years ago", "4.6 billion", "4500000000 YEARS OLD", "approximately 4.6 billions years", "4 Billion", "4.6", "4.6 Billion", "5.6", "5 billion", "Four billion", "The Sun is approximately 4.6 billion years old.", "4 billion", "4.6 billion", "4.2", "4.6 billion years old.", "4.5 billion years", "The Sun is approximately 4.6 billion years old.", "4 billion years", "4.4 billion years", "5 billion years", "4 billion years old", "five billion", "5b", "5bn", "5 billion yrs", "4.603 billion years", "4500000000", "4"),
  "Voyager 1" = c("voyager 1", "Voyager 1", "voyager", "Voyager", "voyager i", "NASA Voyager", "The first human-made object to cross the limits of our solar system was Voyager 1.", "Voyager 1 spacecraft.", "VOYAGER 1 SPACE CRAFT", "Voyager I", "Voyager 1 (or 2)", "Voygaer", "The voyager", "The first human object that crossed the limits of our solar system is Voyager 1."),
  "Neptune" = c("neptune", "Neptune", "Nepturn", "Neptune (since Pluto is now a 'minor planet')", "Neptume"),
  "Ceres" = c("ceres", "Ceres", "dwarf planet ceres", "The largest celestial object of the asteroid belt is Ceres."),
  "Dwarf planet" = c("dwarf planet", "Dwarf, planet","Dwarf Star", "Dwarf plant", "Dwarf planet.", "dwarf planet?", "dwarf plane", "Dwarf Giant", "drawf planet", "drawf planet", "A dwarf Planet", "Dwarf planet", "dwarf plnt", "dwarf", "Dwarf", "planetoid or dwarf planet", "Pluto is no longer considered a planet by scientists; instead, it is classified as a dwarf planet.", "Pluto is no longer considered a planet by scientists nowadays but as a dwarf planet."),
  "Mars and Jupiter" = c("mars and jupiter", "mars, jupiter", "Mars, Jupiter", "Mars and Jupiter.", "Mars & JUpiter", "JUPITTER AND MAR", "The asteroid belt is located between the planets Mars and Jupiter.", "The asteroid belt is located between the orbits of Mars and Jupiter.", "Mars and Jupiter", "between mars and jupiter", "mars jupiter", "Mars Jupiter", "jupiter and mars"),
  "Kuiper belt" = c("kuiper belt", "kuipter belt", "Kuiper Belt.", "kiper belt", "Keiper belt", "Kaipur Belt", "The Kuiper Belt", "Kuiper Belt", "kuiper", "Kuiper", "kuiperbelt", "Kuiperbelt", "Kuiperr belt", "The circumstellar disc located beyond the orbit of Neptune is called the Kuiper Belt.", "The circumstellar disc located beyond the orbit of Neptune is called the Kuiper Belt."),
  
  # Theme 2: American history
  "1492" = c("1492", "1492 AD", "AD 1492", "in 1492", "Fourteen ninety two", "October 1492", "October 12th, 1492", "October 12, 1492.", "October 12, 1492", "Christopher Columbus's first voyage to the Americas began on August 3, 1492, when he set sail from Spain.", "1492, in October"),
  "1776" = c("1776", "1776 AD", "AD 1776", "in 1776", "1776 july 4", "July 4 1776", "7/4/1776", "July 4, 1976", "July, 4 1776", "July 4th, 1776", "July 4th 1776", "July 4,1776", "July 4, 1776", "Juky 4 1776", "7/7/1776", "7/4/76", "7/04/1776", "6-4-1776", "4th July 1776", "4 July 1776", "1776, July 4th", "14th July ,1776", "07/04/1776", "04/06/1776"),
  "1861" = c("1861", "1861-1865", "1860", "April 12th, 1861", "1861 AD", "AD 1861", "in 1861", "It began on April 12 1861", "The American Civil War began in 1861. Specifically, the first shots were fired at Fort Sumter, South Carolina, on April 12, 1861, marking the beginning of the conflict."),
  "1783" = c("1783", "1783 AD", "AD 1783", "in 1783", "March 1st, 1781", "1781", "1780"),
  "1781" = c("1781", "1781 AD", "1780", "AD 1781", "in 1781", "September 4th, 1781", "September 4,1781", "september 4,1781", "September 4, 1781.", "September 4, 1781"),
  "1941" = c("1941", "1941 AD", "AD 1941", "in 1941", "The United States declared war on Japan on December 8, 1941, the day after the Japanese attack on Pearl Harbor.", "November 7, 1941", "December 9, 1941", "December 8th, 1941", "December 8, 1941", "December 8 1941", "December 8", "December 7th, 1941", "December 7, 1941", "december 1945", "Dec. 8, 1941", "Dec 8, 1941", "Dec 8 1941", "dec 7,1941", "Dec 7 1941", "dec 7 1941", "8th December,1941", "8 December 1941", "8 december 1941"),
  "1870" = c("1870", "1870 AD", "AD 1870", "in 1870", "1869 or 1870"),
  "John Adams" = c("john adams", "John Adams", "adams", "Adams", "The first President to live in the White House was John Adams.", "President John Adams", "John Quincy Adams", "John Q. Adams", "John Adams?", "Joh Adams"),
  "Boston" = c("boston", "Boston", "boston ma", "Boston MA", "Boston?", "Boston, Massachusetts", "Boston, MA"),
  "Grover Cleveland" = c("grover cleveland", "Grover Cleverland", "Grover Cleavland", "cloeveland", "Grover Cleveland", "cleveland", "Cleveland", "Cleavland", "Grover Cleaveland", "The President of the United States who served between 1893 and 1897 was Grover Cleveland."),
  "1945" = c("1945", "1945 AD", "AD 1945", "in 1945", "May 1945", "Franklin D. Roosevelt, the 32nd President of the United States, died on April 12, 1945.", "April, 1945", "april 12.1945", "April 12, 1945.", "April 12, 1945", "Apr 19445", "12th April, 1945", "12 april 1945"),
  "South Carolina" = c("south carolina", "South Carolina", "SC", "s carolina", "South Caroline", "S. Carolina"),
  "1920-1933" = c("1920-1933", "1920 to 1933", "January 17, 1920, and ended on December 5, 1933", "1920 AND 1933", "1920 and 1933", "1920 1933", "1920 - 1933", "1933-1920", "17 January 1920 - 5 December 1933", "Started on January 17th, 1920 and ended on December 5th, 1933", "Prohibition in the United States officially started on January 17, 1920, with the ratification of the 18th Amendment to the United States Constitution, which prohibited the manufacture, sale, and transportation of alcoholic beverages."),
  "The Bill of Rights" = c("the bill of rights", "Bill of Rights", "bill of rights", "the biil of rights", "The Bill of Rights?", "Bill ofRights", "Bill of Right"),
  "The Articles of Confederation" = c("the articles of confederation", "Articles of Conferation", "Articles of Confederation.", "Articles of Confederate", "Article of confederation", "articles of confederation", "The article of confederation", "Articles of Confederations", "The Articles of the Confederation", "The Artciles of Confederation", "Confederation articles", "confederation", "Articles of the Confederation"),
  
  # Theme 3: Superheroes
  "Martha" = c("martha", "Martha", "martha wayne", "Martha Wayne", "The name of Batman's mother is Martha Wayne.", "Martha??", "Martha Wayne, nee Kane"),
  "Hammer" = c("hammer", "Mjölnir", "A hammer called Mjolnir", "A Hammer", "A hammer", "Hammer of Odin", "hammer mallet", "Hammer / Mjolnir", "A magical hammer", "Mjollnir", "Mjolier/hammer", "Huge Hammer", "his hammer mjolnir", "Hammer.", "the hammer.", "Mjonir", "Mjonier", "Mjolnr", "Mjolnor (I don't know the spelling)", "Mjolnir (hammer)", "Hammer", "mjolnir", "Mjolnir", "Molnir", "Moljinir", "magic hammer", "Mjnoller", "magical hammer", "Thor uses a magical hammer called Mjolnir as his primary weapon."),
  "Peter Parker" = c("peter parker", "Peter Parker", "The real name of Spider-Man is Peter Parker.", "Peter Parket", "peter parkerq", "Peter Parker.", "Peter Benjamin Parker", "Peter benjamin parker", "Miles Morales", "Peter", "Parker"),
  "Brother" = c("brother", "Her younger brother", "He is her Brother", "brother and sister", "Brother", "Younger brother", "Usually his sister and both on the fantastic four", "sibling", "bro", "Her brother", "is the younger brother of the Invisible Woman", "They are brother and sister.", "The Human Torch is the brother of the Invisible Woman (Sue Storm) in the Fantastic Four. His real name is Johnny Storm.", "The brother", "Sister", "sibliings, brother", "Little brother"),
  "Stan Lee" = c("stan lee", "Stan Lee", "stanley", "Stanley", "Sta Lee", "Sam Lee", "The great stan lee - and others whose name I don't know.", "stan lee??", "Stan Lee, Larry Lieber, Don Heck, and Jack Kirby", "Stan Lee and the artist Larry Lieber.", "stan lee and jack kirby", "Stan Lee & Jack Kirby", "Jack Kirby", "Iron Man was created by writer and editor Stan Lee, developed by scripter Larry Lieber, and designed by artists Don Heck and Jack Kirby."),
  "Gotham" = c("gotham", "Gotham", "gotham city", "Gotham City", "Gothan", "Batman protects Gotham City.", "Goyham", "Gothom", "Gothem", "Gotham.", "Ghotham City", "Arkham"),
  "Wonder Woman" = c("wonder woman", "Wonder Woman", "wonderwoman", "Wonderwomen", "wonder womn", "Wonder Women", "Wonder women", "Wonder Woman.", "Wonder Woman (Diane)", "Wonder Woma", "The superhero with a magic lasso and bullet-proof bracelets is Wonder Woman."),
  "A ring" = c("a ring", "His ring", "A power ring", "a lantern?", "A green ring", "A green lantern", "Green lantern?", "Green Lantern Power Ring", "Green lantern", "A ring.", "his pwer ring", "his power ring", "His lantern ring", "Green Power Ring", "Green Lattern ring", "Green lanterns", "The Green Lantern", "Rings", "Ring/latern", "ring or lantern", "Power Ring.", "Magic Ring", "latern", "lantern and ring", "Lantern and a ring", "The willpower ring", "The Power Ring", "The Green Ring","the Green Lantern Power Ring", "ring", "Ring", "power ring", "lantern ring", "Lantern", "A lantern", "The Green Lantern gains his power from the Green Lantern Power Ring."),
  "Paradise Island" = c("Paradise", "Island of Themyscira", "paradise island", "Paradise Island.", "Themiscyra", "Themiscira", "themiscaria", "Thymoscara", "Thermysceria", "Themyscyra", "themyscera", "Themyscarrae (sp?)", "Themyscara the amazon island", "thymiscira", "Thymiscera", "Thymeslcia? I don't know the proper spelling", "Theymiscira", "Paradise Island", "themyscira", "Themyscira", "Themysica", "Wonder Woman comes from the island of Themyscira", "Thymyscria (I don't know the spelling)"),
  "The Daily Bugle" = c("the daily bugle", "The Daily Bulge", "the Daily Bugle newspaper", "the daily bugel", "The Daily Beagle", "Spider-Man works for the Daily Bugle newspaper.", "Daily Bugle? Bugler?", "Daily Bugle newspaper", "Daily Bugal", "Daily Bugle", "daily bugle", "bugle", "works for the Daily Bugle newspaper"),
  "Dr. Doom" = c("dr. doom", "Victor Von Doom or Doctor Doom", "Dr. Victor Von Doom", "Dt doom", "Dr.Doom", "Dr.Doom", "dr.doom", "Victor Von Doom", "victor doom", "The archnemesis of the Fantastic Four is Doctor Doom (also known as Victor Von Doom).", "Dr. Doom", "doctor doom", "Doctor Doom", "doom", "Dr Doom"),
  "Malefic" = c("malefic", "Malefic", "black manta", "Black Manta", "Ocean master", "Ocean Master", "The archenemy of Aquaman is Black Manta.", "mantra", "Depending on the comic and the age, either Ocean Master or Black Manta", "Black Mantra", "Black Mantis", "Black Manta.", "black manat", "Balck manta"),
  "Doomsday" = c("doomsday", "Doomsday", "Villain Doomsday"),
  "Aqualad" = c("aqualad", "Aqualad", "Aqua lad", "Prince Garth Aqualad", "Prince Garth", "Garth, Aqualad", "Garth", "Gartg", "Aquaman's sidekick was Aqualad (also known as Garth or Tempest).", "Aqualad?"),
  "Pennyworth" = c("pennyworth", "Pennyworth", "Pennywort", "pennysworth", "penneyworth", "pannyworth", "Alfred Thaddeus Crane Pennyworth", "Alfred Pennyworth", "Alfred Pennyworthy", "The family name of Alfred, Batman's butler, is Pennyworth.")
  # Make sure to add variations based on the observed data and common misspellings or abbreviations.
)

# Function to assign score based on answer
assign_score <- function(id_question, answer) {
  # Normalize the answer to lowercase for comparison
  normalized_answer <- tolower(answer)
  
  # Find the correct answer list for the id_question
  correct_answer_variations <- correct_answers[[id_question]]
  
  # Check if the normalized answer is in the list of correct answers
  if (normalized_answer %in% tolower(correct_answer_variations)) {
    return(1) # Correct answer
  } else {
    return(0) # Incorrect or "I don't know"
  }
}

# Apply the function to create the score column
participant_answers_trivia_long <- participant_answers_trivia_long %>%
  rowwise() %>%
  mutate(score = assign_score(id_question, answer))

rm(correct_answers, assign_score)

#4. Data manipulation for nestedness analysis ####

# Create a dataframe with themes
question_numbers <- 1:45
themes <- c(rep("Solar System", 15), rep("American History", 15), rep("Super Heroes", 15))
theme_dataset <- data.frame(question = question_numbers, theme = themes)
rm(question_numbers, themes)

# Join dataset_for_h1bis with theme_dataset to add the theme information
d1 <- participant_answers_trivia_long %>%
  left_join(theme_dataset, by = c("id_question" = "question")) %>%
  select(-answer)

rm(theme_dataset)

dataset_nestedness_americanhistory <- d1 %>% filter(theme=="American History") %>% select(-theme)
dataset_nestedness_solarsystem <- d1 %>% filter(theme=="Solar System") %>% select(-theme)
dataset_nestedness_superheroes <- d1 %>% filter(theme=="Super Heroes") %>% select(-theme)

rm(d1)


# For dataset_americanhistory_nestedness
dataset_nestedness_americanhistory <- pivot_wider(dataset_nestedness_americanhistory,
                                             names_from = id_question,
                                             values_from = score,
                                             values_fill = list(score = 0),
                                             names_prefix = "Q") %>% select(-id_participant)

# For dataset_solarsystem_nestedness
dataset_nestedness_solarsystem <- pivot_wider(dataset_nestedness_solarsystem,
                                         names_from = id_question,
                                         values_from = score,
                                         values_fill = list(score = 0),
                                         names_prefix = "Q") %>% select(-id_participant)

# For dataset_superheroes_nestedness
dataset_nestedness_superheroes <- pivot_wider(dataset_nestedness_superheroes,
                                         names_from = id_question,
                                         values_from = score,
                                         values_fill = list(score = 0),
                                         names_prefix = "Q") %>% select(-id_participant)

#5. Testing H1 ##########

#5.1. Computing 2 measures of nestedness ######

#The two measures are nodf and temp
#High NODF = High nestedness
#High temp = LOW nestedness.

#Heroes
nestednodf(dataset_nestedness_superheroes)
nestedtemp(dataset_nestedness_superheroes)

#Solar
nestednodf(dataset_nestedness_solarsystem)
nestedtemp(dataset_nestedness_solarsystem)

#History
nestednodf(dataset_nestedness_americanhistory)
nestedtemp(dataset_nestedness_americanhistory)

#On NODF, nestedness is lower for History than for the other two 
#(which score roughly equal)
#On nestedness, History is slightly *more* nested.
#(arguably the discrepancy is due to the impact of matrix fill, which is lower for history)

#5.2. Building  simulated matrices: example #####

help(nullmodel)

#Stored parameter: 
#the number of simulated matrices
nsimul = 500

#setting the seed to reproduce the random bootstraps
set.seed(123)

#An example: generating a permutated history matrix using the r1 method:
null.history.r1 <- nullmodel(dataset_nestedness_americanhistory, 'r1')
history.r1.simulations <- simulate(null.history.r1, nsim=nsimul)
#printing a sample matrix, n°4:
matrix4 <- history.r1.simulations[,,4]
head(matrix4)

#5.3. Bootstrapping analysis #####

#Are nestedness measures higher / lower than the simulated baselines?
# Reminders: (1) NODF is positively correlated with nestedness, temp negatively correlated.
# (2) It is perfectly OK for a bootstrapped p value to be equal to exactly 0.

domains <- c("history", "heroes", "solar")

#Creating folder to put figures in:
dir.create(file.path("figures"))
           
for(i in domains){
  print(i)

  if(i == "history"){d <-   dataset_nestedness_americanhistory}
  if(i == "heroes"){d <-   dataset_nestedness_superheroes}
  if(i == "solar"){d <-   dataset_nestedness_solarsystem}
  
  print("this domain's nestedness (NODF) is:")
  real.nodf <-  round(nestednodf(d)$statistic['NODF'])
  print(real.nodf)
  print("this domain's nestedness (temp) is:")
  real.temp <-  nestedtemp(d)$statistic
  print(real.temp)
  
  #Simulation function
  null.r1 <- nullmodel(d, 'r1')
  r1.simulations <- simulate(null.r1, nsim=nsimul)

  #Creating empty vectors to be filled in by the simulations 
  r1.simulated.temperatures <- c()
  r1.simulated.nodfs <- c()
  
  #Running all simulations
  for (j in 1:nsimul){
    #print(i)
    R1Matrix <-   r1.simulations[,,j]
    r1.simulated.temperatures[j] <- nestedtemp(R1Matrix)$statistic
    r1.simulated.nodfs[j] <- nestednodf(R1Matrix)$statistic['NODF']
  }
  
  #Bootstrapped significance tests
  
  nodf.r1.p.value = sum(r1.simulated.nodfs  >= real.nodf) / length(r1.simulated.nodfs)
  nodf.r1.z.value = (real.nodf - mean(r1.simulated.nodfs)) / sd(r1.simulated.nodfs)
  print("the bootstrapped p value for NODF is:")
  print(nodf.r1.p.value)
  print("the test's z value for NODF is:")
  print(nodf.r1.z.value)
  
  temperature.r1.p.value = sum(r1.simulated.temperatures  <= real.temp) / length(r1.simulated.temperatures)
  temperature.r1.z.value = (real.temp - mean(r1.simulated.temperatures)) / sd(r1.simulated.temperatures)
  print("the bootstrapped p value for temp is:")
    print(temperature.r1.p.value)
    print("the test's z value for temp is:")
  print(temperature.r1.z.value)
  
  #Plotting
  
  plot.title <- paste0("figures/", as.character(i),".bootstrapping.png", sep="")
  png(file = plot.title)
  par(mfrow=c(1,2))
  
  #NODF 
  plot( density(r1.simulated.nodfs), xlim = c(min(r1.simulated.nodfs), as.numeric(real.nodf+1)), 
        main="NODF - R1", xlab = "r1.simulated.nodfs")
  abline(v=real.nodf, lwd=3, col='gold')
  
  #Temperature
  
  plot( density(r1.simulated.temperatures), xlim = c(as.numeric(real.temp-1), max(r1.simulated.temperatures)),
        main="Temperature - R1",xlab = "r1.simulated.temps")
  abline(v=real.temp, lwd=3, col='gold')
  
  dev.off()
  }


#6. Testing H1 prime #####

#6.1. Variable computation ####

# Compute the performance of participants

participant_objective_performance <- participant_answers_trivia_long %>%
  group_by(id_participant) %>%
  summarise(
    objective_performance_score = sum(score) / 15
  )

# Compute the difficulty scores of the questions

question_objective_difficulty_score <- participant_answers_trivia_long %>%
  group_by(id_question) %>%
  summarise(
    objective_difficulty_score = sum(score == 0) / n()
  )


test <- participant_answers_trivia_long %>% left_join(question_objective_difficulty_score) %>%
  left_join(participant_objective_performance)

test_2 <- test %>% filter(score==1) %>%
  group_by(id_participant) %>%
  summarise(objective_performance_score=mean(objective_performance_score),
            mean_rarity_questions=mean(objective_difficulty_score))

summary(lm(data=test_2, objective_performance_score~mean_rarity_questions))
ggplot(test_2, aes(x = mean_rarity_questions, y = objective_performance_score)) +
  geom_jitter(size = 3, alpha = 0.6, color = "blue") + # Scatter points
  geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "dashed") + # Add regression line with CI
  labs(
    title = "",
    x = "Average Actual Rarity (of the questions they answered)",
    y = "Actual Knwowledge"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )


# Computing the average objective performance score (by question)

  # Join datasets to associate answers with participant performance scores
answers_with_performance <- participant_answers_trivia_long %>%
  left_join(participant_objective_performance, by = c("id_participant" = "id_participant"))

  # Filter for correct answers
correct_answers_with_performance <- answers_with_performance %>%
  filter(score == 1)

  # Calculate average performance score for participants who got each question right
participant_average_objective_performance <- correct_answers_with_performance %>%
  group_by(id_question) %>%
  summarise(average_objective_performance_score = mean(objective_performance_score))

rm(answers_with_performance, correct_answers_with_performance)

# 6.2. H1 prime Test & figure####

# Join question_objective_difficulty_score with participant_average_objective_performance
dataset_for_h1bis <- question_objective_difficulty_score %>%
  left_join(participant_average_objective_performance, by = "id_question")

# Create a dataframe with themes
question_numbers <- 1:45
themes <- c(rep("Astronomy", 15), rep("American history", 15), rep("Superheroes", 15))
theme_dataset <- data.frame(question = question_numbers, theme = themes)
rm(question_numbers, themes)

# Join dataset_for_h1bis with theme_dataset to add the theme information
dataset_for_h1bis <- dataset_for_h1bis %>%
  left_join(theme_dataset, by = c("id_question" = "question"))

rm(theme_dataset)

# Null model
null_model_h1bis <- lmer(average_objective_performance_score ~ (1|theme) , data = dataset_for_h1bis)
summary(null_model_h1bis)
# Create the model
model_h1bis <- lmer(average_objective_performance_score ~ objective_difficulty_score + (1|theme) , data = dataset_for_h1bis)
summary(model_h1bis)
#without random intercept for participant, as preregistered (but it's the incorrect way)
model_h1ter <- lm(average_objective_performance_score ~ objective_difficulty_score, data = dataset_for_h1bis)
summary(model_h1ter)
#with a random slope for the effect of objective_difficulty_score as it varies by Theme:
model_h1_alt <- lmer(average_objective_performance_score ~ objective_difficulty_score + (1 + objective_difficulty_score | theme) , data = dataset_for_h1bis)
summary(model_h1_alt)

AIC(null_model_h1bis)
AIC(model_h1bis)
AIC(model_h1ter)
AIC(model_h1_alt)



# Adjusted Plotting with ggplot

my.plot <- ggplot(dataset_for_h1bis, aes(x = objective_difficulty_score, y = average_objective_performance_score, shape = theme, color=theme)) +
  geom_point(size=4) + # Plot points with increased size
  geom_smooth(aes(group = theme), method = "lm", se = FALSE, size=2) + # Add linear model line for each theme
  labs(x = "Actual Difficulty", 
       y = "Average Actual Knowledge", # Move subtitle to y-axis label
       title = "H1'") +
  theme_minimal() + # Use a minimal theme
  theme(legend.position = "none",
        plot.title = element_text(size = 18), # Increase title size
        plot.subtitle = element_text(size = 18), # Adjust subtitle size
        axis.title = element_text(size = 18), # Increase axis title size
        axis.text = element_text(size = 18), # Increase axis text size
        legend.title = element_blank(), # Remove legend title
        legend.text = element_text(size = 18)) + # Increase legend text size
  scale_color_brewer(palette = "Set2") + # Use color palette for themes
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) + # Set x-axis limits and breaks
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) # Set y-axis limits and breaks

my.plot

#Exporting
png(file = "figures/H1prime.test.png")
my.plot
dev.off()

# Summary of the linear model
summary(model_h1bis)

#7. Testing H2 ######

#7.1. Variable computation ####

# Select columns from rawdata

# Select id_participant and all columns that start with "vi" followed by any number and "_difficulty_1"
filtered_data <- rawdata %>%
  select(id_participant, starts_with("vi1_difficulty_"), starts_with("vi2_difficulty_"), 
         starts_with("vi3_difficulty_"), starts_with("vi4_difficulty_"), starts_with("vi5_difficulty_"),
         starts_with("vi6_difficulty_"), starts_with("vi7_difficulty_"), starts_with("vi8_difficulty_"),
         starts_with("vi9_difficulty_"), starts_with("vi10_difficulty_"), starts_with("vi11_difficulty_"),
         starts_with("vi12_difficulty_"), starts_with("vi13_difficulty_"), starts_with("vi14_difficulty_"),
         starts_with("vi15_difficulty_"), starts_with("vi16_difficulty_"), starts_with("vi17_difficulty_"),
         starts_with("vi18_difficulty_"), starts_with("vi19_difficulty_"), starts_with("vi20_difficulty_"),
         starts_with("vi21_difficulty_"), starts_with("vi22_difficulty_"), starts_with("vi23_difficulty_"),
         starts_with("vi24_difficulty_"), starts_with("vi25_difficulty_"), starts_with("vi26_difficulty_"),
         starts_with("vi27_difficulty_"), starts_with("vi28_difficulty_"), starts_with("vi29_difficulty_"),
         starts_with("vi30_difficulty_"), starts_with("vi31_difficulty_"), starts_with("vi32_difficulty_"),
         starts_with("vi33_difficulty_"), starts_with("vi34_difficulty_"), starts_with("vi35_difficulty_"),
         starts_with("vi36_difficulty_"), starts_with("vi37_difficulty_"), starts_with("vi38_difficulty_"),
         starts_with("vi39_difficulty_"), starts_with("vi40_difficulty_"), starts_with("vi41_difficulty_"),
         starts_with("vi42_difficulty_"), starts_with("vi43_difficulty_"), starts_with("vi44_difficulty_"),
         starts_with("vi45_difficulty_"))
         
# Pivot the dataset to a longer format
long_difficulty_data <- filtered_data %>%
  pivot_longer(
    cols = -id_participant, # Exclude id_participant from pivoting
    names_to = "question_evaluated", # Column to store the original column names
    values_to = "perceived_difficulty_score" # Column to store the values
  ) %>% drop_na(perceived_difficulty_score)

# Extract the number after "vi" and before "_difficulty_1"
participant_perceived_difficulty <- long_difficulty_data %>%
  mutate(
    question_evaluated = str_extract(question_evaluated, "(?<=vi)\\d+(?=_difficulty_1)")
  )

rm(long_difficulty_data, filtered_data)
participant_perceived_difficulty$question_evaluated <- as.integer(participant_perceived_difficulty$question_evaluated)
participant_perceived_difficulty$perceived_difficulty_score <- as.numeric(participant_perceived_difficulty$perceived_difficulty_score)


#7.2. H2 test & figure ####

# Joining dataset

dataset_for_h2 <- participant_perceived_difficulty %>%
  left_join(question_objective_difficulty_score, by = c("question_evaluated"="id_question"))

# Divide by 100 and invert the score to make it a difficulty score

dataset_for_h2 <- dataset_for_h2 %>% mutate(
  perceived_difficulty_score = 1-(perceived_difficulty_score/100)
)

# Create a dataframe with themes
question_numbers <- 1:45
themes <- c(rep("Astronomy", 15), rep("American history", 15), rep("Superheroes", 15))
theme_dataset <- data.frame(question = question_numbers, theme = themes)
rm(question_numbers, themes)

# Join dataset with theme_dataset to add the theme information
dataset_for_h2 <- dataset_for_h2 %>%
  left_join(theme_dataset, by = c("question_evaluated" = "question"))

rm(theme_dataset)

# Create the models
model_h2_null <- lmer(objective_difficulty_score ~ (1|id_participant), data = dataset_for_h2)
model_h2_test <- lmer(objective_difficulty_score ~ perceived_difficulty_score + (1|id_participant), data = dataset_for_h2)
summary(model_h2_test)

#Variant with TWO random interecepts
model_h2_test_bis <- lmer(objective_difficulty_score ~ perceived_difficulty_score + (1|id_participant) +  (1|theme), data = dataset_for_h2)
summary(model_h2_test_bis)

#Alternative: with random slope for effect of perceived_difficulty_score as it varies with id_participant:
model_h2_test_ter <- lmer(objective_difficulty_score ~ perceived_difficulty_score + (1 + perceived_difficulty_score |id_participant), data = dataset_for_h2)
summary(model_h2_test_ter)

#Maximal model
model_h2_test_ter <- lmer(objective_difficulty_score ~ perceived_difficulty_score + 
                            (1 + perceived_difficulty_score | id_participant) +
                            (1 + perceived_difficulty_score | theme), 
                          data = dataset_for_h2)
summary(model_h2_test_ter)

AIC(model_h2_null)
AIC(model_h2_test)
AIC(model_h2_test_bis)
AIC(model_h2_test_ter)


# Adjusted Plotting with ggplot
my.plot <- ggplot(dataset_for_h2, aes(x = perceived_difficulty_score, y = objective_difficulty_score, shape = theme, color=theme)) +
  geom_point(size=2, position = position_jitter(height = 0.2, width = 0), alpha = 0.1) + # Plot points with increased size
  geom_smooth(aes(group = theme), method = "lm", se = FALSE, size=2) + # Add linear model line for each theme
  labs(x = "Estimated Difficulty", 
       y = "Actual Difficulty", # Move subtitle to y-axis label
       title = "H2") +
  theme_minimal() + # Use a minimal theme
  theme(legend.position = "none",
        plot.title = element_text(size = 18), # Increase title size
        plot.subtitle = element_text(size = 18), # Adjust subtitle size
        axis.title = element_text(size = 18), # Increase axis title size
        axis.text = element_text(size = 18), # Increase axis text size
        legend.title = element_blank(), # Remove legend title
        legend.text = element_text(size = 18)) + # Increase legend text size
  scale_color_brewer(palette = "Set2") + # Use color palette for themes
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) + # Set x-axis limits and breaks
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) # Set y-axis limits and breaks

my.plot 

#Exporting
png(file = "figures/H2.test.png")
my.plot
dev.off()


# Summary of the linear model and comparison of the models
summary(model_h2_null)
summary(model_h2_test)

AIC(model_h2_null, model_h2_test)

compare_performance(model_h2_null, model_h2_test, rank=TRUE)

#8. Testing H3 #####

#8.1. Data manipulation for evaluation ####

# Compute the perceived performance score by participant and VI

participant_perceived_performance <- long_data %>%
  group_by(id_participant, question_virtual_individual) %>%
  summarise(
    number_right_responses = sum(evaluation == 1), # Count of 1s in evaluation
    total = n(), # Total number of evaluations
    perceived_performance_score = number_right_responses / total # Ratio of right responses to total
  )

# Compute the average perceived performance score

participant_average_perceived_performance <- participant_perceived_performance %>%
  group_by(question_virtual_individual) %>%
  summarise(
    average_perceived_performance_score = mean(perceived_performance_score, na.rm = TRUE) # Calculate the mean
  )

#8.2. H3 test & plot ####

dataset_for_h3 <- participant_average_objective_performance %>%
  left_join(participant_average_perceived_performance, by = c("id_question"="question_virtual_individual"))

# Create a dataframe with themes
question_numbers <- 1:45
themes <- c(rep("Astronomy", 15), rep("American history", 15), rep("Superheroes", 15))
theme_dataset <- data.frame(question = question_numbers, theme = themes)
rm(question_numbers, themes)

# Join dataset with theme_dataset to add the theme information
dataset_for_h3 <- dataset_for_h3 %>%
  left_join(theme_dataset, by = c("id_question" = "question"))

rm(theme_dataset)

# Create the models
model_h3_null <- lmer(average_objective_performance_score ~ (1|theme), data = dataset_for_h3)
AIC(model_h3_null)
model_h3_test <- lmer(average_objective_performance_score ~ average_perceived_performance_score + (1|theme), data = dataset_for_h3)
summary(model_h3_test)
AIC(model_h3_test)

#Same with random slope for the effect of average_perceived_performance_score as it varies by theme:
model_h3_test_bis <- lmer(average_objective_performance_score ~ average_perceived_performance_score + (1 + average_perceived_performance_score|theme), data = dataset_for_h3)
summary(model_h3_test_bis)
AIC(model_h3_test_bis)


# Export correlation coefficient

  # Shapiro-Wilk normality tests for each variable
normality_test_objective <- shapiro.test(dataset_for_h3$average_objective_performance_score)
normality_test_perceived <- shapiro.test(dataset_for_h3$average_perceived_performance_score)

  # Print the results of the normality tests
print(normality_test_objective)
print(normality_test_perceived)

hist(dataset_for_h3$average_objective_performance_score)
hist(dataset_for_h3$average_perceived_performance_score)

  # Print the correlatio coefficient
correlation_coefficient <- cor(dataset_for_h3$average_objective_performance_score, dataset_for_h3$average_perceived_performance_score, method = "pearson")

# Adjusted Plotting with ggplot
my.plot <- ggplot(dataset_for_h3, aes(x = average_perceived_performance_score, y = average_objective_performance_score, shape = theme, color=theme)) +
  geom_point(size=4) + # Plot points with increased size
  geom_smooth(aes(group = theme), method = "lm", se = FALSE, size=2) + # Add linear model line for each theme
  labs(x = "Average Estimated Knowledge", 
       y = "Average Actual Knowledge", # Move subtitle to y-axis label
       title = "H3") +
  theme_minimal() + # Use a minimal theme
  theme(legend.position = "none",
        plot.title = element_text(size = 18), # Increase title size
        plot.subtitle = element_text(size = 12), # Adjust subtitle size
        axis.title = element_text(size = 18), # Increase axis title size
        axis.text = element_text(size = 18), # Increase axis text size
        legend.title = element_blank(), # Remove legend title
        legend.text = element_text(size = 18)) + # Increase legend text size
  scale_color_brewer(palette = "Set2") + # Use color palette for themes
  scale_x_continuous(limits = c(0.4, .85), breaks = seq(0.4, .9, by = 0.2)) + # Set x-axis limits and breaks
  scale_y_continuous(limits = c(0.4, .85), breaks = seq(0.4, .9, by = 0.2)) # Set y-axis limits and breaks

my.plot

#Exporting
png(file = "figures/H3.test.png")
my.plot
dev.off()


# Summary of the linear model and comparison of the models
summary(model_h3_null)
summary(model_h3_test)

AIC(model_h3_null, model_h3_test)

compare_performance(model_h3_null, model_h3_test, rank=TRUE)

#8.3 Testing H3p ####

dataset_for_h3p <- participant_perceived_performance %>%
  left_join(participant_average_objective_performance, by = c("question_virtual_individual"="id_question"))

# Create a dataframe with themes
question_numbers <- 1:45
themes <- c(rep("Astronomy", 15), rep("American history", 15), rep("Superheroes", 15))
theme_dataset <- data.frame(question = question_numbers, theme = themes)
rm(question_numbers, themes)

# Join dataset with theme_dataset to add the theme information
dataset_for_h3p <- dataset_for_h3p %>%
  left_join(theme_dataset, by = c("question_virtual_individual" = "question"))

rm(theme_dataset)

# Create the models
model_h3p_null <- lmer(average_objective_performance_score ~ (1|id_participant), data = dataset_for_h3p)
AIC(model_h3p_null)
model_h3p_test <- lmer(average_objective_performance_score ~ perceived_performance_score + (1|id_participant), data = dataset_for_h3p)
summary(model_h3p_test)
AIC(model_h3p_test)
#Variant: with random intercept for theme
model_h3p_test_bis <- lmer(average_objective_performance_score ~ perceived_performance_score + (1|id_participant)+ (1|theme), data = dataset_for_h3p)
summary(model_h3p_test_bis)
AIC(model_h3p_test_bis)

#Variant: adding a random slope for the effect of perceived_performance_score as it varies by id_participant
model_h3p_test_bis <- lmer(average_objective_performance_score ~ perceived_performance_score + (1+ perceived_performance_score |id_participant), data = dataset_for_h3p)
summary(model_h3p_test_bis)
AIC(model_h3p_test_bis)

#Variant: Maximal structure
model_h3p_test_max <- lmer(average_objective_performance_score ~ perceived_performance_score + (1+ perceived_performance_score |theme/id_participant), data = dataset_for_h3p)

# Adjusted Plotting with ggplot
my.plot <- ggplot(dataset_for_h3p, aes(x = perceived_performance_score, y = average_objective_performance_score, shape = theme, color=theme)) +
  geom_point(size=2, position = position_jitter(height = 0, width = 0.3), alpha = 0.1) + # Plot points with increased size
  geom_smooth(aes(group = theme), method = "lm", se = FALSE, size=2) + # Add linear model line for each theme
  labs(x = "Estimated Knowledge", 
       y = "Average Actual Knowledge", # Move subtitle to y-axis label
       title = "H3p") +
  theme_minimal() + # Use a minimal theme
  theme(legend.position = "none",
        plot.title = element_text(size = 18), # Increase title size
        plot.subtitle = element_text(size = 13), # Adjust subtitle size
        axis.title = element_text(size = 18), # Increase axis title size
        axis.text = element_text(size = 18), # Increase axis text size
        legend.title = element_blank(), # Remove legend title
        legend.text = element_text(size = 18)) + # Increase legend text size
  scale_color_brewer(palette = "Set2") 

my.plot

#Exporting
png(file = "figures/H3p.test.png")
my.plot
dev.off()


# Summary of the linear model and comparison of the models
summary(model_h3p_null)
summary(model_h3p_test)

AIC(model_h3p_null, model_h3p_test)

compare_performance(model_h3p_null, model_h3p_test, rank=TRUE)

#Intuitive measure

dataset_for_intuition <- dataset_for_h3p %>%
  mutate(actual_knowledge = average_objective_performance_score*15,
         estimated_knowledge = perceived_performance_score*15,
         gap=abs(actual_knowledge-estimated_knowledge))

percentage_below_4 <- sum(dataset_for_intuition$gap < 2) / nrow(dataset_for_intuition) * 100

dataset_for_intuition_2 <- dataset_for_h3m %>%
  mutate(actual_knowledge = average_objective_performance_score*15,
         estimated_knowledge = perceived_performance_score*15,
         gap=abs(actual_knowledge-estimated_knowledge))

percentage_below_4_2 <- sum(dataset_for_intuition_2$gap < 2) / nrow(dataset_for_intuition_2) * 100

#9. Isolate the bottom 30% participants per theme ####

# Add themes to participant_objective_performance dataset

  # Get theme
participant_theme <- dataset_for_h3p %>% select(id_participant, theme) %>% unique()

  # Join the datasets
participant_objective_performance <- participant_objective_performance %>%
  left_join(participant_theme)

rm(participant_theme)

# Separate the dataset by theme
superheroes <- participant_objective_performance %>% 
  filter(theme == "Superheroes")

american_history <- participant_objective_performance %>% 
  filter(theme == "American history")

solar_system <- participant_objective_performance %>% 
  filter(theme == "Astronomy")

# Keep only the 30% of the participants with the lowest objective_performance_score for each theme
superheroes_bottom_30 <- superheroes %>% 
  arrange(objective_performance_score) %>% 
  top_n(-0.3*n(), objective_performance_score)

american_history_bottom_30 <- american_history %>% 
  arrange(objective_performance_score) %>% 
  top_n(-0.3*n(), objective_performance_score)

solar_system_bottom_30 <- solar_system %>% 
  arrange(objective_performance_score) %>% 
  top_n(-0.3*n(), objective_performance_score)

# Combine the datasets
participant_bottom_30 <- bind_rows(
  superheroes_bottom_30,
  american_history_bottom_30,
  solar_system_bottom_30
)

rm(superheroes, american_history, solar_system, superheroes_bottom_30, american_history_bottom_30, solar_system_bottom_30)

# Calculate the average performance score for the bottom 30% participants by theme
participant_average_performance_score_bottom_30 <- participant_bottom_30 %>% 
  group_by(theme) %>% 
  summarise(
    average_performance_score = mean(objective_performance_score, na.rm = TRUE)
  )

# Extract the id_participant of all participants in the bottom 30%
participant_ids_bottom_30 <- participant_bottom_30 %>% 
  select(id_participant) %>% 
  distinct()

#10. Testing H2m ####

# Filter dataset_for_h3p to include only rows with id_participant in participant_ids_bottom_30
dataset_for_h2m <- dataset_for_h2 %>%
  filter(id_participant %in% participant_ids_bottom_30$id_participant)

# Create the models
model_h2m_null <- lmer(objective_difficulty_score ~ (1|id_participant), data = dataset_for_h2m)
AIC(model_h2m_null)
model_h2m_test <- lmer(objective_difficulty_score ~ perceived_difficulty_score + (1|id_participant), data = dataset_for_h2m)
summary(model_h2m_test)
AIC(model_h2m_test)

# Variant: adding random intercept for theme
model_h2m_test_bis <- lmer(objective_difficulty_score ~ perceived_difficulty_score + (1|id_participant) + (1|theme), data = dataset_for_h2m)
summary(model_h2m_test_bis)
AIC(model_h2m_test_bis)

#Variant: adding random slope for effect of perceived_difficulty_score as it varies by id_participant.
model_h2m_test_bis <- lmer(objective_difficulty_score ~ perceived_difficulty_score + (1 + perceived_difficulty_score|id_participant), data = dataset_for_h2m)
summary(model_h2m_test_bis)
AIC(model_h2m_test_bis)

# Adjusted Plotting with ggplot
my.plot <- ggplot(dataset_for_h2m, aes(x = perceived_difficulty_score, y = objective_difficulty_score, shape = theme, color=theme)) +
  geom_point(size=2, position = position_jitter(height = 0.2, width = 0), alpha = 0.1) + # Plot points with increased size
  geom_smooth(aes(group = theme), method = "lm", se = FALSE, size=2) + # Add linear model line for each theme
  labs(x = "Estimated Difficutly", 
       y = "Actual Difficulty", # Move subtitle to y-axis label
       title = "H2m") +
  theme_minimal() + # Use a minimal theme
  theme(legend.position = "none",plot.title = element_text(size = 18), # Increase title size
        plot.subtitle = element_text(size = 18), # Adjust subtitle size
        axis.title = element_text(size = 18), # Increase axis title size
        axis.text = element_text(size = 18), # Increase axis text size
        legend.title = element_blank(), # Remove legend title
        legend.text = element_text(size = 18)) + # Increase legend text size
  scale_color_brewer(palette = "Set2") + # Use color palette for themes
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) + # Set x-axis limits and breaks
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) # Set y-axis limits and breaks

my.plot

#Exporting
png(file = "figures/H2m.test.png")
my.plot
dev.off()

# Summary of the linear model and comparison of the models
summary(model_h2m_null)
summary(model_h2m_test)

AIC(model_h2m_null, model_h2m_test)
compare_performance(model_h2m_null, model_h2m_test, rank=TRUE)

#11. Testing H3m ####

# Filter dataset_for_h3p to include only rows with id_participant in participant_ids_bottom_30
dataset_for_h3m <- dataset_for_h3p %>%
  filter(id_participant %in% participant_ids_bottom_30$id_participant)

dataset_for_h3m <- dataset_for_h3m %>%
  group_by(question_virtual_individual) %>%
  summarise(average_perceived_performance_score=mean(perceived_performance_score),
            average_objective_performance_score=mean(average_objective_performance_score))

# Create a dataframe with themes
question_numbers <- 1:45
themes <- c(rep("Astronomy", 15), rep("American history", 15), rep("Superheroes", 15))
theme_dataset <- data.frame(question = question_numbers, theme = themes)
rm(question_numbers, themes)

# Join dataset with theme_dataset to add the theme information
dataset_for_h3m <- dataset_for_h3m %>%
  left_join(theme_dataset, by = c("question_virtual_individual" = "question"))

rm(theme_dataset)

# Create the models 
# Removed the VI target questions as random variable
model_h3m_null <- lmer(average_objective_performance_score ~ (1|theme), data = dataset_for_h3m)
model_h3m_test <- lmer(average_objective_performance_score ~ average_perceived_performance_score + (1|theme), data = dataset_for_h3m)

#Variant: adding random slope for effect of average_perceived_performance_score as it varies by id_participant.
model_h3m_test_bis <- lmer(average_objective_performance_score ~ average_perceived_performance_score + (1 + average_perceived_performance_score|theme), data = dataset_for_h3m)
summary(model_h3m_test_bis)
AIC(model_h3m_test_bis)

# Adjusted Plotting with ggplot
my.plot <- ggplot(dataset_for_h3m, aes(x = average_perceived_performance_score, y = average_objective_performance_score, shape = theme, color=theme)) +
  geom_point(size=4) + # Plot points with increased size
  geom_smooth(aes(group = theme), method = "lm", se = FALSE, size=2) + # Add linear model line for each theme
  labs(x = "Average Estimated Knowledge", 
       y = "Average Actual Knowledge", # Move subtitle to y-axis label
       title = "H3m") +
  theme_minimal() + # Use a minimal theme
  theme(legend.position = "none",
        plot.title = element_text(size = 18), # Increase title size
        plot.subtitle = element_text(size = 12), # Adjust subtitle size
        axis.title = element_text(size = 18), # Increase axis title size
        axis.text = element_text(size = 18), # Increase axis text size
        legend.title = element_blank(), # Remove legend title
        legend.text = element_text(size = 18)) + # Increase legend text size
  scale_color_brewer(palette = "Set2") + # Use color palette for themes
  scale_x_continuous(limits = c(0.4, .85), breaks = seq(0.4, .9, by = 0.2)) + # Set x-axis limits and breaks
  scale_y_continuous(limits = c(0.4, .85), breaks = seq(0.4, .9, by = 0.2)) # Set y-axis limits and breaks

my.plot

#Exporting
png(file = "figures/H3m.test.png")
my.plot
dev.off()

# Summary of the linear model and comparison of the models
summary(model_h3m_null)
summary(model_h3m_test)

AIC(model_h3m_null, model_h3m_test)
compare_performance(model_h3m_null, model_h3m_test, rank=TRUE)

#11. Testing H3mp ####

# Filter dataset_for_h3p to include only rows with id_participant in participant_ids_bottom_30
dataset_for_h3mp <- dataset_for_h3p %>%
  filter(id_participant %in% participant_ids_bottom_30$id_participant)

# Create the models 
# Removed the VI target questions as random variable
model_h3mp_null <- lmer(average_objective_performance_score ~ (1|id_participant), data = dataset_for_h3m)
model_h3mp_test <- lmer(average_objective_performance_score ~ perceived_performance_score + (1|id_participant), data = dataset_for_h3m)

# Adjusted Plotting with ggplot
my.plot <- ggplot(dataset_for_h3mp, aes(x = perceived_performance_score, y = average_objective_performance_score, shape = theme, color=theme)) +
  geom_point(size=2, position = position_jitter(height = 0, width = 0.2), alpha = 0.1) + # Plot points with increased size
  geom_smooth(aes(group = theme), method = "lm", se = FALSE, size=2) + # Add linear model line for each theme
  labs(x = "Estimated Knowledge", 
       y = "Average Actual Knowledge", # Move subtitle to y-axis label
       title = "H3m") +
  theme_minimal() + # Use a minimal theme
  theme(legend.position = "none",plot.title = element_text(size = 18), # Increase title size
        plot.subtitle = element_text(size = 12), # Adjust subtitle size
        axis.title = element_text(size = 18), # Increase axis title size
        axis.text = element_text(size = 18), # Increase axis text size
        legend.title = element_blank(), # Remove legend title
        legend.text = element_text(size = 18)) + # Increase legend text size
  scale_color_brewer(palette = "Set2") + # Use color palette for themes
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) + # Set x-axis limits and breaks
  scale_y_continuous(limits = c(0.4, 0.8), breaks = seq(0, 1, by = 0.2)) # Set y-axis limits and breaks

my.plot

#Exporting
png(file = "figures/H3mp.test.png")
my.plot
dev.off()

# Summary of the linear model and comparison of the models
summary(model_h3mp_null)
summary(model_h3mp_test)

AIC(model_h3mp_null, model_h3mp_test)
compare_performance(model_h3mp_null, model_h3mp_test, rank=TRUE)
