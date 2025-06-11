# ======== Netflix and Movielens data cleaning ========

## 1. library import ====
library(tidyverse)
library(reshape2)

## 2. data selection ====
genres <- c("action", "adventure", "animation", "comedy", "crime", 
            "documentary", "drama", "fantasy", "horror", "noir", 
            "romcom", "scifi", "thriller", "war", "western")

## 3. matrix transformation ====
# Function to create binary matrix
create_binary_matrix <- function(data) {
  # Get unique users and movies
  users <- unique(data$userId)
  movies <- unique(data$movieId)
  
  # Create empty matrix
  mat <- matrix(0, nrow = length(users), ncol = length(movies),
                dimnames = list(users, movies))
  
  # Fill matrix with 1s where ratings exist
  for (i in 1:nrow(data)) {
    user <- as.character(data$userId[i])
    movie <- as.character(data$movieId[i])
    mat[user, movie] <- 1
  }
  
  return(mat)
}

## 4. General matrix ====
all_ratings <- read.csv("ratings.csv", header = TRUE, sep = ",")
all_matrix <- create_binary_matrix(all_ratings)
write.csv(all_matrix, "general_bin.csv", row.names = FALSE)

## 5. Genre-specific matrices ====
for (genre in genres) {
  filename <- paste0("ratings_", genre, ".csv")
  genre_data <- read.csv(filename, header = TRUE, sep = ",")
  genre_matrix <- create_binary_matrix(genre_data)
  write.csv(genre_matrix, paste0("bin_", genre, ".csv"), row.names = FALSE)
}
