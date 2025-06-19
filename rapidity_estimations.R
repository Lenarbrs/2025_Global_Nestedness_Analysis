# ======== Rapidity estimation ========

## ==== 1. library import ====
library(tidyverse)
library(progress)
library(ggplot2)
library(dplyr)
library(vegan)
library(permute)
library(lattice)

# set parallel options to the computer's number of cores minus 1
options(mc.cores = max(1, parallel::detectCores() - 1))

## ==== 2. Function for Global analysis ====
# Assuming nestedness_analysis function is defined elsewhere
# For testing purposes, we'll create a dummy version if it doesn't exist
if (!exists("nestedness_analysis")) {
  nestedness_analysis <- function(mat, matrix_id, n_iter = 1000) {
    # Simulate computation time proportional to matrix size
    Sys.sleep(prod(dim(mat)) / 500000)
    return(list())
  }
}

## ==== 3. Matrices simulation ====
### A.  Define matrix sizes ----
# (they resemble matrcies from our data)
matrix_sizes <- list(
  c(4, 30), c(12, 10), c(5, 40), c(8, 30),
  c(6, 50), c(10, 30), c(4, 90), c(7, 50),
  c(5, 130), c(40, 20), c(15, 70), c(20, 60)
)

### B.  Generate random binary matrix with target fill ----
# 30% is the mean fill in our real data
# So we aim for a mean fill of 0.3 for the simulated too
generate_matrix <- function(n_row, n_col, target_fill = 0.3) {
  repeat {
    # Create matrix with approximate target fill
    mat <- matrix(rbinom(n_row * n_col, 1, target_fill), 
                  nrow = n_row, ncol = n_col)
    # Ensure matrix isn't empty (0% fill)
    actual_fill <- mean(mat)
    if (actual_fill > 0) break
  }
  return(list(matrix = mat, fill = actual_fill))
}

### C. Initialize results dataframe ----
results <- data.frame(
  n_row = integer(),
  n_col = integer(),
  size = integer(),
  fill = numeric(),
  time = numeric()
)

### D. Timestamped function call ----
for (size in matrix_sizes) {
  n_row <- size[1]
  n_col <- size[2]
  
  # Generate matrix
  mat_data <- generate_matrix(n_row, n_col)
  mat <- mat_data$matrix
  actual_fill <- mat_data$fill
  
  # Time the function execution
  start_time <- Sys.time()
  nestedness_analysis(mat, matrix_id = paste0(n_row, "x", n_col), n_iter = 1000)
  elapsed <- as.numeric(Sys.time() - start_time, units = "secs")
  
  # Store results
  results <- rbind(results, data.frame(
    n_row = n_row,
    n_col = n_col,
    size = n_row * n_col,
    fill = actual_fill,
    time = elapsed
  ))
}

## ==== 5. Visualisation ====
ggplot(results, aes(x = size, y = time)) +
  geom_point(aes(size = fill), color = "dodgerblue", alpha = 0.8) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, color = "red") +
  labs(title = "Execution Time vs. Matrix Size",
       subtitle = "Linear regression with 95% confidence interval",
       x = "Matrix Size (n_row × n_col)",
       y = "Execution Time (seconds)",
       size = "Fill Percentage") +
  scale_size_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

