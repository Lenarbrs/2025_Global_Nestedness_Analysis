# ======== Rapidity estimation ========

## ==== 1. library import ====
library(tidyverse)
library(progress)
library(ggplot2)
library(dplyr)
library(vegan)
library(permute)
library(lattice)

## Parameters ----
# set parallel options to the computer's number of cores minus 1
options(mc.cores = max(1, parallel::detectCores() - 1))
# Number of simulations
N_ITER_ <- 1000

## ==== 2. Function for Global analysis ====
### ---- A. Compute correlation ----
compute_cor_coef <- function(matrix) {
  # Compute the total number of items each row (agent) holds
  row_totals <- rowSums(matrix)
  # For each item (i.e. each column), 
  # calculate the average inventory size of agents who hold it
  avg_inventory <- apply(matrix, 2, function(item_col) {
    # Select the row totals only for agents who have this item 
    # (i.e. where item_col == 1)
    selected <- row_totals[item_col == 1]
    # If no agent has the item, return NA to avoid mean(numeric(0)) = NaN
    if (length(selected) == 0) {
      return(NA_real_)
    } else {
      # Otherwise, return the average inventory size of agents who have the item
      return(mean(selected))
    }} )
  # Create a data frame with two statistics per item:
  # - Prevalence: how many agents have the item (column sum)
  # - AvgInventory: average inventory size of those agents
  item_stats <- data.frame(
    Prevalence = colSums(matrix),
    AvgInventory = avg_inventory
  )
  # If any NA exists in Prevalence or AvgInventory, 
  # correlation can't be computed
  if (anyNA(item_stats)) {
    return(NA_real_)
  }
  # Compute the standard deviations to check for constant vectors
  sd_prev <- sd(item_stats$Prevalence, na.rm = TRUE)
  sd_avginv <- sd(item_stats$AvgInventory, na.rm = TRUE)
  # If either standard deviation is NA (shouldn’t happen now) or equal to 0, 
  # correlation is undefined
  if (is.na(sd_prev) || is.na(sd_avginv) || sd_prev == 0 || sd_avginv == 0) {
    return(NA_real_)
  }
  # Return the Pearson correlation between Prevalence and AvgInventory
  return(cor(item_stats$Prevalence, item_stats$AvgInventory))
}


## ==== 3. Nestedness analysis ====
nestedness_analysis <- function(matrix, matrix_id, N_ITER_) {
  
  ### A. Calculate real matrix nestedness properties ----
  # Size and Fill
  num_elements <- nrow(matrix)*ncol(matrix)
  num_ones <- sum(matrix == 1)
  fill_percentage <- (num_ones / num_elements) * 100
  # Coefficient of correlation
  cor_coef <- compute_cor_coef(matrix)
  # Nestedness scores
  temp_real_matrix <- nestedtemp(matrix)
  nodf_real_matrix <- nestednodf(matrix,
                                 order = TRUE, 
                                 weighted = FALSE, 
                                 wbinary = FALSE)
  # Extract statistics
  temp_stat <- as.numeric(temp_real_matrix$statistic)
  nodf_col_stat <- as.numeric(nodf_real_matrix$statistic[1])
  nodf_row_stat <- as.numeric(nodf_real_matrix$statistic[2])
  nodf_gen_stat <- as.numeric(nodf_real_matrix$statistic[3])
  
  ### B. Summary dataset ----
  df_summary <- data.frame(
    matrix_id = matrix_id,
    num_rows = nrow(matrix),
    num_columns = ncol(matrix),
    size = num_elements,
    fill = fill_percentage,
    cor_coef = cor_coef,
    nodf_columns_stat = nodf_col_stat,
    nodf_rows_stat = nodf_row_stat,
    nodf_general_stat = nodf_gen_stat,
    temp_stat = temp_stat,
    stringsAsFactors = FALSE
  )
  # write.csv2(df_summary, paste0("nest_summary_", matrix_id, ".csv"), 
  #            row.names = FALSE)
  
  ### C. Parameters list ----
  baselines <- c('r00', 'r0', 'r1', 'r2','c0','c1','curveball', 'swap')
  
  ### D. Initialize simulated matrices dataset ----
  df_simulated <- data.frame(
    matrix_id = character(),
    baseline = character(),
    ceof_cor = numeric(),
    nodf_columns_stat = numeric(),
    nodf_rows_stat = numeric(),
    nodf_general_stat = numeric(),
    temp_stat = numeric(),
    stringsAsFactors = FALSE)
  
  ### E. Simulated matrices dataset ----
  for (b in baselines) {
    # For sanity saves purpose
    df_simulated_b <- data.frame(
      matrix_id = character(),
      baseline = character(),
      ceof_cor = numeric(),
      nodf_columns_stat = numeric(),
      nodf_rows_stat = numeric(),
      nodf_general_stat = numeric(),
      temp_stat = numeric(),
      stringsAsFactors = FALSE)
    # # Directory for simulated matrices
    # dir.create(paste0("simmat_",matrix_id, "_", b))
    # Choose the right matrix and baseline
    current_matrix <- matrix
    baseline_used <- b
    # c1 special treatment
    if (b == 'c1') {
      # Inverse the matrix
      current_matrix <- t(matrix)
      # Use r1
      baseline_used <- 'r1'
    }
    # Simulate matrices
    nullmodel_mat <- nullmodel(x = current_matrix, 
                               method = baseline_used)
    simulated_mat <- simulate(object = nullmodel_mat, 
                              nsim = N_ITER_)
    # We keep the same 1,000 matrices per baseline for temp and nodf
    for (i in 1:dim(simulated_mat)[3]) {
      sim_i <- simulated_mat[, , i]
      
      ### F. Calculate simulated matrices nestedness properties ----
      # Coefficient of correlation
      cor_coef_sim <- compute_cor_coef(sim_i)
      # Nestedness scores
      temp_sim_matrix <- nestedtemp(sim_i)
      nodf_sim_matrix <- nestednodf(sim_i, 
                                    order = TRUE, 
                                    weighted = FALSE, 
                                    wbinary = FALSE)
      # Extract statistics
      temp_stat <- as.numeric(temp_sim_matrix$statistic)
      nodf_col_stat <- as.numeric(nodf_sim_matrix$statistic[1])
      nodf_row_stat <- as.numeric(nodf_sim_matrix$statistic[2])
      nodf_gen_stat <- as.numeric(nodf_sim_matrix$statistic[3])
      
      ### G. Unite results and append dataframes ----
      row_sim <- data.frame(
        matrix_id = matrix_id,
        baseline = b,
        ceof_cor = cor_coef_sim,
        nodf_columns_stat = nodf_col_stat,
        nodf_rows_stat = nodf_row_stat,
        nodf_general_stat = nodf_gen_stat,
        temp_stat = temp_stat,
        stringsAsFactors = FALSE)
      # append results
      df_simulated <- rbind(df_simulated, row_sim)
      df_simulated_b <- rbind(df_simulated_b, row_sim)
      
      # # Save simulated matrix
      # mat_directory <- paste0("simmat_", 
      #                         matrix_id, "_", b, 
      #                         "/simmat_", matrix_id, 
      #                         "_", b, "_", i, ".csv")
      # write.csv2(sim_i, mat_directory)
    }
    # Save nestedness results of simulated matrices for 1 baseline
    # write.csv2(df_simulated_b, paste0("nest_simulated_", 
    #                                   matrix_id, "_", b, ".csv"), 
    #            row.names = FALSE)
  }
  
  ### H. Save nestedness results of simulated matrices for all baselines ----
  # write.csv2(df_simulated, paste0("nest_simulated_", 
  #                                 matrix_id, "_all.csv"), 
  #            row.names = FALSE)
}


## ==== 3. Matrices simulation ====

### A.  Define matrix sizes ----
# These sizes are more or less the same as the matrices we have in our data
matrix_sizes <- list(
  c(4, 30), c(12, 10), c(5, 40), c(40, 30), c(40, 180)
  c(35, 50), c(100,50), c(70, 80), c(40, 140),
  c(60,50), c(90,40), c(30,220), c(170, 40), c(30, 350)
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
  nestedness_analysis(mat, matrix_id = paste0(n_row, "x", n_col), N_ITER_)
  elapsed <- as.numeric(Sys.time() - start_time, 
                        units = "secs")
  # Store results
  results <- rbind(results, data.frame(
    n_row = n_row,
    n_col = n_col,
    size = n_row * n_col,
    fill = actual_fill,
    time = elapsed
  ))
}
write.csv2(results, "rapidity_results.csv")

## ==== 5. Visualisation ====
p <- ggplot(results, aes(x = size, y = time)) +
  geom_point(aes(size = fill), color = "darkblue", alpha = 0.8) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, color = "red") +
  labs(title = "Execution time vs Matrix size",
       subtitle = "Linear regression with 95% confidence interval",
       x = "Matrix size (n_row × n_col)",
       y = "Execution time (seconds)",
       size = "Fill Percentage") +
  scale_size_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
p
ggsave("execution_time_vs_matrix_size.png", plot = p, width = 8, height = 6, dpi = 300)
