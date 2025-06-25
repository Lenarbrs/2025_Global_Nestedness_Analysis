#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Re analysis of nestedness in empirical matrices ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## ==== 1. Library import ====
library(tidyverse)
library(progress)
library(vegan)
library(permute)
library(lattice)

### Parameters ----
# set parallel options to the computer's number of cores minus 1
options(mc.cores = max(1, parallel::detectCores() - 1))
# Number of simulations
N_ITER_ <- 10


## ==== 2. Functions ====
### ---- A. Compute correlation ----
compute_cor_coef <- function(matrix) {
  row_totals <- rowSums(matrix)
  
  avg_inventory <- apply(matrix, 2, function(item_col) {
    selected <- row_totals[item_col == 1]
    if (length(selected) == 0) {
      return(NA_real_)  # Avoid NaN by returning explicit NA
    } else {
      return(mean(selected))
    }
  })
  
  item_stats <- data.frame(
    Prevalence = colSums(matrix),
    AvgInventory = avg_inventory
  )
  
  if (anyNA(item_stats)) {
    return(NA_real_)  # If any NA, correlation can't be computed
  }
  
  sd_prev <- sd(item_stats$Prevalence, na.rm = TRUE)
  sd_avginv <- sd(item_stats$AvgInventory, na.rm = TRUE)
  
  if (is.na(sd_prev) || is.na(sd_avginv) || sd_prev == 0 || sd_avginv == 0) {
    return(NA_real_)
  }
  
  return(cor(item_stats$Prevalence, item_stats$AvgInventory))
}




## ==== 3. Nestedness analysis ====
nestedness_analysis <- function(matrix, matrix_id, N_ITER_) {
  
  ### A. Calculate real matrix nestedness properties ----
  # Coefficient of correlation
  cor_coef <- compute_cor_coef(matrix)
  # Nestedness scores
  temp_real_matrix <- nestedtemp(matrix)
  nodf_real_matrix <- nestednodf(matrix, order = TRUE, weighted = FALSE, wbinary = FALSE)
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
    cor_coef = cor_coef,
    nodf_columns_stat = nodf_col_stat,
    nodf_rows_stat = nodf_row_stat,
    nodf_general_stat = nodf_gen_stat,
    temp_stat = temp_stat,
    stringsAsFactors = FALSE
  )
  write.csv2(df_summary, paste0("summary_", matrix_id, ".csv"), row.names = FALSE)
  
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
    
    current_matrix <- matrix
    baseline_used <- b
    # c1 special treatment
    if (b == 'c1') {
      current_matrix <- t(matrix)
      baseline_used <- 'r1'
      }
    # Simulate matrices
    nullmodel_mat <- nullmodel(x = current_matrix, method = baseline_used)
    simulated_mat <- simulate(object = nullmodel_mat, nsim = N_ITER_)
    
    for (i in 1:dim(simulated_mat)[3]) {
      sim_i <- simulated_mat[, , i]
      
      ### F. Calculate simulated matrices nestedness properties ----
      # Coefficient of correlation
      cor_coef_sim <- compute_cor_coef(sim_i)
      # Nestedness scores
      temp_sim_matrix <- nestedtemp(sim_i)
      nodf_sim_matrix <- nestednodf(sim_i, order = TRUE, weighted = FALSE, wbinary = FALSE)
      # Extract statistics
      temp_stat <- as.numeric(temp_sim_matrix$statistic)
      nodf_col_stat <- as.numeric(nodf_sim_matrix$statistic[1])
      nodf_row_stat <- as.numeric(nodf_sim_matrix$statistic[2])
      nodf_gen_stat <- as.numeric(nodf_sim_matrix$statistic[3])
      
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
    }
    write.csv2(df_simulated_b, paste0("simulated_", matrix_id, "_", b, ".csv"), row.names = FALSE)
  }
  
  ### G. After every baselines for this metric: write a csv ----
  write.csv2(df_simulated, paste0("simulated_", matrix_id, "_all.csv"), row.names = FALSE)
}



## ==== 4. Test ====
mat_example <- matrix(c(
  1, 1, 1, 1,
  1, 1, 0, 0,
  1, 0, 1, 0,
  1, 0, 0, 1,
  0, 1, 0, 1
), nrow = 5, byrow = TRUE)

nestedness_analysis(mat_example, "test1", N_ITER_)

# Sanity check
nullmodel_mat <- nullmodel(x = mat_example, method = "r00")
print("nullmodel matrix")
print(nullmodel_mat)
simulated_mat <- simulate(object = nullmodel_mat, nsim = 10)
print("simulated matrix object")
print(simulated_mat)
for (i in 1:dim(simulated_mat)[3]) {
  sim_i <- simulated_mat[, , i]
  print("Simulated matrix ", i)
  print(sim_i)
  }



