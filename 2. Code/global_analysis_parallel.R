#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Re analysis of nestedness in empirical matrices ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


## ==== 1. Library import ====
library(tidyverse)
library(progress)
library(vegan)
library(permute)
library(lattice)
library(parallel)
library(data.table)
library(foreach)
library(doParallel)

### Parameters ----
# Set parallel options
options(mc.cores = max(1, parallel::detectCores() - 1))
# Number of simulations
N_ITER_ <- 10

## ==== 2. Functions ====
### ---- A. Optimized compute correlation ----
compute_cor_coef <- function(matrix) {
  row_totals <- rowSums(matrix)
  non_zero <- matrix != 0
  
  avg_inventory <- numeric(ncol(matrix))
  for (j in 1:ncol(matrix)) {
    item_col <- non_zero[, j]
    if (any(item_col)) {
      avg_inventory[j] <- mean(row_totals[item_col])
    } else {
      avg_inventory[j] <- NA_real_
    }
  }
  
  prevalence <- colSums(matrix)
  valid <- !is.na(avg_inventory) & !is.na(prevalence)
  
  if (sum(valid) < 2) return(NA_real_)
  
  sd_prev <- sd(prevalence[valid])
  sd_avginv <- sd(avg_inventory[valid])
  
  if (is.na(sd_prev) || is.na(sd_avginv) || sd_prev == 0 || sd_avginv == 0) {
    return(NA_real_)
  }
  
  cor(prevalence[valid], avg_inventory[valid])
}

## ==== 3. Optimized nestedness analysis ====
nestedness_analysis <- function(matrix, matrix_id, N_ITER_) {
  dir.create(paste0("nestedness_", matrix_id), showWarnings = FALSE)
  dir.create(paste0("nestedness_", matrix_id, "/sim_", matrix_id), showWarnings = FALSE)
  dir.create(paste0("nestedness_", matrix_id, "/sim_", matrix_id, "/simmat_examples_", matrix_id), showWarnings = FALSE)
  
  ### A. Calculate real matrix nestedness properties ----
  num_elements <- nrow(matrix) * ncol(matrix)
  num_ones <- sum(matrix == 1)
  fill_percentage <- (num_ones / num_elements) * 100
  cor_coef <- compute_cor_coef(matrix)
  
  # Calculate nestedness metrics
  temp_real_matrix <- nestedtemp(matrix)
  nodf_real_matrix <- nestednodf(matrix, order = TRUE, weighted = FALSE, wbinary = FALSE)
  
  metrics <- list(
    temp_stat = as.numeric(temp_real_matrix$statistic),
    nodf_col_stat = as.numeric(nodf_real_matrix$statistic[1]),
    nodf_row_stat = as.numeric(nodf_real_matrix$statistic[2]),
    nodf_gen_stat = as.numeric(nodf_real_matrix$statistic[3])
  )
  
  ### B. Summary dataset ----
  df_summary <- data.frame(
    matrix_id = matrix_id,
    num_rows = nrow(matrix),
    num_columns = ncol(matrix),
    size = num_elements,
    fill = fill_percentage,
    cor_coef = cor_coef,
    stat_nodf_columns = metrics$nodf_col_stat,
    stat_nodf_rows = metrics$nodf_row_stat,
    stat_nodf_general = metrics$nodf_gen_stat,
    stat_temp = metrics$temp_stat,
    stringsAsFactors = FALSE
  )
  write.csv2(df_summary, paste0("nestedness_", matrix_id, "/nest_summary_", matrix_id, ".csv"), row.names = FALSE)
  
  ### C. Parameters list ----
  baselines <- c('r00', 'r0', 'r1', 'r2', 'c0', 'c1', 'curveball', 'swap')
  
  ### D. Process baselines sequentially ----
  df_simulated_list <- lapply(baselines, function(b) {
    current_matrix <- matrix
    baseline_used <- b
    if (b == 'c1') {
      current_matrix <- t(matrix)
      baseline_used <- 'r1'
    }
    
    # Simulate matrices
    nullmodel_mat <- nullmodel(x = current_matrix, method = baseline_used)
    simulated_mat <- simulate(object = nullmodel_mat, nsim = N_ITER_)
    
    # Process each simulated matrix
    df_simulated_b <- lapply(1:dim(simulated_mat)[3], function(i) {
      sim_i <- simulated_mat[, , i]
      
      # Save first matrix as example
      if (i == 1) {
        mat_directory <- paste0("nestedness_", matrix_id, "/sim_", matrix_id, "/simmat_examples_", matrix_id, "/example_simmat_", matrix_id, "_", b, ".csv")
        write.csv2(sim_i, mat_directory)
      }
      
      # Calculate metrics
      cor_coef_sim <- compute_cor_coef(sim_i)
      
      metrics_sim <- tryCatch({
        temp_sim_matrix <- nestedtemp(sim_i)
        nodf_sim_matrix <- nestednodf(sim_i, order = TRUE, weighted = FALSE, wbinary = FALSE)
        list(
          temp_stat = as.numeric(temp_sim_matrix$statistic),
          nodf_col_stat = as.numeric(nodf_sim_matrix$statistic[1]),
          nodf_row_stat = as.numeric(nodf_sim_matrix$statistic[2]),
          nodf_gen_stat = as.numeric(nodf_sim_matrix$statistic[3])
        )
      }, error = function(e) {
        list(temp_stat = NA_real_, nodf_col_stat = NA_real_, nodf_row_stat = NA_real_, nodf_gen_stat = NA_real_)
      })
      
      data.frame(
        matrix_id = matrix_id,
        baseline = b,
        ceof_cor = cor_coef_sim,
        stat_nodf_columns = metrics_sim$nodf_col_stat,
        stat_nodf_rows = metrics_sim$nodf_row_stat,
        stat_nodf_general = metrics_sim$nodf_gen_stat,
        stat_temp = metrics_sim$temp_stat,
        stringsAsFactors = FALSE
      )
    })
    
    # Combine results for this baseline
    df_simulated_b <- do.call(rbind, df_simulated_b)
    
    # Save nestedness results for this baseline
    write.csv2(df_simulated_b, paste0("nestedness_", matrix_id, "/sim_", matrix_id, "/nest_simulated_", matrix_id, "_", b, ".csv"), row.names = FALSE)
    
    df_simulated_b
  })
  
  ### H. Combine and save all results ----
  df_simulated <- do.call(rbind, df_simulated_list)
  write.csv2(df_simulated, paste0("nestedness_", matrix_id, "/nest_simulated_", matrix_id, "_all.csv"), row.names = FALSE)
}


## ==== 5. Apply function to real matrices (cross-platform optimized) ====

# Set folder path
folder_path <- "Matrices examples simulated"
# List all CSV files in the folder
file_list <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

# Precompute cleaned names using step-by-step cleaning
cleaned_names <- basename(file_list) %>%
  gsub("\\.csv$", "", .) %>%       # Remove .csv extension
  gsub("^cleaned_", "", .) %>%     # Remove "cleaned_" prefix
  gsub("^bin_", "", .) %>%         # Remove "bin_" prefix
  gsub("^matrix_", "", .) %>%      # Remove "matrix_" prefix
  gsub("_bin$", "", .)             # Remove "_bin" suffix

# Set up parallel backend
n_cores <- max(1, detectCores() - 2)  # Reserve 2 cores for system
cl <- makeCluster(n_cores)
registerDoParallel(cl)

# Export required functions to cluster
clusterExport(cl, c("compute_cor_coef", "nestedness_analysis", "N_ITER_"))

# Process matrices in parallel
results <- foreach(i = seq_along(file_list), .packages = c("data.table", "vegan", "permute")) %dopar% {
  file_path <- file_list[i]
  matrix_id <- cleaned_names[i]
  
  # Fast matrix reading
  matrix_data <- as.matrix(fread(file_path, header = FALSE))
  
  # Run analysis
  nestedness_analysis(matrix_data, matrix_id, N_ITER_)
  
  return(matrix_id)
}

# Stop cluster
stopCluster(cl)

# Print completion message
cat("Successfully processed", length(file_list), "matrices\n")
