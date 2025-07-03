#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============== Re-analysis of nestedness in empirical matrices ===============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## ==== 1. General setup ====
### ---- A. Library import ----
# general use
library(tidyverse)
# nestedness analysis
library(vegan)
library(permute)
library(lattice)
# time optimisation
library(parallel)     # For parallel computing capabilities
library(data.table)   # For fast file I/O operations
library(foreach)      # For parallel looping constructs
library(doParallel)   # For parallel backend implementation

### ---- B. Parameters ----
# Number of simulations to run for each null model
N_ITER_ <- 10

### ---- C. Compute correlation function ----
compute_cor_coef <- function(matrix) {
  # Calculate inventory size for each cultural collection
  row_totals <- rowSums(matrix)
  non_zero <- matrix != 0
  # Pre-allocate vector for average inventory sizes
  avg_inventory <- numeric(ncol(matrix))
  # Calculate average inventory size for each cultural type
  for (j in 1:ncol(matrix)) {
    type_col <- non_zero[, j]
    if (any(type_col)) {
      avg_inventory[j] <- mean(row_totals[type_col])
    } else {
      # Handle case where no agent has the item
      avg_inventory[j] <- NA_real_
    }
  }
  # Calculate cultural type prevalence
  prevalence <- colSums(matrix)
  
  # Check if correlation can be computed
  valid <- !is.na(avg_inventory) & !is.na(prevalence)
  if (sum(valid) < 2) return(NA_real_)
  sd_prev <- sd(prevalence[valid])
  sd_avginv <- sd(avg_inventory[valid])
  if (is.na(sd_prev) || is.na(sd_avginv) || sd_prev == 0 || sd_avginv == 0) {
    return(NA_real_)
  }
  
  # Compute and return Pearson correlation
  cor(prevalence[valid], avg_inventory[valid])
}

## ==== 2. Nestedness analysis ====
nestedness_analysis <- function(matrix, matrix_id, N_ITER_) {
  # Create output directories
  dir.create(paste0("nestedness_", matrix_id), showWarnings = FALSE)
  dir.create(paste0("nestedness_", matrix_id, "/sim_", matrix_id), showWarnings = FALSE)
  dir.create(paste0("nestedness_", matrix_id, "/sim_", matrix_id, "/simmat_examples_", matrix_id), showWarnings = FALSE)
  
  ### ---- A. Calculate real matrix properties ----
  # Fill & Size
  num_elements <- nrow(matrix) * ncol(matrix)
  num_ones <- sum(matrix == 1)
  fill_percentage <- (num_ones / num_elements) * 100
  #Correlation coefficient
  cor_coef <- compute_cor_coef(matrix)
  # Calculate nestedness statistics
  temp_real_matrix <- nestedtemp(matrix)
  nodf_real_matrix <- nestednodf(matrix, order = TRUE, weighted = FALSE, wbinary = FALSE)
  metrics <- list(
    temp_stat = as.numeric(temp_real_matrix$statistic),
    nodf_col_stat = as.numeric(nodf_real_matrix$statistic[1]),
    nodf_row_stat = as.numeric(nodf_real_matrix$statistic[2]),
    nodf_gen_stat = as.numeric(nodf_real_matrix$statistic[3])
  )
  
  ### ---- B. Create and save summary dataset ----
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
  
  ### ---- C. Define baselines ----
  baselines <- c('r00', 'r0', 'r1', 'r2', 'c0', 'c1', 'curveball', 'swap')
  
  ### ---- D. Process baselines sequentially ----
  # For each null model baseline:
  df_simulated_list <- lapply(baselines, function(b) {
    current_matrix <- matrix
    baseline_used <- b
    # Special handling for c1 baseline (transpose and use r1 method)
    if (b == 'c1') {
      current_matrix <- t(matrix)
      baseline_used <- 'r1'
    }
    
    # Generate and simulate null model matrices
    nullmodel_mat <- nullmodel(x = current_matrix, method = baseline_used)
    simulated_mat <- simulate(object = nullmodel_mat, nsim = N_ITER_)
    
    # Process each simulated matrix
    df_simulated_b <- lapply(1:dim(simulated_mat)[3], function(i) {
      sim_i <- simulated_mat[, , i]
      # Save first matrix as example for each baseline
      if (i == 1) {
        mat_directory <- paste0("nestedness_", matrix_id, "/sim_", matrix_id, "/simmat_examples_", matrix_id, "/example_simmat_", matrix_id, "_", b, ".csv")
        write.csv2(sim_i, mat_directory)  # Save matrix to file
      }
      
      ### ---- E. Compute simulated matrix properties ----
      # Calculate correlation coefficient
      cor_coef_sim <- compute_cor_coef(sim_i)
      # Calculate nestedness metrics with error handling
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
        # Return NAs if calculation fails
        list(temp_stat = NA_real_, nodf_col_stat = NA_real_, nodf_row_stat = NA_real_, nodf_gen_stat = NA_real_)
      })
      
      # Create results row for this simulation
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
    
    # Combine all simulations for this baseline
    df_simulated_b <- do.call(rbind, df_simulated_b)
    # Save baseline-specific results
    write.csv2(df_simulated_b, paste0("nestedness_", matrix_id, "/sim_", matrix_id, "/nest_simulated_", matrix_id, "_", b, ".csv"), row.names = FALSE)
    
    # Return results for later combination
    df_simulated_b
  })
  
  ### ---- F. Combine and save all results ----
  # Combine results from all baselines
  df_simulated <- do.call(rbind, df_simulated_list)
  # Save comprehensive results file
  write.csv2(df_simulated, paste0("nestedness_", matrix_id, "/nest_simulated_", matrix_id, "_all.csv"), row.names = FALSE)
}


## ==== 3. Apply function to real matrices (with progress tracking) ====

# Set folder path
folder_path <- "Matrices examples simulated"
# List all CSV files in the folder
file_list <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

# Precompute cleaned names using step-by-step cleaning
cleaned_names <- basename(file_list) %>%  # Extract filenames without path
  gsub("\\.csv$", "", .) %>%       # Remove .csv extension
  gsub("^cleaned_", "", .) %>%     # Remove "cleaned_" prefix
  gsub("^bin_", "", .) %>%         # Remove "bin_" prefix
  gsub("^matrix_", "", .) %>%      # Remove "matrix_" prefix
  gsub("_bin$", "", .)             # Remove "_bin" suffix

# Set up progress log file
log_file <- "matrix_processing.log"
cat("=== Matrix Processing Log ===\n", file = log_file)
cat("Started at:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n", file = log_file, append = TRUE)

# Set up parallel backend
n_cores <- max(1, detectCores() - 2)  # Reserve 2 cores for system stability
cl <- makeCluster(n_cores)
registerDoParallel(cl)

# Export required functions to cluster
clusterExport(cl, c("compute_cor_coef", "nestedness_analysis", "N_ITER_", "log_file"))

# Process matrices in parallel with progress tracking
results <- foreach(
  i = seq_along(file_list), 
  .packages = c("data.table", "vegan", "permute"),
  .errorhandling = "pass"  # Continue processing even if some fail
) %dopar% {
  file_path <- file_list[i]
  matrix_id <- cleaned_names[i]
  
  # Log start of processing
  start_msg <- paste0(format(Sys.time(), "[%Y-%m-%d %H:%M:%S]"), 
                      " STARTED matrix: ", matrix_id, 
                      " (", i, "/", length(file_list), ")")
  cat(start_msg, "\n", file = log_file, append = TRUE)
  
  tryCatch({
    # Fast matrix reading
    matrix_data <- as.matrix(fread(file_path, header = FALSE))
    
    # Log matrix dimensions
    dim_msg <- paste0(format(Sys.time(), "[%Y-%m-%d %H:%M:%S]"),
                      " SIZE: ", nrow(matrix_data), "x", ncol(matrix_data),
                      " (", round(object.size(matrix_data)/1024^2, 2), " MB)")
    cat(dim_msg, "\n", file = log_file, append = TRUE)
    
    # Run analysis
    nestedness_analysis(matrix_data, matrix_id, N_ITER_)
    
    # Log successful completion
    success_msg <- paste0(format(Sys.time(), "[%Y-%m-%d %H:%M:%S]"),
                          " COMPLETED matrix: ", matrix_id)
    cat(success_msg, "\n", file = log_file, append = TRUE)
    
    return(list(matrix_id = matrix_id, status = "success"))
  }, error = function(e) {
    # Log error details
    error_msg <- paste0(format(Sys.time(), "[%Y-%m-%d %H:%M:%S]"),
                        " ERROR in matrix: ", matrix_id,
                        " - ", conditionMessage(e))
    cat(error_msg, "\n", file = log_file, append = TRUE)
    return(list(matrix_id = matrix_id, status = "error", error = conditionMessage(e)))
  })
}

# Stop cluster
stopCluster(cl)

# Final log summary
success_count <- sum(sapply(results, function(x) x$status == "success"))
error_count <- length(file_list) - success_count

cat("\n=== Processing Summary ===\n", file = log_file, append = TRUE)
cat("Finished at:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n", file = log_file, append = TRUE)
cat("Total matrices:", length(file_list), "\n", file = log_file, append = TRUE)
cat("Successfully processed:", success_count, "\n", file = log_file, append = TRUE)
cat("Failed:", error_count, "\n", file = log_file, append = TRUE)

# Print completion message to console
cat("\nProcessing complete. Success:", success_count, "Errors:", error_count, "\n")
cat("See detailed log in:", log_file, "\n")

# Print error details to console if any
if (error_count > 0) {
  cat("\n=== Error Details ===\n")
  errors <- results[sapply(results, function(x) x$status == "error")]
  for (e in errors) {
    cat("Matrix:", e$matrix_id, "\n")
    cat("Error:", e$error, "\n\n")
  }
}