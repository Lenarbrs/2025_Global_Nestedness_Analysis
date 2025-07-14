#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Pvalue Bootstrap Preprocessing ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# This script computes two-tailed bootstrapped p-values for nestedness metrics
# (NODF and Temperature) based on simulation outputs. It reads observed vs.
# simulated values from each matrix folder, compares them, and outputs per-matrix
# significance results as well as a global summary CSV.
#
# Requirements:
# - Script must be run from a project root where `4. Results dataset` is located
# - Simulation files and observed summary files must follow naming conventions
#
# Outputs:
# - A CSV file with p-values and significance directions per matrix/baseline
# - Log file tracking progress and missing files


## ==== 1. Library Import ====
library(tidyverse)  # For data manipulation and reading/writing CSVs

## ==== 2. Setup Directories and Logging ====
code_dir <- getwd()               # Assume script is run from working directory
results_dir <- "4. Results dataset"    # Relative path to results
log_file <- "processing_log.txt"       # Log output file
writeLines("Processing nestedness results...\n", log_file)  # Initialize log

# List all folders starting with 'nestedness_' and ignore subfolders like 'sim_'
nestedness_folders <- list.dirs(results_dir, recursive = TRUE) %>%
  keep(~str_detect(.x, "nestedness_")) %>%
  discard(~str_detect(.x, "sim_"))

# Prepare container for all results
all_results <- tibble()

## ==== 3. Loop Through Folders ====
for (folder in nestedness_folders) {
  matrix_name <- str_replace(basename(folder), "nestedness_", "")
  sim_file <- file.path(folder, paste0("nest_simulated_", matrix_name, "_all.csv"))
  summary_file <- file.path(folder, paste0("nest_summary_", matrix_name, ".csv"))
  
  if (file.exists(sim_file) && file.exists(summary_file)) {
    write(paste("\nProcessing:", matrix_name), log_file, append = TRUE)
    
    # Load data
    sims <- read_csv2(sim_file, show_col_types = FALSE)
    reals <- read_csv2(summary_file, show_col_types = FALSE) %>%
      select(matrix_id, obs_NODF = stat_nodf_general, obs_Temp = stat_temp)
    
    ## ==== 4. Define p-value function ====
    compute_pval <- function(obs, sims) {
      n_total <- length(sims)
      n_greater <- sum(sims >= obs, na.rm = TRUE)
      n_lesser  <- sum(sims <= obs, na.rm = TRUE)
      p_value <- 2 * min(n_greater, n_lesser) / n_total  # Two-tailed p-value
      return(min(p_value, 1))
    }
    
    ## ==== 5. Compare Observed vs Simulated ====
    results <- reals %>%
      expand_grid(baseline = unique(sims$baseline)) %>%
      left_join(sims, by = c("matrix_id", "baseline")) %>%
      group_by(matrix_id, baseline) %>%
      summarise(
        p_NODF = compute_pval(first(obs_NODF), stat_nodf_general),
        p_Temp = compute_pval(first(obs_Temp), stat_temp),
        mean_sim_NODF = median(stat_nodf_general, na.rm = TRUE),
        mean_sim_Temp = median(stat_temp, na.rm = TRUE),
        obs_NODF = first(obs_NODF),
        obs_Temp = first(obs_Temp),
        .groups = "drop"
      ) %>%
      mutate(
        sign_NODF = case_when(
          obs_NODF > mean_sim_NODF ~ "nested",
          obs_NODF < mean_sim_NODF ~ "antinested",
          TRUE ~ "equal"
        ),
        sign_Temp = case_when(
          obs_Temp < mean_sim_Temp ~ "nested",
          obs_Temp > mean_sim_Temp ~ "antinested",
          TRUE ~ "equal"
        )
      ) %>%
      select(matrix_id, baseline, p_NODF, p_Temp, sign_NODF, sign_Temp)
    
    ## ==== 6. Format and Save Results ====
    final_results <- results %>%
      mutate(across(c(p_NODF, p_Temp), ~sprintf("%.4f", .x)))
    
    # Write matrix-specific result
    output_file <- file.path(folder, paste0("nest_pvalue_", matrix_name, ".csv"))
    write_csv(final_results, output_file)
    
    # Store in overall output
    all_results <- bind_rows(all_results, final_results)
    
    # Log to file
    write(paste("Results for", matrix_name, ":"), log_file, append = TRUE)
    write.table(final_results, log_file, append = TRUE, row.names = FALSE, 
                quote = FALSE)
    write("\n", log_file, append = TRUE)
    
    # Console message
    message(paste("Processed:", matrix_name))
    
  } else {
    # Handle missing files
    warning(paste("Skipping", matrix_name, "- required files not found"))
    write(paste("Skipping", matrix_name, "- required files not found"), 
          log_file, append = TRUE)
  }
}

## ==== 7. Export Combined Results ====
write_csv(all_results, file.path(results_dir, "nest_pvalue_all.csv"))
message("Processing complete. Results saved to individual files and nest_pvalue_all.csv")
write("\nProcessing complete.", log_file, append = TRUE)