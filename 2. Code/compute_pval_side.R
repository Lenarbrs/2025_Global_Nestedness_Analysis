# ======== Boostrapped p-value calculation ========

## ==== 1. Library import ====
library(tidyverse)

# ==== 2. Data import and processing ====

# Define paths
code_dir <- "2. Code"
results_dir <- "../4. Results dataset"  # Assuming "2. Code" is at same level as "4. Results dataset"

# Get all nestedness folders
nestedness_folders <- list.dirs(results_dir, recursive = TRUE) %>%
  keep(~str_detect(.x, "nestedness_")) %>%
  discard(~str_detect(.x, "nestedness_something_we_dont_care_about"))  # Exclude the folder we don't care about

# Initialize an empty tibble to store all results
all_results <- tibble()

# Create a progress log file
log_file <- file.path(code_dir, "processing_log.txt")
writeLines("Processing nestedness results...\n", log_file)

# Process each nestedness folder
for (folder in nestedness_folders) {
  # Extract the "something" part from folder name
  matrix_name <- str_replace(basename(folder), "nestedness_", "")
  
  # Construct file paths
  sim_file <- file.path(folder, paste0("nest_simulated_", matrix_name, "_all.csv"))
  summary_file <- file.path(folder, paste0("nest_summary_", matrix_name, ".csv"))
  
  # Check if both files exist
  if (file.exists(sim_file) && file.exists(summary_file)) {
    # Log current processing
    write(paste("\nProcessing:", matrix_name), log_file, append = TRUE)
    
    # Load data
    sims <- read_csv2(sim_file, show_col_types = FALSE)
    reals <- read_csv2(summary_file, show_col_types = FALSE) %>%
      select(
        matrix_id,
        obs_NODF = stat_nodf_general,
        obs_Temp = stat_temp
      )
    
    ## ==== 3. Compute p-value ====
    compute_pval <- function(obs, sims) {
      n_total <- length(sims)
      n_greater <- sum(sims >= obs, na.rm = TRUE)
      n_lesser <- sum(sims <= obs, na.rm = TRUE)
      p_value <- 2 * min(n_greater, n_lesser) / n_total
      return(min(p_value, 1))
    }
    
    ## ==== 4. Calculate data position ====
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
    
    # Format p-values to 4 decimal places
    final_results <- results %>%
      mutate(across(c(p_NODF, p_Temp), ~sprintf("%.4f", .x)))
    
    # Save individual results
    output_file <- file.path(folder, paste0("nest_pvalue_", matrix_name, ".csv"))
    write_csv(final_results, output_file)
    
    # Append to all_results
    all_results <- bind_rows(all_results, final_results)
    
    # Log the results for this matrix
    write(paste("Results for", matrix_name, ":"), log_file, append = TRUE)
    write.table(final_results, log_file, append = TRUE, row.names = FALSE, quote = FALSE)
    write("\n", log_file, append = TRUE)
    
    # Print progress
    message(paste("Processed:", matrix_name))
  } else {
    warning(paste("Skipping", matrix_name, "- required files not found"))
    write(paste("Skipping", matrix_name, "- required files not found"), log_file, append = TRUE)
  }
}

# Save all results to a single file
write_csv(all_results, file.path(results_dir, "nest_pvalue_all.csv"))

# Print completion message
message("Processing complete. Results saved to individual files and nest_pvalue_all.csv")
write("\nProcessing complete.", log_file, append = TRUE)
