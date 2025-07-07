#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Boostrapped p-value calculation ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Load tidyverse
library(tidyverse)

# Function to compute a two‐tailed p‐value
# obs = one observed statistic
# sims = vector of simulated statistics
compute_pval <- function(obs, sims) {
  n_total   <- length(sims)   # total number of sims
  n_greater <- sum(sims >= obs)   # sims at or above obs
  n_lesser  <- sum(sims <= obs) # sims at or below obs
  p_value   <- 2 * min(n_greater, n_lesser) / n_total
  return(min(p_value, 1))            # cap at 1
}

# Root directory containing your “4. Results dataset” folder
root_dir <- "C:/Users/Léna/Desktop/GitHub/Global_Nestedness_Analysis/4. Results dataset"

# List all first‐level subfolders (e.g. “phoible”, “Archeology”, etc.)
level1_dirs <- list.dirs(root_dir, recursive = FALSE, full.names = TRUE)
results_list <- list()

# Loop over each first‐level folder
for (lvl1 in level1_dirs) {
  # Get the name of the level‐1 category
  category1 <- basename(lvl1)
  # List its immediate subfolders (level‐2)
  level2_dirs <- list.dirs(lvl1, recursive = FALSE, full.names = TRUE)
  # Loop over each level‐2 folder
  for (lvl2 in level2_dirs) {
    
    # Name of the specific dataset (level‐2)
    category2 <- basename(lvl2)
    
    # Find the one simulated‐data CSV and one summary CSV
    sim_file     <- list.files(lvl2, pattern = "^nest_simulated.*\\.csv$", full.names = TRUE)
    summary_file <- list.files(lvl2, pattern = "^nest_summary.*\\.csv$",  full.names = TRUE)
    
    # If file detection fails, warn and skip
    if (length(sim_file) != 1 || length(summary_file) != 1) {
      warning("Skipping ", lvl2, 
              ": found ", length(sim_file), " simulated files and ", 
              length(summary_file), " summary files.")
      next
    }
    
    # Read in simulations and observed summaries
    sims  <- read_csv2(sim_file, show_col_types = FALSE)
    reals <- read_csv2(summary_file, show_col_types = FALSE) %>%
      select(
        matrix_id,
        obs_NODF = stat_nodf_general,
        obs_Temp = stat_temp
      )
    
    # Compute p‐values and direction flags per matrix / baseline
    df_out <- reals %>%
      expand_grid(baseline = unique(sims$baseline)) %>%
      left_join(sims, by = c("matrix_id", "baseline")) %>%
      group_by(matrix_id, baseline) %>%
      summarise(
        # two‐tailed p‐values
        p_NODF = compute_pval(first(obs_NODF), stat_nodf_general),
        p_Temp = compute_pval(first(obs_Temp),   stat_temp),
        # medians of sims for sign comparison
        med_sim_NODF = median(stat_nodf_general, na.rm = TRUE),
        med_sim_Temp = median(stat_temp,           na.rm = TRUE),
        # observed values 
        obs_NODF = first(obs_NODF),
        obs_Temp = first(obs_Temp),
        .groups = "drop"
      ) %>%
      mutate(
        # decide “nested” vs “antinested” vs “equal”
        sign_NODF = case_when(
          obs_NODF >  med_sim_NODF ~ "nested", # higher nodf = more nested
          obs_NODF <  med_sim_NODF ~ "antinested", # lower nodf = less nested
          TRUE                     ~ "equal"
        ),
        sign_Temp = case_when(
          obs_Temp <  med_sim_Temp ~ "nested",     # lower temp = more nested
          obs_Temp >  med_sim_Temp ~ "antinested", # higher temp = less nested
          TRUE                     ~ "equal"
        )
      ) %>%
      # keep only essentials and format p‐values
      select(matrix_id, baseline, p_NODF, sign_NODF, p_Temp, sign_Temp) %>%
      mutate(across(c(p_NODF, p_Temp), ~ sprintf("%.4f", .x))) %>%
      # annotate with folder levels
      mutate(
        category1 = category1,
        category2 = category2
      )
    
    # Save a separate CSV for each matrix in this dataset
    df_out %>%
      group_split(matrix_id) %>%
      walk(~ write_csv2(.x,
                        file.path(lvl2,
                                  paste0("results_matrix_", unique(.x$matrix_id), ".csv"))))
    
    # Save the combined results for this folder
    write_csv2(df_out,
               file.path(lvl2,
                         paste0("results_folder_", category2, ".csv")))
    
    # Store into our list for potential further aggregation
    results_list[[paste(category1, category2, sep = "__")]] <- df_out
  }
}

# Combine all data frames into one final tibble (optional)
all_results <- bind_rows(results_list)

# Display the full results
print(all_results, n = Inf)

# write out full aggregation
# write_csv2(all_results, "nestedness_results_all_levels.csv")
