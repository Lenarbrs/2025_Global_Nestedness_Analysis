# ======== Boostrapped p-value calculation ========

## ==== 1. Library import ====
library(tidyverse)

# ==== 2. Data import ====
sims <- read_csv2("nest_simulated_americanhistory_all.csv", show_col_types = FALSE)

# Load observed statistics and rename columns
reals <- read_csv2("nest_summary_americanhistory.csv", show_col_types = FALSE) %>%
  select(
    matrix_id,
    obs_NODF = stat_nodf_general,  # Observed nestedness
    obs_Temp = stat_temp           # Observed temperature
  )

## ==== 3. Compute p-value ====
compute_pval <- function(obs, sims) {
  n_total <- length(sims)                          # Total number of simulations
  n_greater <- sum(sims >= obs)     # Number of simulations >= observed
  n_lesser <- sum(sims <= obs)     # Number of simulations <= observed
  p_value <- 2 * min(n_greater, n_lesser) / n_total    # Take the more extreme side
  return(min(p_value, 1))                          # Ensure p-value does not exceed 1
}

## ==== 4. Calculate data position ====
results <- reals %>%
  expand_grid(baseline = unique(sims$baseline)) %>%     # Ensure all baseline x matrix_id combinations
  left_join(sims, by = c("matrix_id", "baseline")) %>%  # Merge with simulated data
  group_by(matrix_id, baseline) %>%
  summarise(
    # Compute two-tailed p-values for both statistics
    p_NODF = compute_pval(first(obs_NODF), stat_nodf_general),
    p_Temp = compute_pval(first(obs_Temp), stat_temp),
    
    ## ==== 5. Compute p-value direction ====
    # Compute simulation means for comparison
    mean_sim_NODF = median(stat_nodf_general, na.rm = TRUE),
    mean_sim_Temp = median(stat_temp, na.rm = TRUE),
    # Keep the observed values for reference
    obs_NODF = first(obs_NODF),
    obs_Temp = first(obs_Temp),
    .groups = "drop"
  ) %>%
  mutate(
    # Determine significance direction for NODF
    sign_NODF = case_when(
      obs_NODF > mean_sim_NODF ~ "nested",
      obs_NODF < mean_sim_NODF ~ "antinested",
      TRUE ~ "equal"
    ),
    # Determine significance direction for Temp (inverse logic)
    sign_Temp = case_when(
      obs_Temp < mean_sim_Temp ~ "nested",      # Lower temperature → more nested
      obs_Temp > mean_sim_Temp ~ "antinested",  # Higher temperature → less nested
      TRUE ~ "equal"
    )
  ) %>%
  
  ## ==== 6. Save results ====
  # Remove raw mean and observed values, keep only p-values and signs
  select(-starts_with("mean_sim_"), -starts_with("obs_"))

# Format p-values to 4 decimal places
final_results <- results %>%
  mutate(across(c(p_NODF, p_Temp), ~sprintf("%.4f", .x)))
# Export results to CSV
write_csv(final_results, "nestedness_results_with_significance.csv")
# Display full results
print(final_results, n = Inf)

