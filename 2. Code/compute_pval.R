#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Script to compute nestedness p-values -----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ====== 1. Library Import ======
library(tidyverse)

# ====== 2. Load CSV Files ======
sim_file  <- "simulations.csv"    
real_file <- "real_matrices.csv"  

sims  <- read_csv(sim_file)
reals <- read_csv(real_file) %>%
  select(
    Matrix_ID,
    obs_NODF = stat_NODF_general,
    obs_Temp = stat_Temp
  )

# ====== 3. Two-Sided Empirical P-Value Function ======
# obs  : numeric, the observed statistic
# sims : numeric vector, simulated statistics from the null model
compute_pval <- function(obs, sims) {
  # Count simulations ≥ observed value
  greater <- sum(sims >= obs, na.rm = TRUE)
  
  # Count simulations ≤ observed value
  lesser <- sum(sims <= obs, na.rm = TRUE)
  
  # Compute two-sided p-value:
  # 1) Take the smaller tail count
  # 2) Multiply by 2 for both tails
  # 3) Divide by total number of simulations
  p_value <- 2 * min(greater, lesser) / length(sims)
  
  return(p_value)
}

# ===== 4. Compute P-Values for Each Baseline ======
pvals_df <- reals %>%
  # Expand to one row per baseline
  crossing(tibble(Baseline = unique(sims$Baseline))) %>%
  # Join corresponding simulation results
  left_join(sims, by = c("Matrix_ID", "Baseline")) %>%
  # Group by matrix and baseline
  group_by(Matrix_ID, Baseline, obs_NODF, obs_Temp) %>%
  # Calculate p-values for NODF and Temperature
  summarise(
    p_value_NODF = compute_pval(obs_NODF, stat_NODF_general),
    p_value_Temp = compute_pval(obs_Temp, stat_Temp),
    .groups = "drop"
  ) %>%
  # Reshape to wide format: one column per baseline and metric
  pivot_wider(
    names_from  = Baseline,
    values_from = c(p_value_NODF, p_value_Temp),
    names_sep   = "_"
  )

pvals_df_formatted <- pvals_df %>%
  mutate(
    across(
      starts_with("p_value"),
      ~ formatC(.x, format = "f", digits = 6)
    )
  )

# ===== 5. Export Results =====
# write_csv2(pvals_df,
#            file   = "nestedness_pvalues.csv",
#            row.names = FALSE,
#            digits = 8
# )
