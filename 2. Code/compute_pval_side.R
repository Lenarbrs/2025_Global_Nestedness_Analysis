# ======== Boostrapped p-value calculation ========

library(tidyverse)

# Charger les données
sims <- read_csv2("nest_simulated_mat_example_2_all.csv", show_col_types = FALSE)
reals <- read_csv2("nest_summary_mat_example_2.csv", show_col_types = FALSE) %>%
  select(
    matrix_id,
    obs_NODF = stat_nodf_general,
    obs_Temp = stat_temp
  )

# Fonction de calcul de p-value bilatérale
compute_pval <- function(obs, sims) {
  n_total <- length(sims)
  n_greater <- sum(sims >= obs, na.rm = TRUE)
  n_lesser <- sum(sims <= obs, na.rm = TRUE)
  p_value <- min(n_greater, n_lesser) / n_total
  return(min(p_value, 1))  # Cap at 1
}

# Calcul des résultats avec signe
results <- reals %>%
  expand_grid(baseline = unique(sims$baseline)) %>%
  left_join(sims, by = c("matrix_id", "baseline")) %>%
  group_by(matrix_id, baseline) %>%
  summarise(
    # P-values
    p_NODF = compute_pval(first(obs_NODF), stat_nodf_general),
    p_Temp = compute_pval(first(obs_Temp), stat_temp),
    
    # Sign determination
    mean_sim_NODF = mean(stat_nodf_general, na.rm = TRUE),
    mean_sim_Temp = mean(stat_temp, na.rm = TRUE),
    obs_NODF = first(obs_NODF),
    obs_Temp = first(obs_Temp),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Determine nestedness direction with significance
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
  select(-starts_with("mean_sim_"), -starts_with("obs_"))

# Formatage des p-values
final_results <- results %>%
  mutate(across(c(p_NODF, p_Temp), ~sprintf("%.4f", .x)))

# Export des résultats
# write_csv(final_results, "nestedness_results_with_significance.csv")

# Affichage
print(final_results, n = Inf)