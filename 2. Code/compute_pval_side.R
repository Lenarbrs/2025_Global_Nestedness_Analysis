library(tidyverse)

# Charger les données
sims <- read_csv("simulations.csv")
reals <- read_csv("real_matrices.csv") %>%
  select(
    Matrix_ID,
    obs_NODF = stat_NODF_general,
    obs_Temp = stat_Temp
  )

# Fonction de calcul de p-value bilatérale
compute_pval <- function(obs, sims) {
  n_total <- length(sims)
  n_greater <- sum(sims >= obs, na.rm = TRUE)
  n_lesser <- sum(sims <= obs, na.rm = TRUE)
  p_value <- 2 * min(n_greater, n_lesser) / n_total
  return(min(p_value, 1))  # Cap at 1
}

# Calcul des résultats avec signe
results <- reals %>%
  expand_grid(Baseline = unique(sims$Baseline)) %>%
  left_join(sims, by = c("Matrix_ID", "Baseline")) %>%
  group_by(Matrix_ID, Baseline) %>%
  summarise(
    # P-values
    p_NODF = compute_pval(first(obs_NODF), stat_NODF_general),
    p_Temp = compute_pval(first(obs_Temp), stat_Temp),
    
    # Sign determination
    mean_sim_NODF = mean(stat_NODF_general, na.rm = TRUE),
    mean_sim_Temp = mean(stat_Temp, na.rm = TRUE),
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
  mutate(across(c(p_NODF, p_Temp), ~sprintf("%.6f", .x)))

# Export des résultats
# write_csv(final_results, "nestedness_results_with_significance.csv")

# Affichage
print(final_results, n = Inf)