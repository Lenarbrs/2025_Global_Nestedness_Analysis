#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Script for data visualisation ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Load libraries ----
library(tidyverse)
library(ggtext)
library(patchwork)

# Function ----
# 1. Function to read all CSVs and create df_obs + sim_summary for every family
process_all_csv_results <- function(measure_col = c("stat_nodf_general", "stat_temp"),
                                    alpha = 0.05) {
  measure_col <- match.arg(measure_col)
  
  # -- Find all nestedness_<family> directories
  dirs       <- list.files(pattern = "^nestedness_")
  family_ids <- sub("^nestedness_", "", dirs)
  
  all_obs <- list()
  all_sim <- list()
  
  for (fam in family_ids) {
    # -- Read the "real" summary
    df_real <- read.csv2(
      file.path(paste0("nestedness_", fam),
                paste0("nest_summary_", fam, ".csv")),
      stringsAsFactors = FALSE
    ) %>%
      transmute(
        Family  = fam,
        n_langs = num_rows,
        Value   = .data[[measure_col]]
      )
    
    # -- Read all simulations
    df_sim <- read.csv2(
      file.path(paste0("nestedness_", fam),
                paste0("nest_simulated_", fam, "_all.csv")),
      stringsAsFactors = FALSE
    )
    
    # -- Compute simulated summaries for r00 and c0
    sim_summary <- df_sim %>%
      filter(baseline %in% c("r00", "c0")) %>%
      group_by(baseline) %>%
      summarise(
        Family   = fam,
        Sim_Mean = mean(.data[[measure_col]], na.rm = TRUE),
        lower    = quantile(.data[[measure_col]], probs = alpha/2,     na.rm = TRUE),
        upper    = quantile(.data[[measure_col]], probs = 1 - alpha/2, na.rm = TRUE),
        .groups  = "drop"
      ) %>%
      rename(Baseline = baseline)
    
    # -- Prepare observed data with p-values, significance and shapes
    df_obs <- sim_summary %>%
      left_join(df_real, by = "Family") %>%
      mutate(
        p_value = case_when(
          Value < lower ~ sum(df_sim[[measure_col]] <= Value & df_sim$baseline == Baseline) /
            sum(df_sim$baseline == Baseline),
          Value > upper ~ sum(df_sim[[measure_col]] >= Value & df_sim$baseline == Baseline) /
            sum(df_sim$baseline == Baseline),
          TRUE         ~ 1 - alpha
        ),
        significant = p_value < alpha,
        Significant_Side = case_when(
          !significant                                    ~ NA_character_,
          measure_col == "stat_nodf_general" & Value > Sim_Mean ~ "Nested",
          measure_col == "stat_nodf_general" & Value < Sim_Mean ~ "Antinested",
          measure_col == "stat_temp"         & Value > Sim_Mean ~ "Antinested",
          measure_col == "stat_temp"         & Value < Sim_Mean ~ "Nested",
          TRUE                                             ~ NA_character_
        ),
        shape = case_when(
          Significant_Side == "Nested"     ~ "triangle",
          Significant_Side == "Antinested" ~ "square",
          TRUE                              ~ "circle"
        )
      )
    
    all_obs[[fam]] <- df_obs
    all_sim[[fam]] <- sim_summary
  }
  
  # -- Combine across families
  df_obs_all     <- bind_rows(all_obs)
  sim_summary_all <- bind_rows(all_sim)
  
  # -- Order families by number of languages (ascending)
  fam_levels <- df_obs_all %>%
    distinct(Family, n_langs) %>%
    arrange(n_langs) %>%
    pull(Family)
  
  df_obs_all$Family      <- factor(df_obs_all$Family,      levels = fam_levels)
  sim_summary_all$Family <- factor(sim_summary_all$Family, levels = fam_levels)
  
  list(df_obs     = df_obs_all,
       sim_summary = sim_summary_all)
}

# 2. Function to plot combined (r00 vs c0) for all families
plot_all_families <- function(proc_res, x_label = "") {
  df_obs      <- proc_res$df_obs
  sim_summary <- proc_res$sim_summary
  
  # -- Define shapes and fill colors
  shapes <- c(triangle = 24, square = 22, circle = 21)
  fills  <- c(triangle = "#64B5F6", square = "#FF6961", circle = "grey")
  
  # -- Plot for baseline r00
  p_r00 <- ggplot() +
    geom_segment(
      data = sim_summary %>% filter(Baseline == "r00"),
      aes(x = lower, xend = upper, y = Family, yend = Family),
      color = "#A9A9A9", size = 0.8
    ) +
    geom_point(
      data = df_obs %>% filter(Baseline == "r00"),
      aes(x = Value, y = Family, shape = shape, fill = shape),
      size = 3.5, color = "black"
    ) +
    scale_shape_manual(values = shapes, guide = "none") +
    scale_fill_manual(values = fills, guide = "none") +
    labs(x = x_label, y = NULL, title = "r00") +
    theme_minimal() +
    theme(
      axis.text.y = element_markdown(),
      plot.title  = element_text(hjust = 0.5)
    )
  
  # -- Plot for baseline c0
  p_c0 <- ggplot() +
    geom_segment(
      data = sim_summary %>% filter(Baseline == "c0"),
      aes(x = lower, xend = upper, y = Family, yend = Family),
      color = "#A9A9A9", size = 0.8
    ) +
    geom_point(
      data = df_obs %>% filter(Baseline == "c0"),
      aes(x = Value, y = Family, shape = shape, fill = shape),
      size = 3.5, color = "black"
    ) +
    scale_shape_manual(values = shapes, guide = "none") +
    scale_fill_manual(values = fills, guide = "none") +
    labs(x = x_label, title = "c0") +
    theme_minimal() +
    theme(
      axis.text.y   = element_blank(),
      axis.ticks.y  = element_blank(),
      plot.title    = element_text(hjust = 0.5)
    )
  
  # -- Combine with patchwork
  combined_plot <- (p_r00 + p_c0) +
    plot_layout(ncol = 2) &
    theme(legend.position = "bottom")
  
  return(combined_plot)
}

# 3. Call for all families in the working directory ----

# Process every nestedness_<family> folder
res_all <- process_all_csv_results(
  measure_col = "stat_nodf_general",  # or "stat_temp"
  alpha       = 0.05
)

# Plot the combined panels for r00 vs c0
plot_all_families(
  res_all,
  x_label = "Generalized NODF (%)"
)
