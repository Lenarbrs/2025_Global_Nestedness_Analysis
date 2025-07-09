# Draft data visualization ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Script for data visualization ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Load libraries ----
library(tidyverse)
library(ggtext)
library(patchwork)

# Function ----
# 1. Function to read all CSVs and create df_obs + sim_summary for every family,
#    taking p-values and significance direction from nest_pvalue_<family>.csv
process_all_csv_results <- function(measure_col = c("stat_nodf_general", "stat_temp"),
                                    alpha = 0.05) {
  measure_col <- match.arg(measure_col)
  
  # -- Find all nestedness_<family> directories
  dirs       <- list.files(pattern = "^nestedness_")
  family_ids <- sub("^nestedness_", "", dirs)
  
  all_obs <- list()
  
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
    
    # -- Compute simulated summaries for every baseline
    sim_summary <- df_sim %>%
      group_by(baseline) %>%
      summarise(
        Family   = fam,
        Sim_Mean = mean(.data[[measure_col]], na.rm = TRUE),
        lower    = quantile(.data[[measure_col]], probs = alpha/2,     na.rm = TRUE),
        upper    = quantile(.data[[measure_col]], probs = 1 - alpha/2, na.rm = TRUE),
        .groups  = "drop"
      ) %>%
      rename(Baseline = baseline)
    
    # -- Read p-values for this family
    df_p <- read.csv(
      file.path(paste0("nestedness_", fam),
                paste0("nest_pvalue_", fam, ".csv")),
      stringsAsFactors = FALSE
    )
    
    p_col    <- if (measure_col == "stat_nodf_general") "p_NODF" else "p_Temp"
    sign_col <- if (measure_col == "stat_nodf_general") "sign_NODF" else "sign_Temp"
    
    df_p2 <- df_p %>%
      transmute(
        Baseline         = baseline,
        p_value          = .data[[p_col]],
        Significant_Side = .data[[sign_col]]
      )
    
    # -- Build df_obs by joining sim_summary, df_real, and p-values
    df_obs <- sim_summary %>%
      left_join(df_real, by = "Family") %>%
      left_join(df_p2,    by = "Baseline") %>%
      mutate(
        significant = p_value < alpha,
        shape = case_when(
          !significant                                ~ "circle",   # not significant
          tolower(Significant_Side) == "nested"       ~ "triangle", # nested
          tolower(Significant_Side) == "antinested"   ~ "square",   # antinested
          TRUE                                        ~ "circle"
        )
      )
    
    all_obs[[fam]] <- df_obs
  }
  
  # -- Combine across families
  df_obs_all <- bind_rows(all_obs)
  
  # -- Order families by number of languages (ascending)
  fam_levels <- df_obs_all %>%
    distinct(Family, n_langs) %>%
    arrange(n_langs) %>%
    pull(Family)
  
  df_obs_all$Family <- factor(df_obs_all$Family, levels = fam_levels)
  # set baseline order explicitly
  df_obs_all$Baseline <- factor(df_obs_all$Baseline,
                                levels = c("r00","r0","r1","r2","c0","c1","curveball","swap")
  )
  
  df_obs_all
}

# 2. Function to plot combined for all baselines and families
plot_all_families <- function(df_obs, x_label = "") {
  # -- Define shapes and fill colors
  shapes <- c(circle = 21, triangle = 24, square = 22)
  fills  <- c(circle = "grey", triangle = "#64B5F6", square = "#FF6961")
  
  # -- Faceted plot with 2 rows: 4 panels per row
  p <- ggplot(df_obs) +
    # simulated intervals
    geom_segment(aes(x = lower, xend = upper,
                     y = Family, yend = Family),
                 color = "#A9A9A9", size = 0.8) +
    # observed points
    geom_point(aes(x = Value, y = Family,
                   shape = shape, fill = shape),
               size = 3.5, color = "black") +
    scale_shape_manual(
      values = shapes,
      name   = "Significance",
      labels = c(circle = "Not significant",
                 triangle = "Nested",
                 square = "Antinested")
    ) +
    scale_fill_manual(
      values = fills,
      name   = "Significance",
      labels = c(circle = "Not significant",
                 triangle = "Nested",
                 square = "Antinested")
    ) +
    scale_y_discrete(labels = function(fam) paste0(fam)) +
    scale_x_continuous(limits = c(0, 100)) +
    labs(x = x_label, y = NULL) +
    facet_wrap(~Baseline, ncol = 4) +    # 4 panels per row, 2 rows total
    theme_minimal() +
    theme(
      axis.text.y      = element_markdown(),
      strip.text       = element_text(face = "bold", size = 11),
      axis.title.x     = element_text(size = 13),    
      legend.position  = "bottom",
      legend.title     = element_text(size = 11),  
      legend.text      = element_text(size = 9)  
    )
  
  p
}

# 3. Call for all families in the working directory ----

df_obs_all <- process_all_csv_results(
  measure_col = "stat_nodf_general",  # "stat_temp" or "stat_nodf_general"
  alpha       = 0.005
)

final_plot <- plot_all_families(
  df_obs_all,
  x_label = "NODF"
)

final_plot

# 4. Save the combined plot to file ----
ggsave(
  filename = "archeology_all_baselines_nodf_plot.png",
  plot     = final_plot,
  width    = 7,
  height   = 5,
  dpi      = 300,
  bg       = "white"
)
