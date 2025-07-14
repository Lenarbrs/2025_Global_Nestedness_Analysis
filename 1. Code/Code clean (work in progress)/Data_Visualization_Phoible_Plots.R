#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============ Data Visualization Phoible Plots =============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# This script generates the plots like in the Phoible study, showing 
# the distribution of simulated values alongside the nestedness value of empirical
# matrices for each group analyzed individually (Phoible, Trivia, Archeology, etc.).
# 
# To run this code, you need to set the working directory to the Results dataset 
# folder from the GitHub repository, then select the subfolder corresponding to 
# the desired group (e.g., phoible, trivia, archeology, etc.). The code will 
# produce the plot for that specific group.
# 
# To generate the plot for a different group, you simply need to change the 
# working directory to the corresponding subfolder of that group 
# (still within the Results dataset folder on GitHub).

## ==== 1. Load Libraries ==== 
### ---- A. Load libraries ----
library(tidyverse)
library(ggtext)
library(patchwork)

## ==== 2. Functions for Processing Results ====
### ---- A. process_all_csv_results ----
# - Reads real summaries and simulations for each family
# - Computes confidence intervals and assigns significance shapes
process_all_csv_results <- function(measure_col = c("stat_nodf_general", 
                                                    "stat_temp"),
                                    alpha = 0.005) {
  measure_col <- match.arg(measure_col)
  dirs <- list.files(pattern = "^nestedness_")
  family_ids <- sub("^nestedness_", "", dirs)
  all_obs <- list()
  for (fam in family_ids) {
    # Read real-world summary
    df_real <- read.csv2(file.path(paste0("nestedness_", fam),
                                   paste0("nest_summary_", fam, ".csv")),
                         stringsAsFactors = FALSE) %>%
      transmute(
        Family  = fam,
        n_langs = num_rows,
        Value   = .data[[measure_col]]
      )
    # Read simulated outputs
    df_sim <- read.csv2(file.path(paste0("nestedness_", fam),
                                  paste0("nest_simulated_", fam, "_all.csv")),
                        stringsAsFactors = FALSE)
    # Summarise simulations
    sim_summary <- df_sim %>%
      group_by(baseline) %>%
      summarise(
        Family   = fam,
        Sim_Mean = mean(.data[[measure_col]], na.rm = TRUE),
        lower    = quantile(.data[[measure_col]], probs = alpha/2, na.rm = TRUE),
        upper    = quantile(.data[[measure_col]], probs = 1 - alpha/2, na.rm = TRUE),
        .groups  = "drop"
      ) %>%
      rename(Baseline = baseline)
    # Read p-values and significance side
    df_p <- read.csv(file.path(paste0("nestedness_", fam),
                               paste0("nest_pvalue_", fam, ".csv")),
                     stringsAsFactors = FALSE)
    p_col  <- if (measure_col == "stat_nodf_general") "p_NODF" else "p_Temp"
    sign_col <- if (measure_col == "stat_nodf_general") "sign_NODF" else "sign_Temp"
    df_p2  <- df_p %>%
      transmute(
        Baseline         = baseline,
        p_value          = .data[[p_col]],
        Significant_Side = .data[[sign_col]]
      )
    # Merge everything and assign shapes
    df_obs <- sim_summary %>%
      left_join(df_real, by = "Family") %>%
      left_join(df_p2,    by = "Baseline") %>%
      mutate(
        significant = p_value < alpha,
        shape = case_when(
          !significant ~ "circle",
          tolower(Significant_Side) == "nested"  ~ "triangle",
          tolower(Significant_Side) == "antinested"  ~ "square",
          TRUE                                         ~ "circle"
        )
      )
    all_obs[[fam]] <- df_obs
  }
  # Combine and factor levels
  df_obs_all   <- bind_rows(all_obs)
  fam_levels   <- df_obs_all %>% distinct(Family, n_langs) %>% arrange(n_langs) %>% pull(Family)
  df_obs_all$Family   <- factor(df_obs_all$Family, levels = fam_levels)
  df_obs_all$Baseline <- factor(df_obs_all$Baseline,
                                levels = c("r00","r0","r1","r2","c0","c1","curveball","swap"))
  df_obs_all
}

## ==== 3. Plotting Function ====
### ---- A. plot_all_families ----
# - Faceted plot: CIs as segments, observed points with shape fill
plot_all_families <- function(df_obs, x_label = "") {
  shapes <- c(circle = 21, triangle = 24, square = 22)
  fills  <- c(circle = "grey", triangle = "#64B5F6", square = "#FF6961")
  ggplot(df_obs) +
    geom_segment(aes(x = lower, xend = upper, y = Family, yend = Family),
                 color = "#A9A9A9", size = 0.8) +
    geom_point(aes(x = Value, y = Family, shape = shape, fill = shape),
               size = 3.5, color = "black") +
    scale_shape_manual(values = shapes, name = "Significance",
                       labels = c(circle = "Not significant",
                                  triangle = "Nested",
                                  square = "Antinested")) +
    scale_fill_manual(values = fills, name = "Significance",
                      labels = c(circle = "Not significant",
                                 triangle = "Nested",
                                 square = "Antinested")) +
    scale_y_discrete(labels = function(f) f) +
    scale_x_continuous(limits = c(0, 100)) +
    facet_wrap(~Baseline, ncol = 4) +
    labs(x = x_label, y = NULL) +
    theme_minimal() +
    theme(
      axis.text.y     = element_markdown(),
      strip.text      = element_text(face = "bold", size = 11),
      axis.title.x    = element_text(size = 13),
      legend.position = "bottom"
    )
}

## ==== 4. Execute ====
# process data and generate plot
# choose the metric and set the R working directory in the folder you want 
# (Archeology, Trivia, Phoible, etc.)

# Call processing function
df_all <- process_all_csv_results(measure_col = "stat_nodf_general", alpha = 0.005)
# Create plot
final_plot <- plot_all_families(df_all, x_label = "NODF")
# Display
print(final_plot)

## ==== 5. Save Output ====
# export plot to PNG file
ggsave("archeology_all_baselines_nodf_plot.png",
       plot = final_plot,
       width = 7,
       height = 5,
       dpi = 300,
       bg = "white")

