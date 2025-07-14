#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============ Significance Analysis & Heatmaps =============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# This script generates visual summaries of the significance analysis results
# from the global nestedness study. It produces:
# - Bar plots showing distributions of significance labels for NODF and TEMP
# - Agreement analysis between NODF and TEMP metrics
# - Standard and color-coded heatmaps of p-values for each matrix and baseline
#
# Requirements:
# - The file 'nest_pvalue_all.csv' must be present in the working directory
# - Output includes multiple .png files saved for plots and heatmaps

## ==== 1. Plot Distribution and Agreement of Significance Labels ====
### ---- A. Load Packages ----
library(tidyverse)  # For data wrangling and plotting
library(ggplot2)    # For base plotting functions

### ---- B. Read Results ----
# Load the full p-value results table
results <- read_csv("nest_pvalue_all.csv")

### ---- C. Plot: Counts per Significance Label ----
# This block reshapes the data and visualizes the frequency
# of significance labels for both NODF and TEMP metrics
plot1 <- results %>%
  pivot_longer(cols = c(sign_NODF, sign_Temp),  # convert wide to long format
               names_to = "metric",
               values_to = "significance") %>%
  mutate(metric = recode(metric,               # rename for plot readability
                         "sign_NODF" = "NODF",
                         "sign_Temp" = "Temperature")) %>%
  ggplot(aes(x = significance, fill = metric)) +
  geom_bar(position = "dodge") +               # side-by-side bars
  scale_fill_manual(values = c("NODF" = "#1f77b4", "Temperature" = "#ff7f0e")) +
  labs(title = "Distribution of Nestedness Significance",
       x = "Significance",
       y = "Count",
       fill = "Metric") +
  theme_minimal() +
  theme(legend.position = "top")

### ---- D. Create Agreement Column ----
# Add column indicating whether both metrics agree on direction
results <- results %>%
  mutate(agreement = if_else(sign_NODF == sign_Temp, "Same", "Different"))

### ---- E. Plot: Agreement Count ----
# Visualize number of matrices with same vs. different significance direction
plot2 <- results %>%
  ggplot(aes(x = agreement, fill = agreement)) +
  geom_bar() +
  scale_fill_manual(values = c("Same" = "#2ca02c", "Different" = "#d62728")) +
  labs(title = "Agreement Between NODF and Temperature Metrics",
       x = "Agreement",
       y = "Count") +
  theme_minimal() +
  theme(legend.position = "none")

### ---- F. Save Plots ----
# Export the two bar plots to PNG files
ggsave("significance_distribution.png", plot1, width = 8, height = 6)
ggsave("metric_agreement.png", plot2, width = 6, height = 6)

### ---- G. Display Plots ----
# Print the plots to the RStudio viewer
print(plot1)
print(plot2)

## ==== 2. Plot Agreement by Significance Status ====
# Reload data to start clean and create new classification columns
results <- read_csv("nest_pvalue_all.csv") %>%
  mutate(
    p_NODF = as.numeric(p_NODF),        # convert p-values from string to numeric
    p_Temp = as.numeric(p_Temp),
    nodf_sig = if_else(p_NODF < 0.05, "Significant", "Non-significant"),
    temp_sig = if_else(p_Temp < 0.05, "Significant", "Non-significant"),
    
    # Create a combined label for all combinations of significance status
    sig_combo = case_when(
      nodf_sig == "Significant" & temp_sig == "Significant" ~ "Both significant",
      nodf_sig == "Significant" & temp_sig == "Non-significant" ~ "Only NODF significant",
      nodf_sig == "Non-significant" & temp_sig == "Significant" ~ "Only Temp significant",
      TRUE ~ "Both non-significant"
    ),
    
    # Label agreement again for this context
    agreement = if_else(sign_NODF == sign_Temp, "Same", "Different")
  )

# Create grouped bar plot showing agreement within each significance combo group
plot3 <- results %>%
  ggplot(aes(x = sig_combo, fill = agreement)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("Same" = "#2ca02c", "Different" = "#d62728")) +
  labs(title = "Agreement by Significance Category",
       x = "Significance Combination",
       y = "Count",
       fill = "Agreement") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top") +
  scale_x_discrete(limits = c("Both significant", 
                              "Only NODF significant", 
                              "Only Temp significant", 
                              "Both non-significant"))

# Save the grouped agreement plot
ggsave("significance_agreement_plot.png", plot3, width = 8, height = 6)
# Display plot
print(plot3)

## ==== 3. Standard Heatmaps (p-values) ====
library(viridis)  # For color scale

# Reload results to ensure fresh p-value columns
results <- read_csv("nest_pvalue_all.csv") %>%
  mutate(p_NODF = as.numeric(p_NODF),
         p_Temp = as.numeric(p_Temp))

# Get all unique matrix IDs for heatmap generation
matrix_ids <- unique(results$matrix_id)

# Iterate through matrices and plot p-values in heatmap layout
for (matrix_id in matrix_ids) {
  matrix_data <- results %>%
    filter(matrix_id == !!matrix_id) %>%
    select(baseline, p_NODF, p_Temp) %>%
    pivot_longer(cols = c(p_NODF, p_Temp), names_to = "metric", 
                 values_to = "p_value") %>%
    mutate(metric = recode(metric, "p_NODF" = "NODF", "p_Temp" = "TEMP"),
           baseline = as.factor(baseline))
  
  # Construct a heatmap for NODF and TEMP p-values
  heatmap <- ggplot(matrix_data, aes(x = metric, y = baseline, fill = p_value)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = sprintf("%.3f", p_value)), color = "white", size = 3) +
    scale_fill_viridis(option = "plasma", direction = -1, limits = c(0, 1)) +
    labs(title = paste("P-values for Matrix:", matrix_id), x = "Metric", 
         y = "Baseline", fill = "P-value") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
          plot.title = element_text(hjust = 0.5),
          panel.grid = element_blank())
  
  # Save plot to file
  ggsave(paste0("heatmap_", matrix_id, ".png"), heatmap, width = 6, height = 6, dpi = 300)
  message(paste("Created heatmap for matrix:", matrix_id))
}

# Final message when all are completed
message("All heatmaps created successfully!")
