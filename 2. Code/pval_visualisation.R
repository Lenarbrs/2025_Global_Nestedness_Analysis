library(tidyverse)
library(ggplot2)

# Read the consolidated results file
results <- read_csv("nest_pvalue_all.csv")

# Plot 1: Counts of sign_nodf and sign_temp values
plot1 <- results %>%
  pivot_longer(cols = c(sign_NODF, sign_Temp), 
               names_to = "metric", 
               values_to = "significance") %>%
  mutate(metric = recode(metric, 
                         "sign_NODF" = "NODF", 
                         "sign_Temp" = "Temperature")) %>%
  ggplot(aes(x = significance, fill = metric)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("NODF" = "#1f77b4", "Temperature" = "#ff7f0e")) +
  labs(title = "Distribution of Nestedness Significance",
       x = "Significance",
       y = "Count",
       fill = "Metric") +
  theme_minimal() +
  theme(legend.position = "top")

# Create agreement column
results <- results %>%
  mutate(agreement = case_when(
    sign_NODF == sign_Temp ~ "Same",
    TRUE ~ "Different"
  ))

# Plot 2: Agreement between NODF and Temperature metrics
plot2 <- results %>%
  ggplot(aes(x = agreement, fill = agreement)) +
  geom_bar() +
  scale_fill_manual(values = c("Same" = "#2ca02c", "Different" = "#d62728")) +
  labs(title = "Agreement Between NODF and Temperature Metrics",
       x = "Agreement",
       y = "Count") +
  theme_minimal() +
  theme(legend.position = "none")

# Save plots
ggsave("significance_distribution.png", plot1, width = 8, height = 6)
ggsave("metric_agreement.png", plot2, width = 6, height = 6)

# Display plots
print(plot1)
print(plot2)


library(tidyverse)
library(ggplot2)

# Read the consolidated results file
results <- read_csv("nest_pvalue_all.csv")

# Create significance categories and agreement column
results <- results %>%
  mutate(
    # Convert p-values to numeric (they were saved as strings with 4 decimals)
    p_NODF = as.numeric(p_NODF),
    p_Temp = as.numeric(p_Temp),
    
    # Create significance categories
    nodf_sig = if_else(p_NODF < 0.05, "Significant", "Non-significant"),
    temp_sig = if_else(p_Temp < 0.05, "Significant", "Non-significant"),
    
    # Create combined significance category
    sig_combo = case_when(
      nodf_sig == "Significant" & temp_sig == "Significant" ~ "Both significant",
      nodf_sig == "Significant" & temp_sig == "Non-significant" ~ "Only NODF significant",
      nodf_sig == "Non-significant" & temp_sig == "Significant" ~ "Only Temp significant",
      TRUE ~ "Both non-significant"
    ),
    
    # Create agreement column
    agreement = if_else(sign_NODF == sign_Temp, "Same", "Different")
  )

# Create the plot
plot <- results %>%
  ggplot(aes(x = sig_combo, fill = agreement)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("Same" = "#2ca02c", "Different" = "#d62728")) +
  labs(title = "Agreement Between NODF and Temperature Metrics by Significance",
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

# Save the plot
ggsave("significance_agreement_plot.png", plot, width = 8, height = 6)

# Display the plot
print(plot)

library(tidyverse)
library(ggplot2)
library(viridis)  # For better color scales

# Read the consolidated results file
results <- read_csv("nest_pvalue_all.csv") %>%
  mutate(p_NODF = as.numeric(p_NODF),
         p_Temp = as.numeric(p_Temp))

# Get unique matrix IDs
matrix_ids <- unique(results$matrix_id)

# Create a heatmap for each matrix
for (matrix_id in matrix_ids) {
  # Filter data for current matrix
  matrix_data <- results %>%
    filter(matrix_id == !!matrix_id) %>%
    select(baseline, p_NODF, p_Temp) %>%
    pivot_longer(cols = c(p_NODF, p_Temp),
                 names_to = "metric",
                 values_to = "p_value") %>%
    mutate(metric = recode(metric,
                           "p_NODF" = "NODF",
                           "p_Temp" = "TEMP"),
           baseline = as.factor(baseline))
  
  # Create heatmap
  heatmap <- ggplot(matrix_data, aes(x = metric, y = baseline, fill = p_value)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = sprintf("%.3f", p_value)), color = "white", size = 3) +
    scale_fill_viridis(option = "plasma", direction = -1, limits = c(0, 1)) +
    labs(title = paste("P-values for Matrix:", matrix_id),
         x = "Metric",
         y = "Baseline",
         fill = "P-value") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
          plot.title = element_text(hjust = 0.5),
          panel.grid = element_blank())
  
  # Save heatmap
  ggsave(paste0("heatmap_", matrix_id, ".png"), 
         heatmap, 
         width = 6, 
         height = 6,
         dpi = 300)
  
  # Print progress
  message(paste("Created heatmap for matrix:", matrix_id))
}

message("All heatmaps created successfully!")

