library(tidyverse)
library(ggplot2)

# Read the consolidated results file
results <- read_csv("nest_pvalue_all.csv")

# Plot 1: Counts of sign_nodf and sign_temp values ====
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

# Plot 2: Agreement between NODF and Temperature metrics ====
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


# 3. Agreement depending on significancy ====
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


# 4. Heatmaps ====
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

# 5. Heatmaps side ====
library(tidyverse)
library(ggplot2)
library(colorspace)  # For better color manipulation

# Read and prepare data
results <- read_csv("nest_pvalue_all.csv") %>%
  mutate(p_NODF = as.numeric(p_NODF),
         p_Temp = as.numeric(p_Temp),
         # Create combined direction column
         direction_NODF = ifelse(sign_NODF == "nested", "nested", 
                                 ifelse(sign_NODF == "antinested", "antinested", "neutral")),
         direction_Temp = ifelse(sign_Temp == "nested", "nested",
                                 ifelse(sign_Temp == "antinested", "antinested", "neutral")))

# Custom color function - vectorized version
dual_color_scale <- function(p_value, direction) {
  # Initialize color vector
  colors <- character(length(p_value))
  
  # Process nested cases
  nested_idx <- which(direction == "nested")
  if (length(nested_idx) > 0) {
    lightness <- 50 + 50 * p_value[nested_idx]  # Scale from 50-100 based on p-value
    colors[nested_idx] <- hcl(h = 40, c = 100, l = lightness)  # Orange tones
  }
  
  # Process antinested cases
  antinested_idx <- which(direction == "antinested")
  if (length(antinested_idx) > 0) {
    lightness <- 50 + 50 * p_value[antinested_idx]  # Scale from 50-100 based on p-value
    colors[antinested_idx] <- hcl(h = 120, c = 100, l = lightness)  # Green tones
  }
  
  # Process neutral cases
  neutral_idx <- which(direction == "neutral")
  if (length(neutral_idx) > 0) {
    colors[neutral_idx] <- "#CCCCCC"  # Light gray
  }
  
  return(colors)
}

# Create heatmaps for each matrix
for (matrix_id in unique(results$matrix_id)) {
  # Prepare data for current matrix
  plot_data <- results %>%
    filter(matrix_id == !!matrix_id) %>%
    pivot_longer(cols = c(p_NODF, p_Temp),
                 names_to = "metric",
                 values_to = "p_value") %>%
    mutate(
      metric = recode(metric, "p_NODF" = "NODF", "p_Temp" = "TEMP"),
      direction = ifelse(metric == "NODF", direction_NODF, direction_Temp),
      # Apply color scaling
      tile_color = dual_color_scale(p_value, direction)
    )
  
  # Create heatmap
  p <- ggplot(plot_data, aes(x = metric, y = as.factor(baseline))) +
    geom_tile(aes(fill = I(tile_color)), color = "white", linewidth = 0.5) +
    geom_text(aes(label = sprintf("%.2f", p_value)), 
              color = ifelse(plot_data$p_value < 0.5, "white", "black"), 
              size = 3) +
    scale_y_discrete(limits = rev) +
    labs(title = paste("Matrix:", matrix_id),
         subtitle = "Orange: Nested | Green: Antinested | Brightness: P-value",
         x = "Metric",
         y = "Baseline") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5, size = 8),
          panel.grid = element_blank())
  
  # Save plot
  ggsave(paste0("dual_heatmap_", matrix_id, ".png"), 
         p, width = 6, height = 6, dpi = 300)
  
  message(paste("Created heatmap for:", matrix_id))
}

message("All dual-color heatmaps created successfully!")

## 5. bis Heatmaps side legend ====
library(tidyverse)
library(ggplot2)
library(RColorBrewer)

# Read and prepare data
results <- read_csv("nest_pvalue_all.csv") %>%
  mutate(p_NODF = as.numeric(p_NODF),
         p_Temp = as.numeric(p_Temp),
         direction_NODF = sign_NODF,
         direction_Temp = sign_Temp)

# Create heatmaps for each matrix
for (matrix_id in unique(results$matrix_id)) {
  # Prepare data for current matrix
  plot_data <- results %>%
    filter(matrix_id == !!matrix_id) %>%
    pivot_longer(cols = c(p_NODF, p_Temp),
                 names_to = "metric",
                 values_to = "p_value") %>%
    mutate(
      metric = recode(metric, "p_NODF" = "NODF", "p_Temp" = "TEMP"),
      direction = ifelse(metric == "NODF", direction_NODF, direction_Temp),
      # Create combined label with p-value and direction
      cell_label = sprintf("%.2f\n%s", p_value, direction)
    )
  
  # Create heatmap
  p <- ggplot(plot_data, aes(x = metric, y = as.factor(baseline), fill = p_value)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = cell_label), color = "black", size = 3) +
    scale_fill_distiller(palette = "YlOrRd", direction = -1, 
                         limits = c(0, 1), name = "p-value") +
    scale_y_discrete(limits = rev) +
    labs(title = paste("Matrix:", matrix_id),
         x = "Metric",
         y = "Baseline") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
          plot.title = element_text(hjust = 0.5),
          panel.grid = element_blank(),
          legend.position = "right") +
    guides(fill = guide_colorbar(barheight = unit(2, "inches")))
  
  # Save plot
  ggsave(paste0("pvalue_heatmap_", matrix_id, ".png"), 
         p, width = 6, height = 6, dpi = 300)
  
  message(paste("Created heatmap for:", matrix_id))
}

message("All heatmaps created successfully!")


## 5. ter. Heatmaps 0 to 2 ====
library(tidyverse)
library(ggplot2)
library(RColorBrewer)

# Read and prepare data
results <- read_csv("nest_pvalue_all.csv") %>%
  mutate(p_NODF = as.numeric(p_NODF),
         p_Temp = as.numeric(p_Temp),
         direction_NODF = sign_NODF,
         direction_Temp = sign_Temp)

# Create heatmaps for each matrix
for (matrix_id in unique(results$matrix_id)) {
  # Prepare data for current matrix with transformed p-values
  plot_data <- results %>%
    filter(matrix_id == !!matrix_id) %>%
    pivot_longer(cols = c(p_NODF, p_Temp),
                 names_to = "metric",
                 values_to = "p_value") %>%
    mutate(
      metric = recode(metric, "p_NODF" = "NODF", "p_Temp" = "TEMP"),
      direction = ifelse(metric == "NODF", direction_NODF, direction_Temp),
      # Transform p-values: antinested becomes 2 - p_value
      transformed_p = ifelse(direction == "antinested", 2 - p_value, p_value),
      # Create combined label with original p-value and direction
      cell_label = sprintf("%.2f\n%s", p_value, direction)
    )
  
  # Create heatmap with transformed p-values
  p <- ggplot(plot_data, aes(x = metric, y = as.factor(baseline), fill = transformed_p)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = cell_label), color = "black", size = 3) +
    scale_fill_gradientn(
      colors = brewer.pal(9, "RdYlBu"),
      limits = c(0, 2),
      breaks = c(0, 1, 2),
      labels = c("0 (sig. nested)", "1 (neutral)", "2 (sig. antinested)"),
      name = "Significance"
    ) +
    scale_y_discrete(limits = rev) +
    labs(title = paste("Matrix:", matrix_id),
         x = "Metric",
         y = "Baseline") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
          plot.title = element_text(hjust = 0.5),
          panel.grid = element_blank(),
          legend.position = "right") +
    guides(fill = guide_colorbar(barheight = unit(2, "inches")))
  
  # Save plot
  ggsave(paste0("transformed_pvalue_heatmap_", matrix_id, ".png"), 
         p, width = 6.5, height = 6, dpi = 300)
  
  message(paste("Created heatmap for:", matrix_id))
}

message("All transformed p-value heatmaps created successfully!")

