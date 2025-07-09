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