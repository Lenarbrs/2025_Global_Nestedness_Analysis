#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============ P-Value Distribution Visualization =============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# This script creates a series of visualizations to explore the distribution of 
# p-values across matrix characteristics and null model baselines. 
#
# Specifically, it:
# - Generates scatter plots of p-values vs. matrix fill
# - Other scatter plots by adding matrix size as a visual variable
# - Produces 3D scatter plots showing fill, size, and p-values
#
# To run this script:
# - Place it in the Results dataset directory (from the GitHub repository)
# - Make sure the required files are present: 'nest_pvalue_all.csv' and 
#   'meta-data-matrices_all.csv'
# - Output will be PNG and HTML visualizations saved to the working directory
# - This script is for temp p values only, if you want to do it for NODF p values, 
#   you just need to change the "p_Temp" by "p_NODF"


## ==== 1. Bivariate Plot: p-values vs Matrix Fill ====
### ---- A. Load Required Packages ----
if (!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)

### ---- B. Read Input Data ----
pvalues <- read_csv("nest_pvalue_all.csv")
metadata <- read_delim(
  "meta-data-matrices_all.csv",
  delim = ";",
  locale = locale(decimal_mark = ","),
  col_types = cols(
    Group = col_character(),
    File = col_character(),
    Rows = col_double(),
    Columns = col_double(),
    Fill_Percentage = col_double(),
    Size = col_double()
  )
)

### ---- C. Clean Metadata and Merge ----
metadata <- metadata %>%
  mutate(
    matrix_id = File %>%
      str_remove("^(cleaned_)?(matrix_)?(bin_)?") %>%
      str_remove("(_bin)?\\.csv$"),
    matrix_fill = Fill_Percentage,
    group       = Group
  )

df <- pvalues %>%
  left_join(metadata %>% select(matrix_id, matrix_fill, group), by = "matrix_id")

### ---- D. Plot p-values by Fill and Group ----
ggplot(df, aes(x = matrix_fill, y = p_Temp, color = group)) +
  geom_point(alpha = 0.7) +
  facet_wrap(~ baseline, scales = "free") +
  labs(
    x     = "Matrix fill",
    y     = "P-value (TEMP)",
    color = "Group",
    title = "Temp P-values vs Matrix Fill"
  ) +
  theme_minimal()

### ---- E. Save Plot ----
ggsave(
  filename = "temp_scatter_plot_fill.png",
  plot     = last_plot(),
  width    = 10,
  height   = 6,
  dpi      = 300,
  bg       = "white"
)

## ==== 2. Enhanced Bivariate Plot: Add Dot Size for Matrix Size ====
### ---- A. Load Additional Libraries ----
if (!require("plotly")) install.packages("plotly")
library(plotly)

### ---- B. Read and Clean Data (Repeat) ----
pvalues <- read_csv("nest_pvalue_all.csv")
metadata <- read_delim(
  "meta-data-matrices_all.csv",
  delim = ";",
  locale = locale(decimal_mark = ","),
  col_types = cols(
    Group = col_character(),
    File = col_character(),
    Rows = col_double(),
    Columns = col_double(),
    Fill_Percentage = col_double(),
    Size = col_double()
  )
)

metadata <- metadata %>%
  mutate(
    matrix_id   = File %>%
      str_remove("^(cleaned_)?(matrix_)?(bin_)?") %>%
      str_remove("(_bin)?\\.csv$"),
    matrix_fill = Fill_Percentage,
    group       = Group,
    matrix_size = Size
  )

df <- pvalues %>%
  left_join(metadata %>% select(matrix_id, matrix_fill, group, matrix_size), 
            by = "matrix_id")

### ---- C. Plot with Size Aesthetic ----
ggplot(df, aes(x = matrix_fill, y = p_Temp, color = group, size = matrix_size)) +
  geom_point(alpha = 0.5) +
  scale_size_continuous(range = c(1, 10)) +
  facet_wrap(~ baseline, scales = "free") +
  labs(
    x     = "Matrix fill",
    y     = "P-value (TEMP)",
    color = "Group",
    size  = "Matrix Size",
    title = "Temp P-values vs Matrix Fill (Dot Size = Matrix Size)"
  ) +
  theme_minimal()

### ---- D. Save Plot ----
ggsave(
  filename = "temp_scatter_plot_fill_size.png",
  plot     = last_plot(),
  width    = 10,
  height   = 6,
  dpi      = 300,
  bg       = "white"
)

## ==== 3. 3D Plot for Each Baseline ====
### ---- A. Create 3D Scatter Plots using Plotly ----
plot_list <- list()
baselines <- unique(df$baseline)

for (bl in baselines) {
  df_subset <- df %>% filter(baseline == bl)
  
  p <- plot_ly(
    df_subset,
    x = ~matrix_fill,
    y = ~matrix_size,
    z = ~p_Temp,
    color = ~group,
    type = "scatter3d",
    mode = "markers",
    marker = list(size = 5, opacity = 0.7),
    text = ~paste("Matrix ID:", matrix_id,
                  "<br>Fill:", matrix_fill,
                  "<br>Size:", matrix_size,
                  "<br>P-value:", p_Temp)
  ) %>%
    layout(
      title = paste("3D Plot - Baseline:", bl),
      scene = list(
        xaxis = list(title = "Matrix Fill"),
        yaxis = list(title = "Matrix Size"),
        zaxis = list(title = "P-value (TEMP)")
      )
    )
  
  plot_list[[bl]] <- p
}

### ---- B. Export Each Plot as HTML ----
for (bl in names(plot_list)) {
  htmlwidgets::saveWidget(
    plot_list[[bl]],
    file = paste0("3d_plot_baseline_", gsub("[^[:alnum:]]", "_", bl), ".html"),
    selfcontained = TRUE
  )
}

