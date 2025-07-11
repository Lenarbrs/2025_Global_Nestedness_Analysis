
# 1. Bivariate plot ----
# Install and load necessary packages
if (!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)

# 1) Read p-values (standard CSV)
pvalues <- read_csv("nest_pvalue_all.csv")

metadata <- read_delim(
  "meta-data-matrices_all.csv",
  delim = ";",
  locale = locale(decimal_mark = ","),  # if your file uses commas as decimals
  col_types = cols(
    Group = col_character(),
    File = col_character(),
    Rows = col_double(),
    Columns = col_double(),
    Fill_Percentage = col_double(),
    Size = col_double()
  )
)

# 3) Clean and extract base matrix_id from filenames
metadata <- metadata %>%
  mutate(
    matrix_id = File %>%
      str_remove("^(cleaned_)?(matrix_)?(bin_)?") %>%
      str_remove("(_bin)?\\.csv$"),
    matrix_fill = Fill_Percentage,
    group       = Group
  )
print(metadata)
# 4) Join tables on matrix_id
df <- pvalues %>%
  left_join(metadata %>% select(matrix_id, matrix_fill, group), by = "matrix_id")
print(df)

# 5) Scatter plot of Temp p-values vs. matrix size (log scale), colored by group, faceted by baseline
library(ggplot2)
ggplot(df, aes(x = matrix_fill, y = p_Temp, color = group)) +
  geom_point(alpha = 0.7) +
  facet_wrap(~ baseline, scales = "free") +
  
  labs(
    x        = "Matrix fill",
    y        = "P-value (TEMP)",
    color    = "Group",
    title    = "Temp P-values vs matrix fill",
  ) +
  theme_minimal()

# Save the plot to a file
ggsave(
  filename = "temp_scatter_plot_fill.png",
  plot     = last_plot(),
  width    = 10,
  height   = 6,
  dpi      = 300,
  bg = "white"
)

# 2. Three variables plot ----
# Install and load necessary packages
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("plotly")) install.packages("plotly") # For 3D plots
library(tidyverse)
library(plotly)

# 1) Read p-values (standard CSV)
pvalues <- read_csv("nest_pvalue_all.csv")

metadata <- read_delim(
  "meta-data-matrices_all.csv",
  delim = ";",
  locale = locale(decimal_mark = ","),  # if your file uses commas as decimals
  col_types = cols(
    Group = col_character(),
    File = col_character(),
    Rows = col_double(),
    Columns = col_double(),
    Fill_Percentage = col_double(),
    Size = col_double()
  )
)

# 3) Clean and extract base matrix_id from filenames
metadata <- metadata %>%
  mutate(
    matrix_id = File %>%
      str_remove("^(cleaned_)?(matrix_)?(bin_)?") %>%
      str_remove("(_bin)?\\.csv$"),
    matrix_fill = Fill_Percentage,
    group = Group,
    matrix_size = Size
  )

# 4) Join tables on matrix_id
df <- pvalues %>%
  left_join(metadata %>% select(matrix_id, matrix_fill, group, matrix_size), by = "matrix_id")

# 5) Enhanced Scatter plot with size representing matrix size
ggplot(df, aes(x = matrix_fill, y = p_Temp, color = group, size = matrix_size)) +
  geom_point(alpha = 0.5) +
  scale_size_continuous(range = c(1, 10)) +  # REMOVED THE COMMA AFTER THIS LINE
  facet_wrap(~ baseline, scales = "free") +
  labs(
    x = "Matrix fill",
    y = "P-value (TEMP)",
    color = "Group",
    size = "Matrix Size",
    title = "Temp P-values vs matrix fill (dot size = matrix size)"
  ) +
  theme_minimal()

# Save the enhanced scatter plot
ggsave(
  filename = "temp_scatter_plot_fill_size.png",
  plot = last_plot(),
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)

# 6) Create 3D plots for each baseline
# First, create a list to store plots
plot_list <- list()

# Get unique baseline values
baselines <- unique(df$baseline)

# Create a 3D plot for each baseline
for (bl in baselines) {
  df_subset <- df %>% filter(baseline == bl)
  
  p <- plot_ly(df_subset, 
               x = ~matrix_fill, 
               y = ~matrix_size, 
               z = ~p_Temp,
               color = ~group,
               type = "scatter3d",
               mode = "markers",
               marker = list(size = 5, opacity = 0.7),
               text = ~paste("Matrix ID:", matrix_id, "<br>Fill:", matrix_fill, 
                             "<br>Size:", matrix_size, "<br>P-value:", p_Temp)) %>%
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

# Save each 3D plot as an HTML file
for (bl in names(plot_list)) {
  htmlwidgets::saveWidget(plot_list[[bl]], 
                          file = paste0("3d_plot_baseline_", gsub("[^[:alnum:]]", "_", bl), ".html"),
                          selfcontained = TRUE)
}

plot_list[[5]]