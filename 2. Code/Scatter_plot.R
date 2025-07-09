# Install and load necessary packages
if (!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)

# 1) Read p-values (standard CSV)
pvalues <- read_csv("nest_pvalue_all.csv")

# 2) Read metadata (semicolon-delimited CSV)
metadata <- read_delim(
  "meta-data-matrices_all.csv",
  delim = ";",
  col_types = cols(
    Group           = col_character(),
    File            = col_character(),
    Rows            = col_double(),
    Columns         = col_double(),
    Fill_Percentage = col_double(),
    Size            = col_double()
  )
)

# 3) Clean and extract base matrix_id from filenames
metadata <- metadata %>%
  mutate(
    matrix_id = File %>%
      str_remove("^(cleaned_)?(matrix_)?") %>%
      str_remove("(_bin)?\\.csv$"),
    matrix_size = Size,
    group       = Group
  )

# 4) Join tables on matrix_id
df <- pvalues %>%
  left_join(metadata %>% select(matrix_id, matrix_size, group), by = "matrix_id")

# 5) Scatter plot of Temp p-values vs. matrix size (log scale), colored by group, faceted by baseline
library(ggplot2)
ggplot(df, aes(x = matrix_size, y = p_NODF, color = group)) +
  geom_point(alpha = 0.7) +
  facet_wrap(~ baseline, scales = "free") +
  
  labs(
    x        = "Matrix size",
    y        = "P-value (NODF)",
    color    = "Group",
    title    = "Nodf P-values vs matrix size",
  ) +
  theme_minimal()

# Save the plot to a file
ggsave(
  filename = "nodf_scatter_plot.png",
  plot     = last_plot(),
  width    = 10,
  height   = 6,
  dpi      = 300,
  bg = "white"
)
