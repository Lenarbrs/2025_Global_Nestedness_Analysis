#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Plot Size Matrices
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# This script reads metadata for all matrices and generates a horizontal bar plot
# showing the number of cells (Size) per matrix, sorted in descending order.
# The plot uses a log10 scale for size and is color-coded by group.
#
# Input:
# - meta-data-matrices_all.csv (semicolon-separated CSV file)
#
# Output:
# - Displayed and save ggplot chart 

# ==== 1. Load Libraries ====
library(dplyr)      
library(ggplot2)    

# ==== 2. Load Metadata File ====
# Reads matrix metadata including group, size, and file name
df <- read.csv("meta-data-matrices_all.csv", sep = ";", stringsAsFactors = FALSE)

# ==== 3. Sort Matrices by Size ====
# Arrange descending by cell count to rank matrices
df_top50 <- df %>%
  arrange(desc(Size))

# ==== 4. Plot Bar Chart ====
# Create horizontal bar chart with log10 Y scale
p <- ggplot(df_top50, aes(x = reorder(File, Size), y = Size, fill = Group)) +
  geom_col() +
  scale_fill_manual(values = c(
    "Movies"          = "plum",
    "Archeology"      = "lightgoldenrod1",
    "Phoible"         = "paleturquoise3",
    "Orangutan"       = "tan1",
    "Chimpanzee"      = "tan2",
    "Human"           = "tan3",
    "Trivia"          = "tomato",
    "Plant Knowledge" = "darkolivegreen2"
  )) +
  coord_flip() +
  scale_y_log10() +
  labs(
    title = "Matrices by number of cells (log10 scale)",
    x     = NULL,
    y     = "Cell Count (log10)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.y      = element_text(size = 6),
    panel.grid.minor = element_blank(),
    legend.position  = "right"
  )

# ==== 5. Display Plot ====
print(p)

# ==== 6. Save Plot (optional) ====
ggsave(
  filename = "matrices_cell_count_log10.png",
  plot = p,
  width = 10,
  height = 10,
  dpi = 300,
  bg = "white"
)