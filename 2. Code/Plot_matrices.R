# Load libraries
library(dplyr)
library(ggplot2)

# 1. Read metadata CSV (adjust path if needed)
df <- read.csv("meta-data-matrices_all.csv", sep = ";", stringsAsFactors = FALSE)

# 2. Select top 50 matrices by cell count (Size column)
df_top50 <- df %>%
  arrange(desc(Size)) 

# 3. Plot with ggplot2
p <- ggplot(df_top50, aes(x = reorder(File, Size), y = Size, fill = Group)) +
  geom_col() +
  scale_fill_manual(values = c(
    "Movies"         = "plum",
    "Archeology"     = "lightgoldenrod1",
    "Phoible"        = "paleturquoise3",
    "Orangutan"      = "tan1",
    "Chimpanzee"      = "tan2",
    "Human" = "tan3",
    "Trivia" = "tomato",
    "Plant Knowledge"= "darkolivegreen2"
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
    axis.text.y        = element_text(size = 6),
    panel.grid.minor   = element_blank(),
    legend.position    = "right"
  )
p



# # 5. Save the plot to file
# ggsave(
#   filename = "matrices_cell_count_log10.png",
#   plot = p,
#   width = 10,
#   height = 10,
#   dpi = 300,
#   bg= "white"
# )