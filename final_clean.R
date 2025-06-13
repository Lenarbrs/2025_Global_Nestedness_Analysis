# Required package
library(tidyverse)

# Directory containing your CSVs
csv_files <- list.files("Clean_data_all", pattern = "\\.csv$", full.names = TRUE)

# Function to clean a matrix
clean_matrix <- function(file_path) {
  df <- read.csv(file_path, header = TRUE, row.names = 1)  # Adjust as needed
  
  # Remove non-binary entries (optional check)
  df[] <- lapply(df, function(x) as.numeric(x == 1))  # Force 1/0
  
  # If row names are important, keep them; else:
  mat <- as.matrix(df)
  
  return(mat)
}

# Apply to all files and save cleaned versions
cleaned_matrices <- lapply(csv_files, clean_matrix)

# Optional: Save cleaned matrices for inspection
for (i in seq_along(cleaned_matrices)) {
  write.csv(cleaned_matrices[[i]], paste0("cleaned_", i, ".csv"), row.names = FALSE)
}
