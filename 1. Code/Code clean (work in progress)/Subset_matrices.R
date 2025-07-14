#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Script to create subset of matrices
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# This script filters binary matrices by keeping only the top 10% of rows
# (with the most 1s), and from those, the top 70% of columns (also based on 1s).
# This script was used for the Movie Lens and the Netflix Matrices.
# It processes all CSV files in a given input folder and writes the filtered
# versions to an output folder.
# You need to change the directories for this script to work

## ==== 1. Set Up Directories ====
input_dir  <- "movies"    # Folder containing input .csv matrices
output_dir <- "subset of movies"     # Folder to save filtered matrices
# dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)  

## ==== 2. Define Filtering Function ====
filter_binary_matrix <- function(mat, row_frac = 0.10, col_frac = 0.70) {
  # Check input validity
  if (!is.matrix(mat) || !all(mat %in% c(0, 1))) {
    stop("`mat` must be a binary matrix containing only 0s and 1s.")
  }
  
  # ==== A. Filter Rows ====
  n_rows <- nrow(mat)
  k_rows <- max(1, ceiling(n_rows * row_frac)) # keep at least 1 row
  row_sums <- rowSums(mat)      # count 1s per row
  top_row_idx <- order(row_sums, decreasing = TRUE)[1:k_rows]
  sub_mat <- mat[top_row_idx, , drop = FALSE]  # subset to top rows
  
  # ==== B. Filter Columns ====
  n_cols <- ncol(sub_mat)
  k_cols <- max(1, ceiling(n_cols * col_frac)) # keep at least 1 col
  col_sums <- colSums(sub_mat) # count 1s per column
  top_col_idx <- order(col_sums, decreasing = TRUE)[1:k_cols]
  
  # ==== C. Return Filtered Matrix ====
  filtered_mat <- sub_mat[, top_col_idx, drop = FALSE]   # subset to top columns
  return(filtered_mat)
}

## ==== 3. Loop Over CSV Files ====
files <- list.files(input_dir, pattern = "\\.csv$", full.names = TRUE)

for (f in files) {
  df  <- read.csv(f, header = TRUE, check.names = FALSE)   # read matrix
  mat <- as.matrix(df)
  # Filter matrix using defined function
  mat_filt <- filter_binary_matrix(mat, row_frac = 0.10, col_frac = 0.70)
  # Build output path and save CSV
  base <- tools::file_path_sans_ext(basename(f))
  out  <- file.path(output_dir, paste0(base, "_filtered.csv"))
  write.csv(mat_filt, out, row.names = FALSE)
  # Progress message
  message("Processed: ", basename(f), " → ", basename(out))
}
