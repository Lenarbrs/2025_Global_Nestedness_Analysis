#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Set up your directories
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

input_dir  <- "data/input" # folder containing the original csv files 
output_dir <- "data/output" # folder where the sorted csvs will be saved 
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Function ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Function to filter a binary matrix by top rows and columns based on density of 1s
filter_binary_matrix <- function(mat, row_frac = 0.10, col_frac = 0.70) {
  # Check that input is a binary matrix
  if (!is.matrix(mat) || !all(mat %in% c(0, 1))) {
    stop("`mat` must be a binary matrix containing only 0s and 1s.")
  }
  
  # ==== 1) Select top fraction of rows ====
  
  # Number of rows in the matrix
  n_rows <- nrow(mat)
  # Compute how many rows to keep (at least 1)
  k_rows <- max(1, ceiling(n_rows * row_frac))
  
  # Compute the sum of 1s for each row
  row_sums <- rowSums(mat)
  # Order rows by descending sum of 1s and take the top k_rows indices
  top_row_idx <- order(row_sums, decreasing = TRUE)[seq_len(k_rows)]
  
  # Subset the matrix to keep only the top rows
  sub_mat <- mat[top_row_idx, , drop = FALSE]
  

  # ==== 2) Select top fraction of columns within selected rows ====

  # Number of columns in the subset matrix
  n_cols <- ncol(sub_mat)
  # Compute how many columns to keep (at least 1)
  k_cols <- max(1, ceiling(n_cols * col_frac))
  
  # Compute the sum of 1s for each column in the subset
  col_sums <- colSums(sub_mat)
  # Order columns by descending sum of 1s and take the top k_cols indices
  top_col_idx <- order(col_sums, decreasing = TRUE)[seq_len(k_cols)]
  

  # ==== 3) Return the filtered matrix ====
  
  # Subset the rows and columns to produce the final filtered matrix
  filtered_mat <- sub_mat[, top_col_idx, drop = FALSE]
  return(filtered_mat)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Loop over all CSV files
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

files <- list.files(input_dir, pattern = "\\.csv$", full.names = TRUE)

for (f in files) {
  # Read CSV and convert to matrix
  df  <- read.csv(f, header = TRUE, check.names = FALSE)
  mat <- as.matrix(df)
  
  # Apply filtering (top 10% rows, then top 70% columns)
  mat_filt <- filter_binary_matrix(mat, row_frac = 0.10, col_frac = 0.70)
  
  # Build output filename
  base <- tools::file_path_sans_ext(basename(f))
  out  <- file.path(output_dir, paste0(base, "_filtered.csv"))
  
  # Write filtered matrix to CSV
  write.csv(mat_filt, out, row.names = FALSE)
  
  message("Processed: ", basename(f), " → ", basename(out))
}