# ======== Re analysis of nestedness in empirical matrices ========

## ==== 1. Library import ====
library(tidyverse)
library(progress)
library(vegan)
library(permute)
library(lattice)
# set parallel options to the computer's number of cores minus 1
options(mc.cores = max(1, parallel::detectCores() - 1))


## ==== 2. Functions ====

### ---- A. Compute correlation ----
compute_cor_coef <- function(mat) {
  avg_inventory <- apply(mat, 2, function(item_col) {
    mean(rowSums(mat)[item_col == 1])})
  
  item_stats <- data.frame(
    Prevalence = colSums(mat),
    AvgInventory = avg_inventory
  )
  return(cor(item_stats$Prevalence, item_stats$AvgInventory))
}

### ---- B. Compute nodf p-value ----
compute_p_val_nodf <- function(mat, b, n_iter) {
  tryCatch({
    out_nodf <- oecosimu(comm = mat, nestfun = nestednodf, 
                           method = b, alt = "greater", nsimul = n_iter,
                           batchsize = 50, parallel = TRUE)
    out_nodf
  }, error = function(e) NA_real_)
}


## ==== 3. Nestedness analysis ====
nestedness_analysis <- function(matrix, matrix_id, n_iter) {
  
  ### ---- A. Parameters list ----
  baselines <- c('r00', 'r0', 'r1', 'r2','c0','curveball', 'swap')
  # add c1, inverse matrix then r1
  
  ### ---- B. Initialize empty dataframe ----
  df_cols <- list(
    matrix_id = character(),
    num_rows = integer(),
    num_columns = integer(),
    coef_cor = numeric()
  )
  # Add baseline/metric-specific columns
  for (b in baselines) {
    df_cols[[paste0("p_value_", b)]] <- numeric()
    df_cols[[paste0("nestedness_value_", b)]] <- numeric()
  }
  df_nestedness <- do.call(data.frame, c(df_cols, stringsAsFactors = FALSE))
  
  ### ---- C. Coefficient of correlation ----
  cor_coef <- compute_cor_coef(matrix)
  
  ### ---- D. Append dataframe ----
  # Create new row
  new_row <- data.frame(
    matrix_id = matrix_id,
    num_rows = nrow(matrix),
    num_columns = ncol(matrix),
    coef_cor = cor_coef,
    stringsAsFactors = FALSE
  )
  
  for (b in baselines){
    ### ---- E. Compute nestedness ----
    p_val_nodf <- compute_p_val_nodf(matrix, b, n_iter)
    new_row[[paste0("p_value_", b)]] <- p_val_nodf$oecosimu$pval[3]
    new_row[[paste0("nestedness_value_", b)]]<- HSHSJ
  }
  
  ### ---- F. Save results ----
  df_nestedness <- rbind(df_nestedness, new_row)
  write.csv(df_nestedness, paste0("nestedness_", matrix_id, ".csv"), row.names = FALSE)
}

