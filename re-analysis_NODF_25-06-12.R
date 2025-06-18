# ======== Re analysis of nestedness in empirical matrices ========

## ==== 1. Library import ====
library(tidyverse)
library(progress)
library(vegan)
library(permute)
library(lattice)

## Parameters 

# set parallel options to the computer's number of cores minus 1
options(mc.cores = max(1, parallel::detectCores() - 1))
# Number of simulations
N_ITER_ = 10

## ==== 2. Functions ====

### ---- A. Compute correlation ----
compute_cor_coef <- function(matrix) {
  avg_inventory <- apply(matrix, 2, function(item_col) {
    mean(rowSums(matrix)[item_col == 1])})
  
  item_stats <- data.frame(
    Prevalence = colSums(matrix),
    AvgInventory = avg_inventory
  )
  return(cor(item_stats$Prevalence, item_stats$AvgInventory))
}

## ==== 3. Nestedness analysis ====
nestedness_analysis <- function(matrix, matrix_id, N_ITER_) {
  
  ### ---- A. Parameters list ----
  baselines <- c('r00', 'r0', 'r1', 'r2','c0','curveball', 'swap')
  # add c1, inverse matrix then r1
  
  ### ---- B. Initialize empty dataframe ----
  df_cols <- list(
    matrix_id = character(),
    num_rows = integer(),
    num_columns = integer(),
    coef_cor = numeric(),
    metric = character(),
    baseline = character(),
    type = character(),
    nestedness_value = numeric(),
    p_value = numeric()
  )
  df_nestedness <- do.call(data.frame, c(df_cols, stringsAsFactors = FALSE))
  
  ### ---- C. Coefficient of correlation ----
  cor_coef <- compute_cor_coef(matrix)
  
  ### ---- D. Append dataframe ----
  for (b in baselines) {
    # Compute nestedness
    res <- oecosimu(
      comm  = matrix,
      nestfun = nestednodf,
      method = b,
      alternative = "two.sided",
      nsimul = N_ITER_,
      batchsize = 50,
      parallel= TRUE
    )
    
    # indice de la ligne globale NODF
    row_idx    <- 3
    # valeur réelle
    stat_val   <- res$statistic[row_idx]
    # simulations
    sim_vals    <- res$oecosimu$simulated[row_idx, ] 
    # p-value globale (identique pour toutes les simulations)
    pval_global <- res$oecosimu$pval[row_idx]
    
    
    ### ---- E1. Simulated rows ----
    new_sim <- data.frame(
      matrix_id = rep(matrix_id, N_ITER_),
      num_rows = rep(nrow(matrix), N_ITER_),
      num_columns = rep(ncol(matrix), N_ITER_),
      coef_cor = rep(cor_coef, N_ITER_),
      metric  = rep("NODF", N_ITER_),
      baseline  = rep(b, N_ITER_),
      type  = rep("simulated", N_ITER_),
      nestedness_value = as.numeric(sim_vals),
      p_value = rep(pval_global, N_ITER_),
      stringsAsFactors = FALSE
    )
    
    ### ---- E2. Real rows ----
    new_real <- data.frame(
      matrix_id  = matrix_id,
      num_rows = nrow(matrix),
      num_columns = ncol(matrix),
      coef_cor = cor_coef,
      baseline = b,
      metric = "NODF",
      type   = "real",
      nestedness_value = res$statistic$statistic[3L],
      p_value = pval_global,
      stringsAsFactors = FALSE
    )
    print(res$oecosimu$simulated)
    print(res$statistic)
    
    ### ---- F. Final Dataframe  ----
    df_nestedness <- rbind(df_nestedness, new_sim, new_real)
  }
  ### ---- G. Save the dataset ----
  
  write.csv(df_nestedness, 
            paste0("analysis_nestedness_", matrix_id, ".csv"), 
            row.names = FALSE)
  return(df_nestedness)
  
  
}


## Example 

mat_example <- matrix(c(
  1, 1, 1, 1,
  1, 1, 0, 0,
  1, 0, 1, 0,
  1, 0, 0, 1,
  0, 1, 0, 1
), nrow = 5, byrow = TRUE)

nestedness_analysis(mat_example, "test1", N_ITER_)