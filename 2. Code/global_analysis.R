#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Re analysis of nestedness in empirical matrices ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
N_ITER_ <- 10

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
  baselines <- c('r00', 'r0', 'r1', 'r2','c0','c1','curveball', 'swap')
  metrics   <- c("NODF", "Temp")
  
  ### ---- B. Initialize empty dataframe ----
  for (met in metrics) {
  df_metric <- data.frame(
    matrix_id = character(),
    num_rows = integer(),
    num_columns = integer(),
    cor_coef = numeric(),
    metric = character(),
    baseline = character(),
    type = character(),
    columns_value  = numeric(),
    rows_value     = numeric(),
    global_value   = numeric(),
    p_value        = numeric()
  )
  
  ### ---- C. Coefficient of correlation ----
  cor_coef <- compute_cor_coef(matrix)
  
  ### ---- D. Append dataframe ----
    for (b in baselines) {
    ### ---- D1. Prepare matrix & method ----
    current_matrix <- matrix
    baseline_used <- b
    # Create the c1 baseline
    if (b == 'c1') { 
      current_matrix <- 1 - matrix
      baseline_used <- 'r1' 
    } 
    # Choose the metric
    if (met == "NODF") {
      nestfun  <- nestednodf
      stat_idx <- 3    # global NODF
    } else {
      nestfun  <- nestedtemp
      stat_idx <- 1    # Temperature
    }
    
    ### ---- D2. Run oecosimu ----
    res <- oecosimu(
      comm  = current_matrix,
      nestfun = nestfun,
      method = baseline_used,
      alternative = "two.sided",
      nsimul = N_ITER_,
      batchsize = 50,
      parallel = TRUE
    )
    
    ## Statistics
    # Nestedness value of the real matrix
    stat_val <- res$statistic$statistic   
    # p-value
    pval <- res$oecosimu$pval[stat_idx]
    
    ### ---- E1. Simulated rows ----
    
    # For NODF
    if (met == "NODF") {
    sim_mat <- as.data.frame(t(res$oecosimu$simulated))
    colnames(sim_mat) <- c("columns_value",
                          "rows_value",
                          "global_value")
    } else {
      sim_mat <- data.frame(
        columns_value = NA,
        rows_value    = NA,
        global_value  = as.numeric(res$oecosimu$simulated)
      )
    }
    
    new_sim <- data.frame(
      matrix_id = rep(matrix_id, N_ITER_),
      num_rows = rep(nrow(matrix), N_ITER_),
      num_columns = rep(ncol(matrix), N_ITER_),
      cor_coef = rep(cor_coef, N_ITER_),
      metric = rep(met , N_ITER_),
      baseline = rep(b, N_ITER_),
      type  = rep("simulated", N_ITER_),
      columns_value = sim_mat$columns_value,
      rows_value = sim_mat$rows_value,
      global_value = sim_mat$global_value,
      p_value = rep(pval, N_ITER_),
      stringsAsFactors = FALSE
    )
    
    ### ---- E2. Real rows ----
    
    # For NODF
    if (met == "NODF") {
    new_real <- data.frame(
      matrix_id  = matrix_id,
      num_rows = nrow(matrix),
      num_columns = ncol(matrix),
      cor_coef = cor_coef,
      metric = met,
      baseline = b,
      type   = "real",
      columns_value = stat_val[1],
      rows_value= stat_val[2],
      global_value = stat_val[3],
      p_value = pval,
      stringsAsFactors = FALSE
    )
    print(res$oecosimu$simulated)
    print(res$statistic)
    
    # For Temperature
    } else {
      new_real <- data.frame(
        matrix_id = matrix_id,
        num_rows  = nrow(matrix),
        num_columns = ncol(matrix),
        cor_coef = cor_coef,
        metric = met,
        baseline = b,
        type = "real",
        columns_value = NA,
        rows_value = NA,
        global_value  = stat_val[1],
        p_value = pval,
        stringsAsFactors = FALSE
      )
    }
    
    ### ---- F. Final Dataframe  ----
    df_metric <- bind_rows(df_metric, new_sim, new_real)
    
    ### ---- G. After every baselines for this metric: write a csv ----
    write.csv(
      df_metric,
      paste0("analysis_nestedness_", matrix_id, "_", met, ".csv"),
      row.names = FALSE)
   }
  }
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