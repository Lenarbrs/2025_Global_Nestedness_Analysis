# ======== Re analysis of nestedness in empirical matrices ========

## ==== 1. Library import ====

library(tidyverse)
library(progress)
library(vegan)
library(permute)
library(lattice)

options(mc.cores = max(1, parallel::detectCores() - 1))


## ==== 2. Functions ====

### A. Compute correlation ----
compute_cor_coef <- function(mat) {
  avg_inventory <- apply(mat, 2, function(item_col) {
    mean(rowSums(mat)[item_col == 1])})
  
  item_stats <- data.frame(
    Prevalence = colSums(mat),
    AvgInventory = avg_inventory
  )
  return(cor(item_stats$Prevalence, item_stats$AvgInventory))
}

### B. Compute nodf p-value ----
compute_p_val_nodf <- function(mat, b) {
  tryCatch({
    out_nodf <- oecosimu(comm = mat, nestfun = nestednodf, 
                           method = b, alt = "greater", nsimul = 1000,
                           batchsize = 50, parallel = TRUE)
    out_nodf$oecosimu$pval[3]
  }, error = function(e) NA_real_)
}

### ---- C. Compute temp p-value ----
compute_p_val_temp <- function(mat, b) {
  tryCatch({
    out_temp <- oecosimu(comm = mat, nestfun = nestedtemp, 
                           method = b, alt = "less", nsimul = 1000,
                           batchsize = 50, parallel = TRUE)
    out_temp$oecosimu$pval
  }, error = function(e) NA_real_)
}


## ==== 3. Data import ====



## ==== 4. Nestedness analysis ====

### ---- A. Parameters lists ----
metrics <- c('NODF', 'Temp') # Pour l'instant que NODF
baselines <- c('r00', 'r0', 'r1', 'r2','c0','curveball','swap','backtracking')

# Rajouter c1 en inversant la matrice + faire r1
