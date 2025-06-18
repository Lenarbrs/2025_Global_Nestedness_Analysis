# Global_Nestedness_Analysis

## Original Datasets: (to update)

- Archeology pots: 5 matrices
- Kamilar, Atkinson - chimpanzees orangutans: 10 matrices
- Koshevoy, Rebours - phoible: 71 matrices (70 families + 1 global matrix)
- Dubourg, Morin - trivia: 3 matrices (3 themes)
- Salali et al. - BaYaka plant knowledge: 1 matrix
- Morin, Shobchuk - the shorlist effect (not in github): 30 in total, MovieLens -> 16 matrices (15 genre matrices + 1 global matrix), Netflix -> 14 matrices (genre)

In total: 121 matrices

## Global Dataset Structure:

Metric: NODF + Temp

Baselines: r00, r0, r1, r2, c0, c1, curveball, swap

Output csv: 
- 2 csv per matrix (A and B), plus 8 oecosimu csv (C)
- A : 8,008 rows (8 baselines * 1,000 simulations + 1 real rows * 8 baselines) and 9 columns
- B : 1 row and 12 columns
- C : oecosimu tables, one csv for each baseline

Structure dataset A (rawdata) :

| Matrix_ID | Baseline | Type (simulated / Real) | stat_NODF_general | stat_NODF_col | stat_NODF_row | stat_Temp |
|-----------|----------|-------------------------|-------------------|---------------|---------------|-----------|

Structure dataset B (general) :

| Matrix_ID | n_row | n_col | fill | size | coef_cor | p_value_NODF_[baseline]*8 | p_value_Temp_[baseline]*8 | stat_NODF_general | stat_NODF_col | stat_NODF_row | stat_Temp |
|-----------|-------|-------|------|------|----------|---------------------------|---------------------------|-------------------|---------------|--------------|-----------|


## Google doc 

Google doc with articles overview, and informations about the matrices and baselines : https://docs.google.com/document/d/1Mr7YmTxljK23lHVOm70Fb527q6YYfOtZWjAf1tklEms/edit?usp=sharing
