# Global_Nestedness_Analysis

## Original Datasets: 

- Archeology pots: 7 matrices
- Kamilar, Atkinson - chimpanzees orangutans: 10 matrices (4 humans + 3 chimpanzees + 3 ourangutans)
- Koshevoy, Rebours - phoible: 71 matrices (70 families + 1 global matrix)
- Dubourg, Morin - trivia: 3 matrices (3 themes)
- Salali et al. - BaYaka plant knowledge: 2 matrices (same base, different treatment of NA)
- Morin, Shobchuk - the shorlist effect (not in github): 30 in total, MovieLens -> 15 matrices (genre), Netflix -> 15 matrices (genre)

In total: 123 matrices

## Global Dataset Structure:

Metric: NODF + Temp

Baselines: r00, r0, r1, r2, c0, c1, curveball, swap

Output csv: 
- 2 csv per matrix (A and B), plus 1 p-value csv (C)
- A : 8,000 rows (8 baselines * 1,000 simulations) and 7 columns
- B : 1 row and 10 columns
- C : p-values table, one csv for each matrix, 1 row and 32 columns

Structure dataset A (rawdata) :

| Matrix_ID | Baseline | stat_NODF_general | stat_NODF_col | stat_NODF_row | stat_Temp |
|-----------|----------|-------------------|---------------|---------------|-----------|

Structure dataset B (general) :

| Matrix_ID | n_row | n_col | fill | size | coef_cor | stat_NODF_general | stat_NODF_col | stat_NODF_row | stat_Temp | 
|-----------|-------|-------|------|------|----------|-------------------|---------------|---------------|-----------|


Structure dataset C (p-values) :

| p_value_NODF_[baseline]*8 | p_value_Temp_[baseline]*8 | Significance Side (Nested / Antinested)*16 |
|---------------------------|---------------------------|--------------------------------------------|

## Google doc 

Google doc with articles overview, and informations about the matrices and baselines : https://docs.google.com/document/d/1Mr7YmTxljK23lHVOm70Fb527q6YYfOtZWjAf1tklEms/edit?usp=sharing
