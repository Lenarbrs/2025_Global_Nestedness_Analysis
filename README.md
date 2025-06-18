# Global_Nestedness_Analysis

## Original Datasets:

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
- 1 csv per matrix
- 7,007 rows (7 baselines * 1,000 simulations + 1 real rows * 7 baselines)
- 9 columns
- c1 in another csv

Structure:

| Matrix_ID | n_rows | n_columns | coefcor | Baseline | Type (simulated / Real) | stat_NODF_general | stat_NODF_col | stat_NODF_row | 
|-----------|--------|-----------|---------|----------|-------------------------|-------------------|---------------|----------|----------|

## Google doc 

Google doc with articles overview, and informations about the matrices and baselines : https://docs.google.com/document/d/1Mr7YmTxljK23lHVOm70Fb527q6YYfOtZWjAf1tklEms/edit?usp=sharing
