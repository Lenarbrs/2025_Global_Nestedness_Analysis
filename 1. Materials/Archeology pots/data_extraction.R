# ======== Archeology pots dataset ========

## 1. Loading data ====
library(folio)

## 2. Save datasets to csv ====
df_boves <- boves
write.csv(df_boves, "boves.csv", row.names = FALSE)

df_compiegne <- compiegne
write.csv(df_compiegne, "compiegne.csv", row.names = FALSE)

df_chevelon <- chevelon
write.csv(df_chevelon, "chevelon.csv", row.names = FALSE)

df_merzbach <- merzbach
write.csv(df_merzbach, "merzback.csv", row.names = FALSE)

df_mississippi <- mississippi
write.csv(df_mississippi, "mississippi.csv", row.names = FALSE)

df_zuni <- zuni
write.csv(df_zuni, "zuni.csv", row.names = FALSE)

df_loire <- loire
write.csv(df_loire, "loire.csv", row.names = FALSE)
