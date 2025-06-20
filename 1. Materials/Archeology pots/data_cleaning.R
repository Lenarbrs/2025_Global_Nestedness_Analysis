# ======== Archeology pots dataset - data cleaning ========

## 1. library import ====
library(dplyr)
library(readr)


## 2. data import ====
df_loire <- read_csv("loire.csv")
df_chevelon <- read_csv("chevelon.csv")
df_merzback <- read_csv("merzback.csv")


## 3. LOIRE data cleaning ====
# Identify ceramic columns (all columns from index 5 onwards)
ceramic_columns <- names(df)[6:ncol(df)]

# Create loire_site: group by site and check for any ceramic presence
loire_site <- df_loire %>%
  group_by(site) %>%
  summarise(across(all_of(ceramic_columns), ~ as.integer(any(. > 0)))) %>%
  ungroup()
write.csv(loire_site, "loire_site.csv", row.names = FALSE)

# Create loire_city: group by city and check for any ceramic presence
loire_city <- df_loire %>%
  group_by(city) %>%
  summarise(across(all_of(ceramic_columns), ~ as.integer(any(. > 0)))) %>%
  ungroup()
write.csv(loire_city, "loire_city.csv", row.names = FALSE)

# Create loire_area: group by area and check for any ceramic presence
loire_area <- df_loire %>%
  group_by(area) %>%
  summarise(across(all_of(ceramic_columns), ~ as.integer(any(. > 0)))) %>%
  ungroup()
write.csv(loire_area, "loire_area.csv", row.names = FALSE)


## 4. CHEVELON data cleaning ====
chevelon_binarized <- ifelse(df_chevelon != 0, 1, 0)
write.csv(chevelon_binarized, "chevelon_bin.csv", row.names = FALSE)


## 5. MERZBACK data cleaning ====
merzback_binarized <- ifelse(df_merzback != 0, 1, 0)
write.csv(merzback_binarized, "merzback_bin.csv", row.names = FALSE)

