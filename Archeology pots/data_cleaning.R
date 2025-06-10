# ======== Archeology pots dataset - data cleaning ========

## 1. library import ====
library(dplyr)
library(readr)

## 2. data import ====
df <- read_csv("loire.csv")

## 3. data cleaning ====
# Identify ceramic columns (all columns from index 5 onwards)
ceramic_columns <- names(df)[6:ncol(df)]

# Create loire_site: group by site and check for any ceramic presence
loire_site <- df %>%
  group_by(site) %>%
  summarise(across(all_of(ceramic_columns), ~ as.integer(any(. > 0)))) %>%
  ungroup()
write.csv(loire_site, "loire_site.csv", row.names = FALSE)

# Create loire_city: group by city and check for any ceramic presence
loire_city <- df %>%
  group_by(city) %>%
  summarise(across(all_of(ceramic_columns), ~ as.integer(any(. > 0)))) %>%
  ungroup()
write.csv(loire_city, "loire_city.csv", row.names = FALSE)

# Create loire_area: group by area and check for any ceramic presence
loire_area <- df %>%
  group_by(area) %>%
  summarise(across(all_of(ceramic_columns), ~ as.integer(any(. > 0)))) %>%
  ungroup()
write.csv(loire_area, "loire_area.csv", row.names = FALSE)
