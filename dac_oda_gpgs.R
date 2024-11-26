library(noradstats)
library(DBI)
library(duckplyr)
library(tidyverse)

# DAC donor codes
vec_dac_donors <- read_csv2("data/raw/dac_donor_codes.csv") |> 
  select(dac_donor_code) |> 
  pull()

# Get international ODA data from CRS database
df_crs_remote <- access_international_crs()

df_crs <- df_crs_remote |>
  filter(
    category == 10,
    donor_code %in% vec_dac_donors,
    year == max(year)
  ) |>
  collect()

# GPGs
df_sectors_gpg <- read_csv("data/processed/sector_codes_gpg_mapping.csv")

df_sectors_gpg <- df_sectors_gpg |> 
  mutate(gpg = gpg_promotion == TRUE | gpg_concequences == TRUE) |> 
  filter(gpg == TRUE) |> 
  select(crs_code, gpg, gpg_promotion, gpg_concequences)

# Include GPG variables in crs data
df_crs <- df_crs |> 
  left_join(df_sectors_gpg, join_by(purpose_code == crs_code))
