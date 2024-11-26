library(readxl)
library(readr)

df <- read_xlsx("data/raw/sector_codes_gpg_mapping.xlsx")

write_csv(df, "data/processed/sector_codes_gpg_mapping.csv")
