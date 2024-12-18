library(noradstats)
library(DBI)
library(duckplyr)
library(tidyverse)
library(scales)
library(noradplot)
ggnorad()

# DAC donor codes
vec_dac_donors <- read_csv2("data/raw/dac_donor_codes.csv") |> 
  filter(dac_donor_name != "EU Institutions") |> 
  select(dac_donor_code) |> 
  pull()

# Get international ODA data from CRS database
df_crs_remote <- access_international_crs()

df_crs_orig <- df_crs_remote |>
  filter(
    category == 10,
    donor_code %in% vec_dac_donors,
    year %in% c(2010:2022)
  )|>
  collect()

# df_crs_orig |> 
#   filter(year %in% c(2002:2014)) |> 
#   group_by(year) |> 
#   summarise(usd = sum(usd_disbursement_defl, na.rm = TRUE)) |> 
#   ungroup()

# df_crs_orig |> 
#   filter(year %in% c(2002:2014)) |> 
#   group_by(year) |> 
#   summarise(usd = sum(usd_disbursement_defl, na.rm = TRUE)) |> 
#   ungroup() |> 
#   ggplot(aes(x = year, y = usd)) +
#   geom_line() +
#   scale_y_continuous(limits = c(0, NA))
  

# GPGs
df_sectors_gpg <- read_csv("data/processed/sector_codes_gpg_mapping.csv")

df_sectors_gpg <- df_sectors_gpg |> 
  filter(gpg_promotion == TRUE | gpg_consequences == TRUE) |> 
  select(crs_code, gpg_promotion, gpg_consequences, gpg_theme)

# Include GPG variables in crs data
df_crs_merge <- df_crs_orig |> 
  left_join(df_sectors_gpg, join_by(purpose_code == crs_code))

# Include additional GPG activities from Rio markers, only not already included by sectors
df_crs <- df_crs_merge |>
  mutate(
    gpg_theme =
      case_when(
        is.na(gpg_theme) & (biodiversity == 2 & purpose_code == 41010) ~ "Biodiversity",
        is.na(gpg_theme) & climate_adaptation == 2 ~ "Climate adaptation",
        is.na(gpg_theme) & climate_mitigation == 2 ~ "Climate mitigation",
        .default = gpg_theme
      )
  )

# Include the additional GPG activities from Rio markers by promotion and consequences.
df_crs <- df_crs |> 
  mutate(
    gpg_promotion = case_when(
      is.na(gpg_promotion) & gpg_theme == "Biodiversity" ~ TRUE,
      is.na(gpg_promotion) & gpg_theme == "Climate mitigation" ~ TRUE,
      gpg_promotion == FALSE & gpg_theme == "Biodiversity" ~ TRUE,
      gpg_promotion == FALSE & gpg_theme == "Climate mitigation" ~ TRUE,
      .default = gpg_promotion
    ),
    gpg_consequences = case_when(
      is.na(gpg_consequences) & gpg_theme == "Climate adaptation" ~ TRUE,
      gpg_consequences == FALSE & gpg_theme == "Climate adaptation" ~ TRUE,
      .default = gpg_consequences
    )
  )

# Create gpg variable
df_crs <- df_crs |> 
  mutate(gpg = gpg_promotion == TRUE | gpg_consequences == TRUE)

###

# Checks:
# df_crs should be higher than df_crs_merge, as the Riomarked projects are included in df_crs
df_crs |> 
  filter(gpg_promotion == TRUE) |> 
  filter(gpg_theme == "Biodiversity") |> 
  summarise(sum(usd_disbursement_defl, na.rm = TRUE))

df_crs_merge |> 
  filter(gpg_promotion == TRUE) |> 
  filter(gpg_theme == "Biodiversity") |> 
  summarise(sum(usd_disbursement_defl, na.rm = TRUE))

###

# Total bilateral ODA and GPGs
df_gpg_summary <- df_crs |>
  group_by(year) |>
  summarise(
    usd = sum(usd_disbursement_defl, na.rm = TRUE),
    gpg_usd = sum(usd_disbursement_defl[gpg == TRUE], na.rm = TRUE),
    gpg_promotion_usd = sum(usd_disbursement_defl[gpg_promotion == TRUE], na.rm = TRUE),
    gpg_consequences_usd = sum(usd_disbursement_defl[gpg_consequences == TRUE], na.rm = TRUE)
  ) |>
  ungroup() |> 
  mutate(
    share_gpg = gpg_usd / usd,
    share_gpg_promotion = gpg_promotion_usd / usd,
    share_gpg_consequences = gpg_consequences_usd / usd
  )

df_gpg_amounts <- df_gpg_summary |> 
  select(year, usd, gpg_usd, gpg_promotion_usd, gpg_consequences_usd) |> 
  pivot_longer(
    cols = c(usd, gpg_usd, gpg_promotion_usd, gpg_consequences_usd),
    names_to = "category",
    values_to = "value"
  ) |> 
    mutate(category = case_when(
      category == "usd" ~ "Total bilateral ODA",
      category == "gpg_usd" ~ "Total GPGs",
      category == "gpg_promotion_usd" ~ "Contributing to GPGs",
      category == "gpg_consequences_usd" ~ "Adressing global challenges",
      .default = category
    ))

p1a <- df_gpg_amounts |>
  ggplot(aes(x = year, y = value, color = category)) +
  geom_line() +
  scale_y_continuous(limits = c(0, NA), labels = label_number(big.mark = ",")) +
  labs(
    title = "Total bilateral ODA and GPG support",
    subtitle = "Bilateral ODA from OECD DAC-countries. US dollar, Millions, 2022. Gross disbursements",
    color = NULL,
    x = NULL,
    y = NULL,
  ) +
  theme(
    legend.position = "bottom"
  ) +
  guides(color = guide_legend(reverse = TRUE, nrow = 2))


ggsave("df_gpg_summary_amounts.svg", p1a, width = 8, height = 5)

df_gpg_shares <- df_gpg_summary |> 
  select(year, share_gpg, share_gpg_promotion, share_gpg_consequences) |> 
  pivot_longer(
    cols = c(share_gpg, share_gpg_promotion, share_gpg_consequences),
    names_to = "category",
    values_to = "value"
  ) |> 
  mutate(category = case_when(
    category == "share_gpg" ~ "GPGs total",
    category == "share_gpg_promotion" ~ "Provision of GPGs ",
    category == "share_gpg_consequences" ~ "Tackling consequences of global challenges",
    .default = category
  ))

# Figures percentages: Fig 1. ODA for Global Public Goods: Provision of GPGs\nand Tackling Concequences of Global Challenges. GPG share of gross bilateral ODA disbursements from OECD DAC
p1b <- df_gpg_shares |>
  filter(category != "GPGs total") |> 
  mutate(category = fct_rev(category)) |> 
  ggplot(aes(x = year, y = value, fill = category)) +
  # geom_line(linewidth = 1.3) +
  geom_area() +
  scale_y_continuous(labels = label_percent(accuracy = 1), limits = c(0, 0.5), expand = c(0,0)) +
  scale_x_continuous(labels = as.integer, expand = c(0, 0)) +
  scale_fill_manual(values = c("#9acce8", "#03542d")) +
  labs(
title = NULL,
subtitle = NULL,
    color = NULL,
    fill = NULL,
    x = NULL,
    y = NULL,
  ) +
  theme(
    legend.position = "bottom",
    plot.margin = margin(t = 3, r = 10, b = 3, l = 3, unit = "mm"),
    legend.text = element_text(size = 14, family = "Arial")
  ) +
    guides(fill = guide_legend(nrow = 1, reverse = TRUE))


ggsave("df_gpg_summary_shares.svg", p1b, width = 6.5, height = 4)

# Test Total ODA
df_gpg_summary |>
  select(year, usd) |>
  ggplot(aes(x = year, y = usd)) +
  geom_line() +
  scale_y_continuous(limits = c(0, NA))

# If policy marker criteria is met and gpg is false gpg theme should be x.
# Then the mitigation and biodiversity is not complete but only activities not in other sectors.
# GPG provision
p2a <- df_crs |>
  filter(gpg_promotion == TRUE) |>
  group_by(year, gpg_theme) |>
  summarise(usd = sum(usd_disbursement_defl, na.rm = TRUE)) |>
  ungroup() |>
  mutate(gpg_theme = fct_reorder(gpg_theme, usd)) |>
  ggplot(aes(year, usd, fill = gpg_theme)) +
  geom_area() +
  scale_fill_norad() +
  scale_y_continuous(labels = label_number(big.mark = ","), expand = c(0, 0)) +
  scale_x_continuous(labels = as.integer, expand = c(0, 0)) +  # Right expansion
  labs(
    title = "Fig 2. ODA for provision of GPGs",
    subtitle = "OECD DAC-members. US dollar, Millions, 2022. Gross disbursements",
    fill = NULL,
    x = NULL,
    y = NULL
  ) +
  theme(legend.position = "bottom", plot.margin = margin(5, 30, 5, 5)) +
  guides(fill = guide_legend(nrow = 3, reverse = TRUE))


ggsave("df_gpg_support.svg", p2a, width = 8, height = 5)

# Adressing global challenges
# GPG provision
p2b <- df_crs |>
  filter(gpg_consequences == TRUE) |>
  group_by(year, gpg_theme) |>
  summarise(usd = sum(usd_disbursement_defl, na.rm = TRUE)) |>
  ungroup() |>
  mutate(gpg_theme = fct_reorder(gpg_theme, usd)) |>
  ggplot(
    aes(year, usd, fill = gpg_theme)
  ) +
  geom_area() +
  scale_fill_norad() +
    scale_y_continuous(labels = label_number(big.mark = ","), expand = c(0, 0)) +
    scale_x_continuous(labels = as.integer, expand = c(0, 0)) +  # Right expansion
  labs(
    title = "Fig 3. ODA for tackling Consequences of Global Challenges",
    subtitle = "OECD DAC-members. US dollar, Millions, 2022. Gross disbursements",
    fill = NULL,
    x = NULL,
    y = NULL
  ) +
  theme(legend.position = "bottom", plot.margin = margin(5, 30, 5, 5)) +
  guides(fill = guide_legend(nrow = 2, reverse = TRUE))


ggsave("df_gpg_challenges.svg", p2b, width = 9, height = 5)
