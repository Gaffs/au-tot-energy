# 01_ingest.R
# Download and save raw data from ABS and FRED

library(tidyverse)
library(readabs)
library(fredr)

# FRED API key — set FRED_API_KEY in .Renviron (usethis::edit_r_environ())
fredr_set_key(Sys.getenv("FRED_API_KEY"))

# ------------------------------------------------------------------------------
# ABS: International Trade Price Indexes (Cat. 6457.0), quarterly
# ------------------------------------------------------------------------------

# Construct dataframe with Import and Export Price Indexes for SITC to obtain series IDs:
abs_6457 <- read_abs(cat_no = "6457.0")
abs_6457_ids <- abs_6457 |> 
  distinct(table_title, series, series_id) |> 
  filter(str_detect(table_title, "SITC")) |> 
  filter(str_detect(series, "Index"))
print(abs_6457_ids, n = Inf)

# Filter abs_6457_ids for all SITC one-digit components
abs_6457_sitc1_ids <- abs_6457_ids |> 
  # If table_title contains "Import", then filter for series string beginning "Index Numbers ;  " followed by a single digit
  # If table_title contains "Export", then filter for series string containing a single digit between parentheses
  filter((str_detect(table_title, "Import") & str_detect(series, "^Index Numbers ;  \\d ")) |
          (str_detect(table_title, "Export") & str_detect(series, "\\(\\d\\)")))
print(abs_6457_sitc1_ids, n = Inf)

sitc1_trade_prices <- abs_6457 |> 
  filter(series_id %in% abs_6457_sitc1_ids$series_id)

saveRDS(sitc1_trade_prices, "data/raw/sitc1_trade_prices.rds")

# Confirmed series IDs:
abs_price_series_ids <- c(
  # Import price indexes
  "A2295765J",  # Import Price Index: All groups
  "A2295777T",  # Import Price Index: Mineral fuels, lubricants and related materials (SITC 3)
  "A2295834A",  # Import Price Index: Petroleum, petroleum products and related materials (SITC 33)
  "A2295855L",  # Import Price Index: Fertilisers, excluding crude (SITC 56)
  # Export price indexes
  "A2294886K",  # Export Price Index: All groups
  "A2295528C",  # Export Price Index: Mineral fuels, lubricants and related materials (SITC 3)
  "A2295606X",  # Coal, coke and briquettes (SITC 32)
  "A2295600K",  # Petroleum, petroleum products and related materials (SITC 33)
  "A2295603T"  # Export Price Index: Gas, natural and manufactured (SITC 34)
)

abs_trade_prices <- abs_6457 |>
  filter(series_id %in% abs_price_series_ids)

saveRDS(abs_trade_prices, "data/raw/abs_trade_prices.rds")

# ------------------------------------------------------------------------------
# ABS: International Trade in Goods and Services (Cat. 5368.0), annual
# Used to construct time-varying value-share weights for the ToT decomposition
# ------------------------------------------------------------------------------

# Construct dataframe with Import and Export Price Indexes for SITC to obtain series IDs:
abs_5368 <- read_abs(cat_no = "5368.0")
abs_5368 |> distinct(table_title) |> print(n = Inf)
abs_5368 |> filter(table_title == "TABLE 2. GOODS, Summary: Original, Current prices") |> distinct(series, series_id) |> print(n = Inf)
sitc_tables <- c(
  "TABLE 12a. MERCHANDISE EXPORTS, Standard International Trade Classification (1 and 2 digit), FOB Value",
  "TABLE 13a. MERCHANDISE IMPORTS, Standard International Trade Classification (1 and 2 digit), Customs Value"
)

abs_5368_sitc_ids <- abs_5368 |>
  filter(table_title %in% sitc_tables) |>
  distinct(table_title, series, series_id)
  print(abs_5368_sitc_ids, n = Inf)

# Filter abs_6457_ids for all SITC one-digit components
abs_5368_sitc1_ids <- abs_5368_sitc_ids |> 
  filter(str_detect(series, "^\\d "))
print(abs_5368_sitc1_ids, n = Inf)

saveRDS(abs_5368_sitc1_ids, "data/raw/sitc1_trade_values.rds")

abs_5368_sitc1_values <- abs_5368 |>
  filter(series_id %in% abs_5368_sitc1_ids$series_id)

saveRDS(abs_5368_sitc1_values, "data/raw/sitc1_trade_values_data.rds")

abs_trade_value_ids <- c(
  # Exports
  "A1827881R",  # Exports: All goods
  "A1827828C",  # Exports: Mineral fuels, lubricants and related materials (SITC 3)
  "A1827829F",  # Exports: Coal, coke and briquettes (SITC 32)
  "A1827830R",  # Exports: Petroleum, petroleum products and related materials (SITC 33)
  "A1827831T",  # Exports: Gas, natural and manufactured / LNG (SITC 34)
  # Imports
  "A1828721W",  # Imports: All goods
  "A1828668W",  # Imports: Mineral fuels, lubricants and related materials (SITC 3)
  "A1828670J",  # Imports: Petroleum, petroleum products and related materials (SITC 33)
  "A1828682T"  # Imports: Fertilisers, excluding crude (SITC 56)
)

abs_trade_values <- read_abs(cat_no = "5368.0") |>
  filter(series_id %in% abs_trade_value_ids)

saveRDS(abs_trade_values, "data/raw/abs_trade_values.rds")

# ------------------------------------------------------------------------------
# FRED: Global commodity prices, monthly
# ------------------------------------------------------------------------------

fred_series <- c(
  brent_crude_usd_bbl  = "MCOILBRENTEU",  # Brent crude oil (USD/barrel)
  ttf_gas_usd_mmbtu    = "PNGASEUUSDM",   # TTF natural gas, EU (USD/MMBtu)
  aud_usd              = "EXUSAL"         # AUD/USD exchange rate (USD per AUD), monthly average
)

fred_commodity_prices <- map(fred_series, fredr) |>
  list_rbind(names_to = "series_label") |>
  select(series_label, date, value)

saveRDS(fred_commodity_prices, "data/raw/fred_commodity_prices.rds")
