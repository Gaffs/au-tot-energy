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

# Download the full catalogue first. Run the next two lines interactively to
# verify series IDs before filtering:
  abs_catalogue <- read_abs(cat_no = "6457.0")
  abs_catalogue |> distinct(table_title, series, series_id) |> print(n = Inf)
#
# Confirmed series IDs (verify against catalogue output above):
abs_series_ids <- c(
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

abs_trade_prices <- read_abs(cat_no = "6457.0") |>
  filter(series_id %in% abs_series_ids)

saveRDS(abs_trade_prices, "data/raw/abs_trade_prices.rds")

# ------------------------------------------------------------------------------
# FRED: Global commodity prices, monthly
# ------------------------------------------------------------------------------

fred_series <- c(
  brent_crude_usd_bbl  = "MCOILBRENTEU",  # Brent crude oil (USD/barrel)
  ttf_gas_usd_mmbtu    = "PNGASEUUSDM",   # TTF natural gas, EU (USD/MMBtu)
  aud_usd              = "AEXUSAL"         # AUD/USD exchange rate (USD per AUD), monthly average
)

fred_commodity_prices <- map(fred_series, fredr) |>
  list_rbind(names_to = "series_label") |>
  select(series_label, date, value)

saveRDS(fred_commodity_prices, "data/raw/fred_commodity_prices.rds")
