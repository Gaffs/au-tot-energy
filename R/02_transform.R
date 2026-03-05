# 02_transform.R
# Clean and transform raw data; compute ToT decompositions

library(tidyverse)

# ------------------------------------------------------------------------------
# Step 1: Clean ABS trade price indexes
# ------------------------------------------------------------------------------

series_lookup <- tribble(
  ~series_id,    ~label,
  "A2295765J",   "mpi_all",
  "A2295777T",   "mpi_mineral_fuels",
  "A2295834A",   "mpi_petroleum",
  "A2295855L",   "mpi_fertilisers",
  "A2294886K",   "epi_all",
  "A2295528C",   "epi_mineral_fuels",
  "A2295606X",   "epi_coal",
  "A2295600K",   "epi_petroleum",
  "A2295603T",   "epi_gas"
)

abs_prices_clean <- readRDS("data/raw/abs_trade_prices.rds") |>
  select(date, series_id, value) |>
  left_join(series_lookup, by = "series_id") |>
  select(date, label, value) |>
  pivot_wider(names_from = label, values_from = value) |>
  arrange(date) |>
  mutate(across(-date, \(x) log(x / lag(x, 4)), .names = "dlog_{.col}"))

saveRDS(abs_prices_clean, "data/processed/abs_prices_clean.rds")

# ------------------------------------------------------------------------------
# Step 2: Clean and align FRED commodity prices
# ------------------------------------------------------------------------------

fred_prices_clean <- readRDS("data/raw/fred_commodity_prices.rds") |>
  pivot_wider(names_from = series_label, values_from = value) |>
  mutate(quarter = floor_date(date, "quarter")) |>
  group_by(quarter) |>
  summarise(across(c(brent_crude_usd_bbl, ttf_gas_usd_mmbtu, aud_usd),
                   \(x) mean(x, na.rm = TRUE)),
            .groups = "drop") |>
  mutate(
    brent_crude_aud_bbl = brent_crude_usd_bbl / aud_usd,
    ttf_gas_aud_mmbtu   = ttf_gas_usd_mmbtu   / aud_usd
  ) |>
  arrange(quarter) |>
  mutate(across(-quarter, \(x) log(x / lag(x, 4)), .names = "dlog_{.col}"))

saveRDS(fred_prices_clean, "data/processed/fred_prices_clean.rds")

# ------------------------------------------------------------------------------
# Step 3a: ToT decomposition — aggregated (SITC 3)
# ------------------------------------------------------------------------------

tot_decomp_agg <- abs_prices_clean |>
  select(date, dlog_epi_all, dlog_mpi_all,
         dlog_epi_mineral_fuels, dlog_mpi_mineral_fuels) |>
  mutate(
    dlog_tot            = dlog_epi_all - dlog_mpi_all,
    export_contribution = dlog_epi_all,
    import_contribution = -dlog_mpi_all
  ) |>
  select(date, dlog_tot, export_contribution, import_contribution,
         dlog_epi_mineral_fuels, dlog_mpi_mineral_fuels)

saveRDS(tot_decomp_agg, "data/processed/tot_decomp_agg.rds")

# ------------------------------------------------------------------------------
# Step 3b: ToT decomposition — disaggregated (SITC 32/33/34/56)
# ------------------------------------------------------------------------------

tot_decomp_disagg <- abs_prices_clean |>
  select(date, dlog_epi_all, dlog_mpi_all,
         dlog_epi_coal, dlog_epi_petroleum, dlog_epi_gas,
         dlog_mpi_petroleum, dlog_mpi_fertilisers) |>
  mutate(
    dlog_tot            = dlog_epi_all - dlog_mpi_all,
    export_contribution = dlog_epi_all,
    import_contribution = -dlog_mpi_all
  ) |>
  select(date, dlog_tot, export_contribution, import_contribution,
         dlog_epi_coal, dlog_epi_petroleum, dlog_epi_gas,
         dlog_mpi_petroleum, dlog_mpi_fertilisers)

saveRDS(tot_decomp_disagg, "data/processed/tot_decomp_disagg.rds")
