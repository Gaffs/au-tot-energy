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
  mutate(across(-date, \(x) log(x / lag(x, 4)), .names = "dlog_{.col}")) |>
  mutate(across(c(-date, -starts_with("dlog_")),
                \(x) log(x / lag(x, 1)), .names = "qlog_{.col}"))

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
  mutate(across(-quarter, \(x) log(x / lag(x, 4)), .names = "dlog_{.col}")) |>
  mutate(across(c(-quarter, -starts_with("dlog_")),
                \(x) log(x / lag(x, 1)), .names = "qlog_{.col}"))

saveRDS(fred_prices_clean, "data/processed/fred_prices_clean.rds")

# ------------------------------------------------------------------------------
# Step 2.5: Construct annual value-share weights from trade values (Cat. 5368.0)
# ------------------------------------------------------------------------------

value_lookup <- tribble(
  ~series_id,   ~label,
  "A1827881R",  "exports_all",
  "A1827828C",  "exports_mineral_fuels",
  "A1827829F",  "exports_coal",
  "A1827830R",  "exports_petroleum",
  "A1827831T",  "exports_gas",
  "A1828721W",  "imports_all",
  "A1828668W",  "imports_mineral_fuels",
  "A1828670J",  "imports_petroleum",
  "A1828682T",  "imports_fertilisers"
)

trade_weights <- readRDS("data/raw/abs_trade_values.rds") |>
  select(date, series_id, value) |>
  left_join(value_lookup, by = "series_id") |>
  mutate(fy = year(date) + (month(date) >= 7L)) |>
  summarise(value = sum(value, na.rm = TRUE), .by = c(fy, label)) |>
  pivot_wider(names_from = label, values_from = value) |>
  mutate(
    # Aggregated weights (SITC 3)
    w_epi_mineral_fuels = exports_mineral_fuels / exports_all,
    w_mpi_mineral_fuels = imports_mineral_fuels / imports_all,
    # Disaggregated weights (SITC 32/33/34/56)
    w_epi_coal          = exports_coal          / exports_all,
    w_epi_petroleum     = exports_petroleum     / exports_all,
    w_epi_gas           = exports_gas           / exports_all,
    w_mpi_petroleum     = imports_petroleum     / imports_all,
    w_mpi_fertilisers   = imports_fertilisers   / imports_all
  ) |>
  select(fy, starts_with("w_"))

saveRDS(trade_weights, "data/processed/trade_weights.rds")

# Helper: derive financial year from a quarterly date
quarter_fy <- function(date) year(date) + (month(date) >= 7L)

# ------------------------------------------------------------------------------
# Step 3a: ToT decomposition — aggregated (SITC 3)
# ------------------------------------------------------------------------------

tot_decomp_agg <- abs_prices_clean |>
  select(date, dlog_epi_all, dlog_mpi_all,
         dlog_epi_mineral_fuels, dlog_mpi_mineral_fuels) |>
  mutate(fy_epi = quarter_fy(date) - 2L,
         fy_mpi = quarter_fy(date) - 1L) |>
  left_join(trade_weights |> select(fy, w_epi_mineral_fuels) |> rename(fy_epi = fy),
            by = "fy_epi") |>
  left_join(trade_weights |> select(fy, w_mpi_mineral_fuels) |> rename(fy_mpi = fy),
            by = "fy_mpi") |>
  mutate(
    dlog_tot                  = dlog_epi_all - dlog_mpi_all,
    energy_export_contrib     =  w_epi_mineral_fuels * dlog_epi_mineral_fuels,
    non_energy_export_contrib =  dlog_epi_all - energy_export_contrib,
    energy_import_contrib     = -w_mpi_mineral_fuels * dlog_mpi_mineral_fuels,
    non_energy_import_contrib = -dlog_mpi_all - energy_import_contrib
  ) |>
  select(date, dlog_tot, energy_export_contrib, non_energy_export_contrib,
         energy_import_contrib, non_energy_import_contrib)

saveRDS(tot_decomp_agg, "data/processed/tot_decomp_agg.rds")

tot_decomp_agg_qoq <- abs_prices_clean |>
  select(date, qlog_epi_all, qlog_mpi_all,
         qlog_epi_mineral_fuels, qlog_mpi_mineral_fuels) |>
  mutate(fy_epi = quarter_fy(date) - 2L,
         fy_mpi = quarter_fy(date) - 1L) |>
  left_join(trade_weights |> select(fy, w_epi_mineral_fuels) |> rename(fy_epi = fy),
            by = "fy_epi") |>
  left_join(trade_weights |> select(fy, w_mpi_mineral_fuels) |> rename(fy_mpi = fy),
            by = "fy_mpi") |>
  mutate(
    dlog_tot                  = qlog_epi_all - qlog_mpi_all,
    energy_export_contrib     =  w_epi_mineral_fuels * qlog_epi_mineral_fuels,
    non_energy_export_contrib =  qlog_epi_all - energy_export_contrib,
    energy_import_contrib     = -w_mpi_mineral_fuels * qlog_mpi_mineral_fuels,
    non_energy_import_contrib = -qlog_mpi_all - energy_import_contrib
  ) |>
  select(date, dlog_tot, energy_export_contrib, non_energy_export_contrib,
         energy_import_contrib, non_energy_import_contrib)

saveRDS(tot_decomp_agg_qoq, "data/processed/tot_decomp_agg_qoq.rds")

# ------------------------------------------------------------------------------
# Step 3b: ToT decomposition — disaggregated (SITC 32/33/34/56)
# ------------------------------------------------------------------------------

tot_decomp_disagg <- abs_prices_clean |>
  select(date, dlog_epi_all, dlog_mpi_all,
         dlog_epi_coal, dlog_epi_petroleum, dlog_epi_gas,
         dlog_mpi_petroleum, dlog_mpi_fertilisers) |>
  mutate(fy_epi = quarter_fy(date) - 2L,
         fy_mpi = quarter_fy(date) - 1L) |>
  left_join(trade_weights |>
              select(fy, w_epi_coal, w_epi_petroleum, w_epi_gas) |>
              rename(fy_epi = fy),
            by = "fy_epi") |>
  left_join(trade_weights |>
              select(fy, w_mpi_petroleum, w_mpi_fertilisers) |>
              rename(fy_mpi = fy),
            by = "fy_mpi") |>
  mutate(
    dlog_tot                  = dlog_epi_all - dlog_mpi_all,
    coal_export_contrib       =  w_epi_coal        * dlog_epi_coal,
    petroleum_export_contrib  =  w_epi_petroleum   * dlog_epi_petroleum,
    gas_export_contrib        =  w_epi_gas         * dlog_epi_gas,
    non_energy_export_contrib =  dlog_epi_all - coal_export_contrib
                                              - petroleum_export_contrib
                                              - gas_export_contrib,
    petroleum_import_contrib  = -w_mpi_petroleum   * dlog_mpi_petroleum,
    fertiliser_import_contrib = -w_mpi_fertilisers * dlog_mpi_fertilisers,
    non_energy_import_contrib = -dlog_mpi_all - petroleum_import_contrib
                                              - fertiliser_import_contrib
  ) |>
  select(date, dlog_tot, coal_export_contrib, petroleum_export_contrib,
         gas_export_contrib, non_energy_export_contrib,
         petroleum_import_contrib, fertiliser_import_contrib,
         non_energy_import_contrib)

saveRDS(tot_decomp_disagg, "data/processed/tot_decomp_disagg.rds")

tot_decomp_disagg_qoq <- abs_prices_clean |>
  select(date, qlog_epi_all, qlog_mpi_all,
         qlog_epi_coal, qlog_epi_petroleum, qlog_epi_gas,
         qlog_mpi_petroleum, qlog_mpi_fertilisers) |>
  mutate(fy_epi = quarter_fy(date) - 2L,
         fy_mpi = quarter_fy(date) - 1L) |>
  left_join(trade_weights |>
              select(fy, w_epi_coal, w_epi_petroleum, w_epi_gas) |>
              rename(fy_epi = fy),
            by = "fy_epi") |>
  left_join(trade_weights |>
              select(fy, w_mpi_petroleum, w_mpi_fertilisers) |>
              rename(fy_mpi = fy),
            by = "fy_mpi") |>
  mutate(
    dlog_tot                  = qlog_epi_all - qlog_mpi_all,
    coal_export_contrib       =  w_epi_coal        * qlog_epi_coal,
    petroleum_export_contrib  =  w_epi_petroleum   * qlog_epi_petroleum,
    gas_export_contrib        =  w_epi_gas         * qlog_epi_gas,
    non_energy_export_contrib =  qlog_epi_all - coal_export_contrib
                                              - petroleum_export_contrib
                                              - gas_export_contrib,
    petroleum_import_contrib  = -w_mpi_petroleum   * qlog_mpi_petroleum,
    fertiliser_import_contrib = -w_mpi_fertilisers * qlog_mpi_fertilisers,
    non_energy_import_contrib = -qlog_mpi_all - petroleum_import_contrib
                                              - fertiliser_import_contrib
  ) |>
  select(date, dlog_tot, coal_export_contrib, petroleum_export_contrib,
         gas_export_contrib, non_energy_export_contrib,
         petroleum_import_contrib, fertiliser_import_contrib,
         non_energy_import_contrib)

saveRDS(tot_decomp_disagg_qoq, "data/processed/tot_decomp_disagg_qoq.rds")

# ------------------------------------------------------------------------------
# Step A: Clean SITC 1-digit price indexes
# ------------------------------------------------------------------------------

sitc1_prices_clean <- readRDS("data/raw/sitc1_trade_prices.rds") |>
  select(table_title, date, series, value) |>
  mutate(
    direction  = if_else(str_detect(table_title, "Import"), "mpi", "epi"),
    sitc_digit = case_when(
      direction == "mpi" ~ str_extract(series, "(?<=Index Numbers ;  )\\d"),
      direction == "epi" ~ str_extract(series, "(?<=\\()\\d(?=\\))")
    )
  ) |>
  filter(!is.na(sitc_digit)) |>
  select(date, direction, sitc_digit, value) |>
  pivot_wider(names_from = c(direction, sitc_digit), values_from = value,
              names_glue = "{direction}_sitc{sitc_digit}") |>
  arrange(date) |>
  mutate(across(-date, \(x) log(x / lag(x, 4)), .names = "dlog_{.col}")) |>
  mutate(across(c(-date, -starts_with("dlog_")),
                \(x) log(x / lag(x, 1)), .names = "qlog_{.col}"))

saveRDS(sitc1_prices_clean, "data/processed/sitc1_prices_clean.rds")

# ------------------------------------------------------------------------------
# Step B: SITC 1-digit value-share weights
# ------------------------------------------------------------------------------

sitc1_value_totals <- readRDS("data/raw/abs_trade_values.rds") |>
  filter(series_id %in% c("A1827881R", "A1828721W")) |>
  mutate(
    direction = if_else(series_id == "A1827881R", "exports", "imports"),
    fy        = year(date) + (month(date) >= 7L)
  ) |>
  summarise(total = sum(value, na.rm = TRUE), .by = c(fy, direction))

sitc1_weights <- readRDS("data/raw/sitc1_trade_values_data.rds") |>
  select(table_title, date, series, value) |>
  mutate(
    direction  = if_else(str_detect(table_title, "EXPORTS"), "exports", "imports"),
    sitc_digit = str_extract(series, "^\\d")
  ) |>
  filter(!is.na(sitc_digit)) |>
  mutate(fy = year(date) + (month(date) >= 7L)) |>
  summarise(value = sum(value, na.rm = TRUE), .by = c(fy, direction, sitc_digit)) |>
  left_join(sitc1_value_totals, by = c("fy", "direction")) |>
  mutate(weight = value / total) |>
  select(fy, direction, sitc_digit, weight)

saveRDS(sitc1_weights, "data/processed/sitc1_weights.rds")

# ------------------------------------------------------------------------------
# Step C: SITC 1-digit ToT decompositions (YoY and QoQ)
# ------------------------------------------------------------------------------

make_sitc1_decomp <- function(prices_clean, prefix, tot_df) {
  prices_clean |>
    select(date, matches(paste0("^", prefix, "_(epi|mpi)_sitc"))) |>
    pivot_longer(-date, names_to = "col", values_to = "price_change") |>
    mutate(
      direction  = if_else(str_detect(col, "_epi_"), "epi", "mpi"),
      sitc_digit = str_extract(col, "\\d+$")
    ) |>
    mutate(
      fy_join = if_else(
        direction == "epi",
        quarter_fy(date) - 2L,
        quarter_fy(date) - 1L
      )
    ) |>
    left_join(
      sitc1_weights |>
        mutate(direction = if_else(direction == "exports", "epi", "mpi"),
               fy_join   = fy) |>
        select(-fy),
      by = c("fy_join", "direction", "sitc_digit")
    ) |>
    mutate(contrib = if_else(direction == "epi", 1, -1) * weight * price_change) |>
    summarise(contrib = sum(contrib, na.rm = TRUE), .by = c(date, sitc_digit)) |>
    left_join(tot_df, by = "date")
}

yoy_tot <- abs_prices_clean |>
  select(date, dlog_epi_all, dlog_mpi_all) |>
  mutate(dlog_tot = dlog_epi_all - dlog_mpi_all) |>
  select(date, dlog_tot)

qoq_tot <- abs_prices_clean |>
  select(date, qlog_epi_all, qlog_mpi_all) |>
  mutate(dlog_tot = qlog_epi_all - qlog_mpi_all) |>
  select(date, dlog_tot)

sitc1_decomp_yoy <- make_sitc1_decomp(sitc1_prices_clean, "dlog", yoy_tot)
sitc1_decomp_qoq <- make_sitc1_decomp(sitc1_prices_clean, "qlog", qoq_tot)

saveRDS(sitc1_decomp_yoy, "data/processed/sitc1_decomp_yoy.rds")
saveRDS(sitc1_decomp_qoq, "data/processed/sitc1_decomp_qoq.rds")
