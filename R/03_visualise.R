# 03_visualise.R
# Produce charts; save to output/figures/

library(tidyverse)

tot_decomp_agg        <- readRDS("data/processed/tot_decomp_agg.rds")
tot_decomp_disagg     <- readRDS("data/processed/tot_decomp_disagg.rds")
tot_decomp_agg_qoq    <- readRDS("data/processed/tot_decomp_agg_qoq.rds")
tot_decomp_disagg_qoq <- readRDS("data/processed/tot_decomp_disagg_qoq.rds")
sitc1_decomp_yoy      <- readRDS("data/processed/sitc1_decomp_yoy.rds")
sitc1_decomp_qoq      <- readRDS("data/processed/sitc1_decomp_qoq.rds")

# ------------------------------------------------------------------------------
# Chart 1: Total export vs import price contributions
# ------------------------------------------------------------------------------

colors_1 <- c(
  "Export prices" = "#2166ac",
  "Import prices" = "#d6604d"
)

chart1_data <- tot_decomp_agg |>
  filter(date >= "2000-01-01") |>
  mutate(
    "Export prices" = (energy_export_contrib + non_energy_export_contrib) * 100,
    "Import prices" = (energy_import_contrib + non_energy_import_contrib) * 100,
    dlog_tot_pct    = dlog_tot * 100
  ) |>
  pivot_longer(cols = c("Export prices", "Import prices"),
               names_to = "component", values_to = "value_pct") |>
  mutate(component = factor(component, levels = names(colors_1)))

p1 <- ggplot(chart1_data, aes(x = date)) +
  geom_col(aes(y = value_pct, fill = component), position = "stack", width = 70) +
  geom_line(aes(y = dlog_tot_pct), color = "black", linewidth = 0.7) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  scale_fill_manual(values = colors_1) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y",
               limits = as.Date(c("2000-01-01", "2025-12-31"))) +
  labs(
    title    = "Australia's terms of trade: export and import price contributions",
    subtitle = "Year-on-year log change, percentage points",
    x = NULL, y = "Percentage points", fill = NULL,
    caption  = "Sources: ABS Cat. 6457.0, 5368.0"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("output/figures/tot_decomp_1_export_import.png", p1,
       width = 18, height = 10, units = "cm", dpi = 150)

# ------------------------------------------------------------------------------
# Chart 2: SITC 3 mineral fuels + other
# ------------------------------------------------------------------------------

colors_2 <- c(
  "Energy exports (SITC 3)" = "#2166ac",
  "Other exports"           = "#92c5de",
  "Energy imports (SITC 3)" = "#d6604d",
  "Other imports"           = "#f4a582"
)

chart2_data <- tot_decomp_agg |>
  filter(date >= "2000-01-01") |>
  mutate(
    "Energy exports (SITC 3)" = energy_export_contrib     * 100,
    "Other exports"           = non_energy_export_contrib * 100,
    "Energy imports (SITC 3)" = energy_import_contrib     * 100,
    "Other imports"           = non_energy_import_contrib * 100,
    dlog_tot_pct              = dlog_tot * 100
  ) |>
  pivot_longer(cols = all_of(names(colors_2)),
               names_to = "component", values_to = "value_pct") |>
  mutate(component = factor(component, levels = names(colors_2)))

p2 <- ggplot(chart2_data, aes(x = date)) +
  geom_col(aes(y = value_pct, fill = component), position = "stack", width = 70) +
  geom_line(aes(y = dlog_tot_pct), color = "black", linewidth = 0.7) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  scale_fill_manual(values = colors_2) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y",
               limits = as.Date(c("2000-01-01", "2025-12-31"))) +
  labs(
    title    = "Australia's terms of trade: mineral fuels contributions (SITC 3)",
    subtitle = "Year-on-year log change, percentage points",
    x = NULL, y = "Percentage points", fill = NULL,
    caption  = "Sources: ABS Cat. 6457.0, 5368.0"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("output/figures/tot_decomp_2_sitc3.png", p2,
       width = 18, height = 10, units = "cm", dpi = 150)

# ------------------------------------------------------------------------------
# Chart 3: SITC 32/33/34 exports, SITC 33/56 imports + other (net)
# ------------------------------------------------------------------------------

colors_3 <- c(
  "Coal exports (SITC 32)"      = "#1d4e89",
  "Petroleum exports (SITC 33)" = "#2166ac",
  "Gas/LNG exports (SITC 34)"   = "#92c5de",
  "Petroleum imports (SITC 33)" = "#b2182b",
  "Fertiliser imports (SITC 56)"= "#d6604d",
  "Other (net)"                 = "#bababa"
)

chart3_data <- tot_decomp_disagg |>
  filter(date >= "2000-01-01") |>
  mutate(
    "Coal exports (SITC 32)"       = coal_export_contrib       * 100,
    "Petroleum exports (SITC 33)"  = petroleum_export_contrib  * 100,
    "Gas/LNG exports (SITC 34)"    = gas_export_contrib        * 100,
    "Petroleum imports (SITC 33)"  = petroleum_import_contrib  * 100,
    "Fertiliser imports (SITC 56)" = fertiliser_import_contrib * 100,
    "Other (net)"                  = (non_energy_export_contrib +
                                      non_energy_import_contrib) * 100,
    dlog_tot_pct                   = dlog_tot * 100
  ) |>
  pivot_longer(cols = all_of(names(colors_3)),
               names_to = "component", values_to = "value_pct") |>
  mutate(component = factor(component, levels = names(colors_3)))

p3 <- ggplot(chart3_data, aes(x = date)) +
  geom_col(aes(y = value_pct, fill = component), position = "stack", width = 70) +
  geom_line(aes(y = dlog_tot_pct), color = "black", linewidth = 0.7) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  scale_fill_manual(values = colors_3) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y",
               limits = as.Date(c("2000-01-01", "2025-12-31"))) +
  labs(
    title    = "Australia's terms of trade: commodity-level contributions",
    subtitle = "Year-on-year log change, percentage points",
    x = NULL, y = "Percentage points", fill = NULL,
    caption  = "Sources: ABS Cat. 6457.0, 5368.0"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("output/figures/tot_decomp_3_sitc_disagg.png", p3,
       width = 18, height = 10, units = "cm", dpi = 150)

# ------------------------------------------------------------------------------
# Chart 4: QoQ — Total export vs import price contributions
# ------------------------------------------------------------------------------

chart4_data <- tot_decomp_agg_qoq |>
  filter(date >= "2000-01-01") |>
  mutate(
    "Export prices" = (energy_export_contrib + non_energy_export_contrib) * 100,
    "Import prices" = (energy_import_contrib + non_energy_import_contrib) * 100,
    dlog_tot_pct    = dlog_tot * 100
  ) |>
  pivot_longer(cols = c("Export prices", "Import prices"),
               names_to = "component", values_to = "value_pct") |>
  mutate(component = factor(component, levels = names(colors_1)))

p4 <- ggplot(chart4_data, aes(x = date)) +
  geom_col(aes(y = value_pct, fill = component), position = "stack", width = 70) +
  geom_line(aes(y = dlog_tot_pct), color = "black", linewidth = 0.7) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  scale_fill_manual(values = colors_1) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y",
               limits = as.Date(c("2000-01-01", "2025-12-31"))) +
  labs(
    title    = "Australia's terms of trade: export and import price contributions",
    subtitle = "Quarter-on-quarter log change, percentage points",
    x = NULL, y = "Percentage points", fill = NULL,
    caption  = "Sources: ABS Cat. 6457.0, 5368.0"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("output/figures/tot_decomp_4_qoq_export_import.png", p4,
       width = 18, height = 10, units = "cm", dpi = 150)

# ------------------------------------------------------------------------------
# Chart 5: QoQ — SITC 3 mineral fuels + other
# ------------------------------------------------------------------------------

chart5_data <- tot_decomp_agg_qoq |>
  filter(date >= "2000-01-01") |>
  mutate(
    "Energy exports (SITC 3)" = energy_export_contrib     * 100,
    "Other exports"           = non_energy_export_contrib * 100,
    "Energy imports (SITC 3)" = energy_import_contrib     * 100,
    "Other imports"           = non_energy_import_contrib * 100,
    dlog_tot_pct              = dlog_tot * 100
  ) |>
  pivot_longer(cols = all_of(names(colors_2)),
               names_to = "component", values_to = "value_pct") |>
  mutate(component = factor(component, levels = names(colors_2)))

p5 <- ggplot(chart5_data, aes(x = date)) +
  geom_col(aes(y = value_pct, fill = component), position = "stack", width = 70) +
  geom_line(aes(y = dlog_tot_pct), color = "black", linewidth = 0.7) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  scale_fill_manual(values = colors_2) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y",
               limits = as.Date(c("2000-01-01", "2025-12-31"))) +
  labs(
    title    = "Australia's terms of trade: mineral fuels contributions (SITC 3)",
    subtitle = "Quarter-on-quarter log change, percentage points",
    x = NULL, y = "Percentage points", fill = NULL,
    caption  = "Sources: ABS Cat. 6457.0, 5368.0"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("output/figures/tot_decomp_5_qoq_sitc3.png", p5,
       width = 18, height = 10, units = "cm", dpi = 150)

# ------------------------------------------------------------------------------
# Chart 6: QoQ — SITC 32/33/34 exports, SITC 33/56 imports + other (net)
# ------------------------------------------------------------------------------

chart6_data <- tot_decomp_disagg_qoq |>
  filter(date >= "2000-01-01") |>
  mutate(
    "Coal exports (SITC 32)"       = coal_export_contrib       * 100,
    "Petroleum exports (SITC 33)"  = petroleum_export_contrib  * 100,
    "Gas/LNG exports (SITC 34)"    = gas_export_contrib        * 100,
    "Petroleum imports (SITC 33)"  = petroleum_import_contrib  * 100,
    "Fertiliser imports (SITC 56)" = fertiliser_import_contrib * 100,
    "Other (net)"                  = (non_energy_export_contrib +
                                      non_energy_import_contrib) * 100,
    dlog_tot_pct                   = dlog_tot * 100
  ) |>
  pivot_longer(cols = all_of(names(colors_3)),
               names_to = "component", values_to = "value_pct") |>
  mutate(component = factor(component, levels = names(colors_3)))

p6 <- ggplot(chart6_data, aes(x = date)) +
  geom_col(aes(y = value_pct, fill = component), position = "stack", width = 70) +
  geom_line(aes(y = dlog_tot_pct), color = "black", linewidth = 0.7) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  scale_fill_manual(values = colors_3) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y",
               limits = as.Date(c("2000-01-01", "2025-12-31"))) +
  labs(
    title    = "Australia's terms of trade: commodity-level contributions",
    subtitle = "Quarter-on-quarter log change, percentage points",
    x = NULL, y = "Percentage points", fill = NULL,
    caption  = "Sources: ABS Cat. 6457.0, 5368.0"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("output/figures/tot_decomp_6_qoq_sitc_disagg.png", p6,
       width = 18, height = 10, units = "cm", dpi = 150)

# ------------------------------------------------------------------------------
# Shared setup for Charts 7–8: SITC 1-digit decomposition
# ------------------------------------------------------------------------------

sitc_labels <- c(
  "0" = "SITC 0: Food and live animals",
  "1" = "SITC 1: Beverages and tobacco",
  "2" = "SITC 2: Crude materials, excl. fuels",
  "3" = "SITC 3: Mineral fuels",
  "4" = "SITC 4: Animal/vegetable oils",
  "5" = "SITC 5: Chemicals",
  "6" = "SITC 6: Manufactured goods",
  "7" = "SITC 7: Machinery and transport",
  "8" = "SITC 8: Miscellaneous manufactures",
  "9" = "SITC 9: Other commodities"
)

sitc_colors <- c(
  "SITC 0: Food and live animals"        = "#a6cee3",
  "SITC 1: Beverages and tobacco"        = "#1f78b4",
  "SITC 2: Crude materials, excl. fuels" = "#b2df8a",
  "SITC 3: Mineral fuels"                = "#33a02c",
  "SITC 4: Animal/vegetable oils"        = "#fb9a99",
  "SITC 5: Chemicals"                    = "#e31a1c",
  "SITC 6: Manufactured goods"           = "#fdbf6f",
  "SITC 7: Machinery and transport"      = "#ff7f00",
  "SITC 8: Miscellaneous manufactures"   = "#cab2d6",
  "SITC 9: Other commodities"            = "#6a3d9a",
  "Other (net)"                          = "#bababa"
)

prep_sitc1_chart <- function(decomp) {
  contrib <- decomp |>
    filter(date >= "2000-01-01", !is.na(contrib)) |>
    mutate(
      value_pct = contrib * 100,
      component = sitc_labels[sitc_digit]
    )

  residual <- decomp |>
    filter(date >= "2000-01-01") |>
    distinct(date, dlog_tot) |>
    left_join(
      contrib |>
        summarise(total = sum(value_pct, na.rm = TRUE), .by = date),
      by = "date"
    ) |>
    transmute(
      date,
      dlog_tot,
      value_pct = dlog_tot * 100 - coalesce(total, 0),
      component = "Other (net)"
    )

  bind_rows(contrib, residual) |>
    mutate(
      dlog_tot_pct = dlog_tot * 100,
      component    = factor(component, levels = names(sitc_colors))
    )
}

# ------------------------------------------------------------------------------
# Chart 7: YoY — SITC 1-digit contributions
# ------------------------------------------------------------------------------

chart7_data <- prep_sitc1_chart(sitc1_decomp_yoy)

p7 <- ggplot(chart7_data, aes(x = date)) +
  geom_col(aes(y = value_pct, fill = component), position = "stack", width = 70) +
  geom_line(aes(y = dlog_tot_pct), color = "black", linewidth = 0.7) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  scale_fill_manual(values = sitc_colors) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y",
               limits = as.Date(c("2000-01-01", "2025-12-31"))) +
  labs(
    title    = "Australia's terms of trade: SITC 1-digit contributions",
    subtitle = "Year-on-year log change, percentage points",
    x = NULL, y = "Percentage points", fill = NULL,
    caption  = "Sources: ABS Cat. 6457.0, 5368.0"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("output/figures/tot_decomp_7_sitc1_yoy.png", p7,
       width = 18, height = 10, units = "cm", dpi = 150)

# ------------------------------------------------------------------------------
# Chart 8: QoQ — SITC 1-digit contributions
# ------------------------------------------------------------------------------

chart8_data <- prep_sitc1_chart(sitc1_decomp_qoq)

p8 <- ggplot(chart8_data, aes(x = date)) +
  geom_col(aes(y = value_pct, fill = component), position = "stack", width = 70) +
  geom_line(aes(y = dlog_tot_pct), color = "black", linewidth = 0.7) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  scale_fill_manual(values = sitc_colors) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y",
               limits = as.Date(c("2000-01-01", "2025-12-31"))) +
  labs(
    title    = "Australia's terms of trade: SITC 1-digit contributions",
    subtitle = "Quarter-on-quarter log change, percentage points",
    x = NULL, y = "Percentage points", fill = NULL,
    caption  = "Sources: ABS Cat. 6457.0, 5368.0"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("output/figures/tot_decomp_8_sitc1_qoq.png", p8,
       width = 18, height = 10, units = "cm", dpi = 150)
