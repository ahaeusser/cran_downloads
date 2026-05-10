
# Download statistics for the R package {echos} ################################


# Setup ########################################################################

# Load libraries ===============================================================

library(adjustedcranlogs)
library(dplyr)
library(lubridate)
library(ggplot2)
library(scales)
library(zoo)
library(patchwork)
library(tscv)
library(gt)
library(glue)


# Configuration ================================================================

# Change default location and time
Sys.setlocale("LC_TIME", "C")

package_name <- "echos"

start_date <- as.Date("2025-02-01")
end_date   <- Sys.Date()

rolling_window <- 7
release_window_days <- 30

col_main  <- "steelblue"
col_light <- "lightsteelblue"

release_dates <- tibble::tribble(
  ~version, ~release_date,
  "v1.0.1", as.Date("2025-02-11"),
  "v1.0.2", as.Date("2025-06-23"),
  "v1.0.3", as.Date("2026-02-22")
)


# Data #########################################################################

# Download data ================================================================

downloads_raw <- adj_cran_downloads(
  packages = package_name,
  from = start_date,
  to = end_date
)

downloads <- downloads_raw |>
  mutate(
    date = as.Date(date),
    month = floor_date(date, unit = "month"),
    downloads = pmax(adjusted_downloads, 0)
  )


# Calculations #################################################################

# Daily data ===================================================================

downloads <- downloads |>
  arrange(date) |>
  mutate(
    rolling_downloads = rollmean(
      downloads,
      k = rolling_window,
      fill = NA,
      align = "right"
    ),
    cumulative_downloads = cumsum(downloads)
  )

analysis_start_date <- min(downloads$date, na.rm = TRUE)
analysis_end_date <- max(downloads$date, na.rm = TRUE)

total_downloads <- sum(downloads$downloads, na.rm = TRUE)
mean_daily_downloads <- mean(downloads$downloads, na.rm = TRUE)
median_daily_downloads <- median(downloads$downloads, na.rm = TRUE)
sd_daily_downloads <- sd(downloads$downloads, na.rm = TRUE)
min_daily_downloads <- min(downloads$downloads, na.rm = TRUE)
max_daily_downloads <- max(downloads$downloads, na.rm = TRUE)

last_cumulative_observation <- downloads |>
  slice_tail(n = 1)


# Summary table ================================================================

daily_download_summary <- tibble::tibble(
  Statistic = c(
    "Minimum",
    "Mean",
    "Median",
    "Standard deviation",
    "Maximum",
    "Total"
  ),
  Value = c(
    min_daily_downloads,
    mean_daily_downloads,
    median_daily_downloads,
    sd_daily_downloads,
    max_daily_downloads,
    total_downloads
  )
)


# Monthly table ================================================================

monthly_downloads <- downloads |>
  group_by(month) |>
  summarise(
    downloads = sum(downloads, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(month) |>
  mutate(
    downloads_lag = lag(downloads),
    change_abs = downloads - downloads_lag,
    change_pct = downloads / downloads_lag - 1,
    label = if_else(
      is.na(change_pct),
      comma(downloads),
      paste0(
        comma(downloads),
        "\n",
        if_else(change_pct > 0, "+", ""),
        percent(change_pct, accuracy = 1)
      )
    )
  )

monthly_download_table <- monthly_downloads |>
  transmute(
    Month = format(month, "%Y-%m"),
    Downloads = downloads,
    `Absolute change` = change_abs,
    `Relative change` = change_pct
  )


# Monthly interpretation data ==================================================

last_observed_month <- floor_date(analysis_end_date, unit = "month")
last_day_observed_month <- ceiling_date(last_observed_month, unit = "month") - days(1)

complete_monthly_downloads <- if (analysis_end_date < last_day_observed_month) {
  monthly_downloads |>
    filter(month < last_observed_month)
} else {
  monthly_downloads
}

strongest_month <- complete_monthly_downloads |>
  slice_max(downloads, n = 1, with_ties = FALSE)

latest_month <- monthly_downloads |>
  slice_tail(n = 1)

latest_month_is_incomplete <- analysis_end_date < last_day_observed_month

top_growth_months <- complete_monthly_downloads |>
  filter(!is.na(change_pct), change_pct > 0) |>
  arrange(desc(change_pct)) |>
  slice_head(n = 3) |>
  mutate(
    growth_label = paste0(
      format(month, "%B %Y"),
      " (",
      percent(change_pct, accuracy = 0.1),
      ")"
    )
  )

top_decline_months <- complete_monthly_downloads |>
  filter(!is.na(change_pct), change_pct < 0) |>
  arrange(change_pct) |>
  slice_head(n = 3) |>
  mutate(
    decline_label = paste0(
      format(month, "%B %Y"),
      " (",
      percent(change_pct, accuracy = 0.1),
      ")"
    )
  )

growth_months_text <- if (nrow(top_growth_months) > 0) {
  paste(top_growth_months$growth_label, collapse = ", ")
} else {
  "no months with positive month-over-month growth"
}

decline_months_text <- if (nrow(top_decline_months) > 0) {
  paste(top_decline_months$decline_label, collapse = ", ")
} else {
  "no months with negative month-over-month change"
}


# Release data =================================================================

release_points <- release_dates |>
  filter(
    release_date >= min(downloads$date),
    release_date <= max(downloads$date)
  ) |>
  left_join(
    downloads |>
      select(date, cumulative_downloads),
    by = c("release_date" = "date")
  ) |>
  mutate(
    release_label = paste0(version, "\n", format(release_date, "%Y-%m-%d"))
  )

release_impact <- release_dates |>
  rowwise() |>
  mutate(
    downloads_before = sum(
      downloads$downloads[
        downloads$date >= release_date - days(release_window_days) &
          downloads$date < release_date
      ],
      na.rm = TRUE
    ),
    downloads_after = sum(
      downloads$downloads[
        downloads$date > release_date &
          downloads$date <= release_date + days(release_window_days)
      ],
      na.rm = TRUE
    ),
    change_abs = downloads_after - downloads_before,
    change_pct = if_else(
      downloads_before > 0,
      change_abs / downloads_before,
      NA_real_
    )
  ) |>
  ungroup() |>
  mutate(
    before_label = comma(downloads_before),
    after_label = if_else(
      is.na(change_pct),
      comma(downloads_after),
      paste0(
        comma(downloads_after),
        "\n",
        if_else(change_pct > 0, "+", ""),
        percent(change_pct, accuracy = 1)
      )
    )
  )

release_impact_table <- release_impact |>
  transmute(
    Version = version,
    `Release date` = release_date,
    `Downloads before` = downloads_before,
    `Downloads after` = downloads_after,
    `Absolute change` = change_abs,
    `Relative change` = change_pct
  )

release_impact_with_change <- release_impact |>
  filter(!is.na(change_pct))

strongest_release <- release_impact_with_change |>
  slice_max(change_pct, n = 1, with_ties = FALSE)

release_summary_sentences <- release_impact |>
  mutate(
    sentence = if_else(
      is.na(change_pct),
      glue(
        "After {version}, downloads reached {comma(downloads_after)} in the ",
        "{release_window_days} days after the release. A relative change is ",
        "not reported because there were no downloads in the comparison window before the release."
      ),
      glue(
        "After {version}, downloads increased from {comma(downloads_before)} ",
        "before the release to {comma(downloads_after)} after the release. ",
        "This corresponds to an increase of {comma(change_abs)} downloads, ",
        "or {percent(change_pct, accuracy = 0.1)}."
      )
    )
  ) |>
  pull(sentence)

release_summary_text <- paste0(
  "- ",
  release_summary_sentences,
  collapse = "\n"
)


# Top download days ============================================================

top_download_days <- downloads |>
  arrange(desc(downloads)) |>
  slice_head(n = 10) |>
  transmute(
    Date = date,
    Downloads = downloads
  )


# Label positions ==============================================================

daily_label_y <- max(downloads$downloads, na.rm = TRUE) * 0.95
cumulative_label_nudge_y <- max(downloads$cumulative_downloads, na.rm = TRUE) * 0.04
total_label_nudge_y <- -max(downloads$cumulative_downloads, na.rm = TRUE) * 0.05


# Reporting tables #############################################################

# Daily summary gt table =======================================================

daily_download_summary_gt <- daily_download_summary |>
  gt() |>
  tab_header(
    title = "Daily download summary",
    subtitle = paste0(
      "Adjusted downloads, ",
      format(analysis_start_date, "%Y-%m-%d"),
      " to ",
      format(analysis_end_date, "%Y-%m-%d")
    )
  ) |>
  fmt_number(
    columns = Value,
    decimals = 1,
    use_seps = TRUE
  ) |>
  cols_align(
    align = "left",
    columns = Statistic
  ) |>
  cols_align(
    align = "right",
    columns = Value
  ) |>
  tab_options(
    table.font.size = px(12),
    heading.title.font.size = px(14),
    heading.subtitle.font.size = px(11),
    table.background.color = "white",
    heading.background.color = "white",
    column_labels.background.color = "white",
    table.font.color = "#202123"
  )


# Monthly gt table =============================================================

monthly_download_table_gt <- monthly_download_table |>
  gt() |>
  tab_header(
    title = "Monthly downloads",
    subtitle = "Adjusted downloads"
  ) |>
  fmt_number(
    columns = c(Downloads, `Absolute change`),
    decimals = 0,
    use_seps = TRUE
  ) |>
  fmt_percent(
    columns = `Relative change`,
    decimals = 1
  ) |>
  sub_missing(
    columns = everything(),
    missing_text = "—"
  ) |>
  cols_align(
    align = "right",
    columns = c(Downloads, `Absolute change`, `Relative change`)
  ) |>
  tab_options(
    table.font.size = px(12),
    heading.title.font.size = px(14),
    heading.subtitle.font.size = px(11),
    table.background.color = "white",
    heading.background.color = "white",
    column_labels.background.color = "white",
    table.font.color = "#202123"
  )


# Release impact gt table ======================================================

release_impact_gt <- release_impact_table |>
  gt() |>
  tab_header(
    title = "Release impact",
    subtitle = paste0("Adjusted downloads, ±", release_window_days, " days")
  ) |>
  fmt_date(
    columns = `Release date`,
    date_style = "iso"
  ) |>
  fmt_number(
    columns = c(`Downloads before`, `Downloads after`, `Absolute change`),
    decimals = 0,
    use_seps = TRUE
  ) |>
  fmt_percent(
    columns = `Relative change`,
    decimals = 1
  ) |>
  sub_missing(
    columns = everything(),
    missing_text = "—"
  ) |>
  cols_align(
    align = "right",
    columns = c(
      `Downloads before`,
      `Downloads after`,
      `Absolute change`,
      `Relative change`
    )
  ) |>
  tab_options(
    table.font.size = px(12),
    heading.title.font.size = px(14),
    heading.subtitle.font.size = px(11),
    table.background.color = "white",
    heading.background.color = "white",
    column_labels.background.color = "white",
    table.font.color = "#202123"
  )


# Top download days gt table ===================================================

top_download_days_gt <- top_download_days |>
  gt() |>
  tab_header(
    title = "Top download days",
    subtitle = "Ten days with the highest adjusted downloads"
  ) |>
  fmt_date(
    columns = Date,
    date_style = "iso"
  ) |>
  fmt_number(
    columns = Downloads,
    decimals = 0,
    use_seps = TRUE
  ) |>
  cols_align(
    align = "right",
    columns = Downloads
  ) |>
  tab_options(
    table.font.size = px(12),
    heading.title.font.size = px(14),
    heading.subtitle.font.size = px(11),
    table.background.color = "white",
    heading.background.color = "white",
    column_labels.background.color = "white",
    table.font.color = "#202123"
  )


# Print gt tables ==============================================================

daily_download_summary_gt
monthly_download_table_gt
release_impact_gt
top_download_days_gt


# Figure 1: Core download patterns #############################################

# Plot (a): Daily downloads ====================================================

# Base plot --------------------------------------------------------------------

p_daily <- ggplot(downloads, aes(x = date))

# Lines ------------------------------------------------------------------------

p_daily <- p_daily +
  geom_line(
    aes(y = downloads),
    color = col_light,
    linewidth = 0.8
  )

p_daily <- p_daily +
  geom_line(
    aes(y = rolling_downloads),
    color = col_main,
    linewidth = 1.1
  )

# Release labels ---------------------------------------------------------------

p_daily <- p_daily +
  geom_vline(
    data = release_points,
    aes(xintercept = release_date),
    linetype = "dotted",
    color = "grey40",
    linewidth = 0.6
  )

p_daily <- p_daily +
  geom_label(
    data = release_points,
    aes(
      x = release_date,
      y = daily_label_y,
      label = release_label
    ),
    size = 3.1,
    vjust = 1,
    label.padding = unit(0.15, "lines")
  )

# Scales and labels ------------------------------------------------------------

p_daily <- p_daily +
  scale_y_continuous(
    labels = comma,
    expand = expansion(mult = c(0.02, 0.12))
  )

p_daily <- p_daily +
  labs(
    title = "(a) Daily downloads",
    subtitle = "Adjusted values and 7-day average",
    x = NULL,
    y = NULL
  )

p_daily <- p_daily +
  theme_tscv()


# Plot (b): Cumulative downloads ==============================================

# Base plot --------------------------------------------------------------------

p_cumulative <- ggplot(downloads, aes(x = date, y = cumulative_downloads))

# Line -------------------------------------------------------------------------

p_cumulative <- p_cumulative +
  geom_line(
    color = col_main,
    linewidth = 1
  )

# Release labels ---------------------------------------------------------------

p_cumulative <- p_cumulative +
  geom_vline(
    data = release_points,
    aes(xintercept = release_date),
    linetype = "dotted",
    color = "grey40",
    linewidth = 0.6
  )

p_cumulative <- p_cumulative +
  geom_point(
    data = release_points,
    aes(x = release_date, y = cumulative_downloads),
    color = "grey30",
    size = 2
  )

p_cumulative <- p_cumulative +
  geom_label(
    data = release_points,
    aes(
      x = release_date,
      y = cumulative_downloads,
      label = release_label
    ),
    nudge_y = cumulative_label_nudge_y,
    size = 3.1,
    label.padding = unit(0.15, "lines")
  )

# Total label ------------------------------------------------------------------

p_cumulative <- p_cumulative +
  geom_point(
    data = last_cumulative_observation,
    color = col_main,
    size = 2.5
  )

p_cumulative <- p_cumulative +
  geom_label(
    data = last_cumulative_observation,
    aes(label = paste0("Total: ", comma(cumulative_downloads))),
    nudge_x = -20,
    nudge_y = total_label_nudge_y,
    hjust = 1,
    vjust = 1,
    label.padding = unit(0.2, "lines")
  )

# Scales and labels ------------------------------------------------------------

p_cumulative <- p_cumulative +
  scale_y_continuous(
    labels = comma,
    expand = expansion(mult = c(0.02, 0.15))
  )

p_cumulative <- p_cumulative +
  scale_x_date(
    expand = expansion(mult = c(0.02, 0.08))
  )

p_cumulative <- p_cumulative +
  labs(
    title = "(b) Cumulative downloads",
    subtitle = "Releases and final total",
    x = NULL,
    y = NULL
  )

p_cumulative <- p_cumulative +
  theme_tscv()


# Plot (c): Distribution of daily downloads ====================================

# Base plot --------------------------------------------------------------------

p_daily_histogram <- ggplot(downloads, aes(x = downloads))

# Histogram --------------------------------------------------------------------

p_daily_histogram <- p_daily_histogram +
  geom_histogram(
    bins = 30,
    fill = col_light,
    color = "white"
  )

# Mean and median lines --------------------------------------------------------

p_daily_histogram <- p_daily_histogram +
  geom_vline(
    xintercept = mean_daily_downloads,
    color = col_main,
    linewidth = 1,
    linetype = "dashed"
  )

p_daily_histogram <- p_daily_histogram +
  geom_vline(
    xintercept = median_daily_downloads,
    color = "black",
    linewidth = 1,
    linetype = "dotted"
  )

# Mean and median labels -------------------------------------------------------

p_daily_histogram <- p_daily_histogram +
  annotate(
    "label",
    x = mean_daily_downloads,
    y = Inf,
    label = paste0("Mean: ", comma(round(mean_daily_downloads, 1))),
    vjust = 1.5,
    hjust = -0.05,
    color = col_main
  )

p_daily_histogram <- p_daily_histogram +
  annotate(
    "label",
    x = median_daily_downloads,
    y = Inf,
    label = paste0("Median: ", comma(median_daily_downloads)),
    vjust = 3.2,
    hjust = -0.05,
    color = "black"
  )

# Scales and labels ------------------------------------------------------------

p_daily_histogram <- p_daily_histogram +
  scale_x_continuous(labels = comma)

p_daily_histogram <- p_daily_histogram +
  scale_y_continuous(labels = comma)

p_daily_histogram <- p_daily_histogram +
  labs(
    title = "(c) Distribution",
    subtitle = "Adjusted downloads",
    x = "Adjusted daily downloads",
    y = "Number of days"
  )

p_daily_histogram <- p_daily_histogram +
  theme_tscv()


# Figure 2: Monthly downloads ##################################################

# Base plot --------------------------------------------------------------------

p_monthly <- ggplot(monthly_downloads, aes(x = month, y = downloads))

# Bars and labels --------------------------------------------------------------

p_monthly <- p_monthly +
  geom_col(fill = col_light)

p_monthly <- p_monthly +
  geom_text(
    aes(label = label),
    vjust = -0.3,
    size = 3.2,
    lineheight = 0.9
  )

# Scales and labels ------------------------------------------------------------

p_monthly <- p_monthly +
  scale_y_continuous(
    labels = comma,
    expand = expansion(mult = c(0, 0.18))
  )

p_monthly <- p_monthly +
  labs(
    title = "Monthly downloads",
    subtitle = "Adjusted totals and change",
    x = NULL,
    y = NULL
  )

p_monthly <- p_monthly +
  theme_tscv()


# Figure 3: Release impact #####################################################

# Base plot --------------------------------------------------------------------

p_release_impact <- ggplot(release_impact, aes(x = version))

# Bars -------------------------------------------------------------------------

p_release_impact <- p_release_impact +
  geom_col(
    aes(y = downloads_before),
    fill = col_light,
    width = 0.45,
    position = position_nudge(x = -0.23)
  )

p_release_impact <- p_release_impact +
  geom_col(
    aes(y = downloads_after),
    fill = col_main,
    width = 0.45,
    position = position_nudge(x = 0.23)
  )

# Labels -----------------------------------------------------------------------

p_release_impact <- p_release_impact +
  geom_text(
    aes(
      y = downloads_before,
      label = before_label
    ),
    position = position_nudge(x = -0.23),
    vjust = -0.3,
    size = 3.1
  )

p_release_impact <- p_release_impact +
  geom_text(
    aes(
      y = downloads_after,
      label = after_label
    ),
    position = position_nudge(x = 0.23),
    vjust = -0.3,
    size = 3.1,
    lineheight = 0.9
  )

# Scales and labels ------------------------------------------------------------

p_release_impact <- p_release_impact +
  scale_y_continuous(
    labels = comma,
    expand = expansion(mult = c(0, 0.22))
  )

p_release_impact <- p_release_impact +
  labs(
    title = "Release impact",
    subtitle = paste0("Adjusted downloads, ±", release_window_days, " days"),
    x = NULL,
    y = NULL
  )

p_release_impact <- p_release_impact +
  theme_tscv()


# Combined figures #############################################################

# Combine core plots ===========================================================

core_download_plots <- p_daily /
  (p_cumulative | p_daily_histogram) +
  plot_layout(heights = c(1, 0.9))

print(core_download_plots)


# Print separate analysis plots ================================================

print(p_monthly)
print(p_release_impact)


# Save figures =================================================================

fig_width <- 17
fig_hight <- 12

ggsave(
  filename = "echos/figure_01_echos_daily_downloads.pdf",
  plot = core_download_plots,
  width = fig_width,
  height = fig_hight
)

ggsave(
  filename = "echos/figure_02_echos_monthly_downloads.pdf",
  plot = p_monthly,
  width = fig_width,
  height = floor(fig_hight/2)
)

ggsave(
  filename = "echos/figure_03_echos_release_impact.pdf",
  plot = p_release_impact,
  width = floor(fig_width/2),
  height = floor(fig_hight/2)
)
