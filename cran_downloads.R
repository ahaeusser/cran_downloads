
# Download statistics for an R package ########################################

# Setup =======================================================================

# Load libraries --------------------------------------------------------------

library(adjustedcranlogs)
library(dplyr)
library(lubridate)
library(ggplot2)
library(scales)
library(zoo)
library(patchwork)
library(tscv)
library(ggpattern)


# Configuration ---------------------------------------------------------------

invisible(Sys.setlocale("LC_TIME", "C"))

rolling_window <- 7
release_window_days <- 30
histogram_bins <- 30

col_main <- "steelblue"
col_light <- "lightsteelblue"

figure_width <- 17
figure_height <- 17


# Package specifics -----------------------------------------------------------

package_name <- "echos"

start_date <- as.Date("2025-02-01")
end_date <- Sys.Date()

release_dates <- tibble::tribble(
  ~version, ~release_date,
  "v1.0.1", as.Date("2025-02-11"),
  "v1.0.2", as.Date("2025-06-23"),
  "v1.0.3", as.Date("2026-02-22"),
  "v1.0.4", as.Date("2026-06-15")
)


# # Package specifics -----------------------------------------------------------
# 
# package_name <- "echos"
# package_name <- "tscv"
# 
# if (package_name == "echos") {
#   
#   start_date <- as.Date("2025-02-01")
#   
#   release_dates <- tibble::tribble(
#     ~version, ~release_date,
#     "v1.0.1", as.Date("2025-02-11"),
#     "v1.0.2", as.Date("2025-06-23"),
#     "v1.0.3", as.Date("2026-02-22"),
#     "v1.0.4", as.Date("2026-06-15")
#   )
#   
# } else if (package_name == "tscv") {
#   
#   start_date <- as.Date("2026-05-13")
#   
#   release_dates <- tibble::tribble(
#     ~version, ~release_date,
#     "v1.0.0", as.Date("2026-05-13")
#   )
#   
# } else {
#   
#   stop("Unknown package: ", package_name)
# }
# 
# end_date <- Sys.Date()


# Data preparation ============================================================

# Download data ---------------------------------------------------------------

downloads <- adj_cran_downloads(
  packages = package_name,
  from = start_date,
  to = end_date
)

downloads <- downloads |>
  transmute(
    date = as.Date(date),
    downloads = pmax(adjusted_downloads, 0)) |>
  arrange(date)


# Calculate daily statistics --------------------------------------------------

downloads <- downloads |>
  mutate(
    month = floor_date(date, unit = "month"),
    rolling_downloads = rollmean(
      downloads,
      k = rolling_window,
      fill = NA_real_,
      align = "right"),
    cumulative_downloads = cumsum(downloads)
  )

data_start_date <- min(downloads$date, na.rm = TRUE)
data_end_date <- max(downloads$date, na.rm = TRUE)

mean_daily_downloads <- mean(downloads$downloads, na.rm = TRUE)
median_daily_downloads <- median(downloads$downloads, na.rm = TRUE)

last_cumulative_observation <- downloads |>
  slice_tail(n = 1)

daily_label_y <- max(downloads$downloads, na.rm = TRUE) * 0.95


# Calculate monthly statistics ------------------------------------------------

last_observed_month <- floor_date(data_end_date, unit = "month")

last_day_observed_month <- ceiling_date(
  last_observed_month,
  unit = "month"
) - days(1)

last_complete_month <- if (data_end_date >= last_day_observed_month) {
  last_observed_month
} else {
  last_observed_month - months(1)
}

monthly_downloads <- downloads |>
  filter(month <= last_complete_month) |>
  group_by(month) |>
  summarise(
    downloads = sum(downloads, na.rm = TRUE),
    .groups = "drop") |>
  arrange(month)

monthly_downloads <- monthly_downloads |>
  mutate(
    downloads_lag = lag(downloads),
    change_pct = downloads / downloads_lag - 1
  )

monthly_downloads <- monthly_downloads |>
  mutate(
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


# Prepare release dates -------------------------------------------------------

release_points <- release_dates |>
  filter(
    release_date >= data_start_date,
    release_date <= data_end_date) |>
  mutate(
    release_label = paste0(
      version,
      "\n",
      format(release_date, "%Y-%m-%d")
    )
  )


# Calculate release impact ----------------------------------------------------

release_impact <- release_dates |>
  filter(
    release_date - days(release_window_days) >= data_start_date,
    release_date + days(release_window_days) <= data_end_date
  )

release_impact <- release_impact |>
  rowwise() |>
  mutate(
    downloads_before = sum(
      downloads$downloads[
        downloads$date >= release_date - days(release_window_days) &
          downloads$date < release_date],
      na.rm = TRUE),
    downloads_after = sum(
      downloads$downloads[
        downloads$date > release_date &
          downloads$date <= release_date + days(release_window_days)],
      na.rm = TRUE)) |>
  ungroup()

release_impact <- release_impact |>
  mutate(
    change_pct = if_else(
      downloads_before > 0,
      downloads_after / downloads_before - 1,
      NA_real_),
    before_label = comma(downloads_before),
    after_label = comma(downloads_after)
  )

release_impact <- release_impact |>
  mutate(
    after_label = if_else(
      is.na(change_pct),
      after_label,
      paste0(
        after_label,
        "\n",
        if_else(change_pct > 0, "+", ""),
        percent(change_pct, accuracy = 1)
      )
    ),
    version = factor(
      version,
      levels = release_dates$version
    )
  )


# Visualizations ==============================================================

# Daily downloads -------------------------------------------------------------

p_daily <- ggplot(
  data = downloads,
  aes(x = date)
)

p_daily <- p_daily +
  geom_line(
    aes(y = downloads),
    colour = col_light,
    linewidth = 0.8
  )

p_daily <- p_daily +
  geom_line(
    aes(y = rolling_downloads),
    colour = col_main,
    linewidth = 1.1
  )

p_daily <- p_daily +
  geom_vline(
    data = release_points,
    aes(xintercept = release_date),
    colour = "grey40",
    linetype = "dotted",
    linewidth = 0.6
  )

p_daily <- p_daily +
  geom_label(
    data = release_points,
    aes(
      x = release_date,
      y = daily_label_y,
      label = release_label),
    size = 3.1,
    vjust = 1,
    label.padding = grid::unit(0.15, "lines")
  )

p_daily <- p_daily +
  scale_y_continuous(
    labels = comma,
    expand = expansion(mult = c(0.02, 0.12))
  )

p_daily <- p_daily +
  labs(
    title = "(a) Daily downloads",
    subtitle = paste0(
      "Adjusted daily downloads and ",
      rolling_window,
      "-day average"),
    x = NULL,
    y = NULL
  )

p_daily <- p_daily +
  theme_tscv()


# Cumulative downloads --------------------------------------------------------

p_cumulative <- ggplot(
  data = downloads,
  aes(
    x = date,
    y = cumulative_downloads
  )
)

p_cumulative <- p_cumulative +
  geom_area_pattern(
    pattern = "gradient",
    fill = "#00000000",
    colour = NA,
    pattern_fill = "#00000000",
    pattern_fill2 = col_light
  )

p_cumulative <- p_cumulative +
  geom_line(
    colour = col_main,
    linewidth = 1
  )

p_cumulative <- p_cumulative +
  geom_point(
    data = last_cumulative_observation,
    colour = col_main,
    size = 4
  )

p_cumulative <- p_cumulative +
  geom_point(
    data = last_cumulative_observation,
    colour = "white",
    size = 2
  )

p_cumulative <- p_cumulative +
  geom_label(
    data = last_cumulative_observation,
    aes(
      label = paste0(
        "Total: ",
        comma(cumulative_downloads)
      )
    ),
    hjust = 1.1,
    vjust = -0.6,
    label.padding = grid::unit(0.2, "lines")
  )

p_cumulative <- p_cumulative +
  scale_x_date(
    expand = expansion(mult = c(0.02, 0.05))
  )

p_cumulative <- p_cumulative +
  scale_y_continuous(
    labels = comma,
    expand = expansion(mult = c(0.02, 0.15))
  )

p_cumulative <- p_cumulative +
  labs(
    title = "(b) Cumulative downloads",
    subtitle = "Adjusted cumulative downloads",
    x = NULL,
    y = NULL
  )

p_cumulative <- p_cumulative +
  theme_tscv()


# Distribution of daily downloads --------------------------------------------

p_daily_histogram <- ggplot(
  data = downloads,
  aes(x = downloads)
)

p_daily_histogram <- p_daily_histogram +
  geom_histogram(
    bins = histogram_bins,
    fill = col_light,
    colour = "white"
  )

p_daily_histogram <- p_daily_histogram +
  geom_vline(
    xintercept = mean_daily_downloads,
    colour = col_main,
    linewidth = 1,
    linetype = "dashed"
  )

p_daily_histogram <- p_daily_histogram +
  geom_vline(
    xintercept = median_daily_downloads,
    colour = "black",
    linewidth = 1,
    linetype = "dotted"
  )

p_daily_histogram <- p_daily_histogram +
  annotate(
    geom = "label",
    x = mean_daily_downloads,
    y = Inf,
    label = paste0(
      "Mean: ",
      number(mean_daily_downloads, accuracy = 0.1)
    ),
    colour = col_main,
    vjust = 1.5,
    hjust = -0.05
  )

p_daily_histogram <- p_daily_histogram +
  annotate(
    geom = "label",
    x = median_daily_downloads,
    y = Inf,
    label = paste0(
      "Median: ",
      comma(median_daily_downloads)
    ),
    colour = "black",
    vjust = 3.2,
    hjust = -0.05
  )

p_daily_histogram <- p_daily_histogram +
  scale_x_continuous(labels = comma)

p_daily_histogram <- p_daily_histogram +
  scale_y_continuous(labels = comma)

p_daily_histogram <- p_daily_histogram +
  labs(
    title = "(c) Distribution",
    subtitle = "Adjusted daily downloads",
    x = "Adjusted daily downloads",
    y = "Number of days"
  )

p_daily_histogram <- p_daily_histogram +
  theme_tscv()


# Monthly downloads -----------------------------------------------------------

p_monthly <- ggplot(
  data = monthly_downloads,
  aes(
    x = month,
    y = downloads
  )
)

p_monthly <- p_monthly +
  geom_col(
    fill = col_light
  )

p_monthly <- p_monthly +
  geom_text(
    aes(label = label),
    vjust = -0.3,
    size = 3.2,
    lineheight = 0.9
  )

p_monthly <- p_monthly +
  scale_y_continuous(
    labels = comma,
    expand = expansion(mult = c(0, 0.18))
  )

p_monthly <- p_monthly +
  labs(
    title = "(d) Monthly downloads",
    subtitle = "Monthly totals and month-over-month change",
    x = NULL,
    y = NULL
  )

p_monthly <- p_monthly +
  theme_tscv()


# Release impact --------------------------------------------------------------

p_release_impact <- ggplot(
  data = release_impact,
  aes(x = version)
)

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

p_release_impact <- p_release_impact +
  scale_y_continuous(
    labels = comma,
    expand = expansion(mult = c(0, 0.22))
  )

p_release_impact <- p_release_impact +
  labs(
    title = "(e) Release impact",
    subtitle = paste0(
      "Adjusted downloads, ±",
      release_window_days,
      " days"
    ),
    x = NULL,
    y = NULL
  )

p_release_impact <- p_release_impact +
  theme_tscv()


# Combine plots ===============================================================

p_downloads <- p_daily /
  (p_cumulative | p_daily_histogram) /
  (p_monthly | p_release_impact)

p_downloads <- p_downloads +
  plot_annotation(
    title = paste0(
      "CRAN Downloads for {",
      package_name,
      "} (",
      format(start_date, "%b %Y"),
      "–",
      format(end_date, "%b %Y"),
      ")"
    ),
    theme = theme(
      plot.title = element_text(
        size = 20,
        face = "bold",
        hjust = 0
      )
    )
  )

p_downloads


# Save figure =================================================================

output_file <- paste0(
  "figure_downloads_",
  package_name,
  ".png"
)

ggsave(
  filename = output_file,
  plot = p_downloads,
  width = figure_width,
  height = figure_height,
  units = "in",
  dpi = 300,
  bg = "white"
)
