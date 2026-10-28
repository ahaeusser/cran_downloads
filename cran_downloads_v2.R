
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

# Use R's native gradients, avoiding the optional magick dependency.
options(ggpattern_use_R4.1_gradients = TRUE)


# Configuration ---------------------------------------------------------------

invisible(Sys.setlocale("LC_TIME", "C"))

rolling_window <- 7
release_window_days <- 30
histogram_bins <- 30
base_font_size <- 18
axis_font_size <- 16
annotation_font_size <- 5

theme_downloads <- function() {
  theme_tscv(base_size = base_font_size) +
    theme(
      axis.text = element_text(size = axis_font_size, colour = "grey25"),
      axis.title = element_text(size = base_font_size),
      plot.subtitle = element_text(size = base_font_size - 1, lineheight = 1.15)
    )
}

col_main <- "steelblue"
col_light <- "lightsteelblue"

figure_width <- 17
figure_height <- 17

# Package specifics -----------------------------------------------------------

package_name <- "echos"
package_name <- "tscv"

if (package_name == "echos") {

  start_date <- as.Date("2025-02-01")

  release_dates <- tibble::tribble(
    ~version, ~release_date,
    "v1.0.1", as.Date("2025-02-11"),
    "v1.0.2", as.Date("2025-06-23"),
    "v1.0.3", as.Date("2026-02-22"),
    "v1.0.4", as.Date("2026-06-15"),
    "v1.0.5", as.Date("2026-10-22")
  )

} else if (package_name == "tscv") {

  start_date <- as.Date("2026-05-01")

  release_dates <- tibble::tribble(
    ~version, ~release_date,
    "v1.0.0", as.Date("2026-05-13"),
    "v1.0.1", as.Date("2026-09-02"),
    "v1.0.2", as.Date("2026-10-21")
  )

} else {

  stop("Unknown package: ", package_name)
}

end_date <- Sys.Date()


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
min_daily_downloads <- min(downloads$downloads, na.rm = TRUE)
max_daily_downloads <- max(downloads$downloads, na.rm = TRUE)
daily_quartiles <- quantile(downloads$downloads, c(0.25, 0.75), na.rm = TRUE)
daily_iqr <- IQR(downloads$downloads, na.rm = TRUE)
n_days <- n_distinct(downloads$date[is.finite(downloads$downloads)])

last_cumulative_observation <- downloads |>
  slice_tail(n = 1)

# Keep the axis close to the observed total, with room for the final label.
cumulative_axis_max <- max(
  1, max(downloads$cumulative_downloads, na.rm = TRUE)
) * 1.10

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

# Shape-preserving cubic interpolation passes through actual monthly totals
# without adding artificial peaks or troughs between observations.
monthly_curve <- monthly_downloads
monthly_labels <- monthly_downloads |>
  mutate(label = character(n()), label_hjust = numeric(n()))

if (nrow(monthly_downloads) >= 2L) {
  month_numbers <- as.numeric(monthly_downloads$month)
  month_gaps <- diff(month_numbers)
  secant_slopes <- diff(monthly_downloads$downloads) / month_gaps
  # Weighted harmonic tangents preserve each interval's direction. Zero
  # tangents at turning points prevent the curve overshooting actual extrema.
  tangents <- c(secant_slopes[1], rep(0, nrow(monthly_downloads) - 2L),
                tail(secant_slopes, 1))
  if (nrow(monthly_downloads) > 2L) {
    for (i in 2L:(nrow(monthly_downloads) - 1L)) {
      if (secant_slopes[i - 1L] * secant_slopes[i] > 0) {
        weight_left <- 2 * month_gaps[i] + month_gaps[i - 1L]
        weight_right <- month_gaps[i] + 2 * month_gaps[i - 1L]
        tangents[i] <- (weight_left + weight_right) /
          (weight_left / secant_slopes[i - 1L] + weight_right / secant_slopes[i])
      }
    }
  }
  monthly_interpolator <- splinefunH(
    x = month_numbers,
    y = monthly_downloads$downloads,
    m = tangents
  )
  curve_dates <- seq(
    min(monthly_downloads$month),
    max(monthly_downloads$month),
    by = "day"
  )
  monthly_curve <- tibble::tibble(
    month = curve_dates,
    downloads = monthly_interpolator(as.numeric(curve_dates))
  )
}

if (nrow(monthly_downloads) > 0L) {
  # Use the first occurrence of tied extrema and combine roles at each month.
  monthly_label_rows <- c(
    First = 1L,
    Last = nrow(monthly_downloads),
    Min = which.min(monthly_downloads$downloads),
    Max = which.max(monthly_downloads$downloads)
  )
  selected_rows <- sort(unique(unname(monthly_label_rows)))
  monthly_labels <- monthly_downloads[selected_rows, ]
  monthly_labels$label <- vapply(selected_rows, function(row) {
    roles <- names(monthly_label_rows)[monthly_label_rows == row]
    paste0(
      paste(roles, collapse = " / "), ": ",
      comma(monthly_downloads$downloads[row]),
      "\n", format(monthly_downloads$month[row], "%Y-%m")
    )
  }, character(1))
  monthly_labels$label_hjust <- ifelse(
    selected_rows == 1L, 0,
    ifelse(selected_rows == nrow(monthly_downloads), 1, 0.5)
  )
}


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
    linewidth = 1.1,
    # The first rolling_window - 1 days do not yet have a full rolling average.
    na.rm = TRUE
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
    size = annotation_font_size,
    vjust = 1,
    label.padding = grid::unit(0.15, "lines")
  )

p_daily <- p_daily +
  scale_x_date(date_labels = "%Y-%m")

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
  theme_downloads()


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
    stat = "identity",
    position = "identity",
    pattern = "gradient",
    fill = "#00000000",
    colour = NA,
    pattern_fill = scales::alpha(col_light, 0),
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
    size = annotation_font_size,
    hjust = 1.1,
    vjust = -0.6,
    label.padding = grid::unit(0.2, "lines")
  )

p_cumulative <- p_cumulative +
  scale_x_date(
    date_labels = "%Y-%m",
    expand = expansion(mult = c(0.02, 0.05))
  )

p_cumulative <- p_cumulative +
  scale_y_continuous(
    labels = comma,
    limits = c(0, cumulative_axis_max),
    breaks = breaks_pretty(n = 5),
    expand = expansion(mult = c(0, 0))
  )

p_cumulative <- p_cumulative +
  labs(
    title = "(b) Cumulative downloads",
    subtitle = "Adjusted cumulative downloads",
    x = NULL,
    y = NULL
  )

p_cumulative <- p_cumulative +
  theme_downloads()


# Distribution of daily downloads --------------------------------------------

p_daily_histogram <- ggplot(
  data = downloads,
  aes(x = downloads)
) +
  annotate(
    geom = "rect",
    xmin = daily_quartiles[1],
    xmax = daily_quartiles[2],
    ymin = -Inf,
    ymax = Inf,
    fill = col_main,
    alpha = 0.08
  ) +
  geom_histogram(
    bins = histogram_bins,
    fill = col_light,
    colour = "white",
    alpha = 0.85
  ) +
  geom_vline(
    xintercept = mean_daily_downloads,
    colour = col_main,
    linewidth = 0.7,
    linetype = "dashed"
  ) +
  geom_vline(
    xintercept = median_daily_downloads,
    colour = "grey35",
    linewidth = 0.7,
    linetype = "dotted"
  ) +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "(c) Distribution",
    subtitle = paste0(
      "Mean: ", number(mean_daily_downloads, accuracy = 0.1), " (dashed)",
      "  |  Median: ", comma(median_daily_downloads), " (dotted)",
      "\nMin: ", comma(min_daily_downloads),
      "  |  Max: ", comma(max_daily_downloads),
      "  |  IQR: ", number(daily_iqr, accuracy = 0.1),
      " (", number(daily_quartiles[1], accuracy = 0.1),
      "\u2013", number(daily_quartiles[2], accuracy = 0.1), ", shaded)"
    ),
    x = "Adjusted daily downloads",
    y = "Number of days"
  ) +
  theme_downloads()


# Monthly downloads -----------------------------------------------------------

p_monthly <- ggplot(
  data = monthly_downloads,
  aes(x = month, y = downloads)
) +
  geom_area_pattern(
    data = monthly_curve,
    stat = "identity",
    position = "identity",
    pattern = "gradient",
    fill = "#00000000",
    colour = NA,
    pattern_fill = scales::alpha(col_light, 0),
    pattern_fill2 = col_light
  ) +
  geom_line(
    data = monthly_curve,
    colour = col_main,
    linewidth = 1.1
  ) +
  geom_point(colour = col_main, size = 2.5) +
  geom_label(
    data = monthly_labels,
    aes(label = label, hjust = label_hjust),
    vjust = -0.6,
    size = annotation_font_size,
    lineheight = 1,
    fill = "white",
    colour = "grey25",
    linewidth = 0,
    label.padding = grid::unit(0.15, "lines")
  ) +
  scale_x_date(
    date_breaks = if (nrow(monthly_downloads) <= 8L) "2 months" else "6 months",
    date_labels = "%Y-%m",
    expand = expansion(mult = c(0.06, 0.06))
  ) +
  scale_y_continuous(
    labels = comma,
    limits = c(0, NA),
    expand = expansion(mult = c(0.02, 0.3))
  ) +
  labs(
    title = "(d) Monthly downloads",
    subtitle = "Monthly totals; complete months",
    x = NULL,
    y = NULL
  ) +
  theme_downloads()


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
    size = annotation_font_size
  )

p_release_impact <- p_release_impact +
  geom_text(
    aes(
      y = downloads_after,
      label = after_label
    ),
    position = position_nudge(x = 0.23),
    vjust = -0.3,
    size = annotation_font_size,
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
      "Adjusted downloads, \u00b1",
      release_window_days,
      " days"
    ),
    x = NULL,
    y = NULL
  )

p_release_impact <- p_release_impact +
  theme_downloads()


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
      start_date,
      "\u2013",
      end_date,
      ") (n = ",
      comma(n_days),
      " days)"
    ),
    theme = theme(
      plot.title = element_text(
        size = 28,
        face = "bold",
        hjust = 0
      )
    )
  )

# Save figure =================================================================

output_file <- paste0(
  "downloads_",
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
  # Cairo renders native gradients consistently in PNG output on Windows.
  device = grDevices::png,
  type = "cairo",
  bg = "white"
)
