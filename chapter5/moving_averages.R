# moving_averages.R
# Moving average feature construction and three diagnostic plots:
# build_ma_data() - computes the four MA columns
# plot_signal_extraction() - density overlay (raw vs 3d vs 7d)
# plot_variance_reduction()- SD bar chart by window

build_ma_data <- function(dat = df_final) {
  dat %>%
    group_by(location) %>%
    arrange(date) %>%
    mutate(
      rainfall_ma3 = zoo::rollmean(rainfall, k = 3, fill = NA, align = "right"),
      rainfall_ma7 = zoo::rollmean(rainfall, k = 7, fill = NA, align = "right"),
      humidity_ma3 = zoo::rollmean(
        humidity3pm,
        k = 3,
        fill = NA,
        align = "right"
      ),
      humidity_ma7 = zoo::rollmean(
        humidity3pm,
        k = 7,
        fill = NA,
        align = "right"
      )
    ) %>%
    ungroup()
}

plot_signal_extraction <- function(ma_dat) {
  p1_data <- ma_dat %>%
    filter(rainfall > 0) %>%
    select(rainfall, rainfall_ma3, rainfall_ma7) %>%
    pivot_longer(everything(), names_to = "metric", values_to = "value") %>%
    filter(!is.na(value)) %>%
    mutate(
      metric = factor(
        metric,
        levels = c("rainfall", "rainfall_ma3", "rainfall_ma7"),
        labels = c("Raw daily", "3-day MA", "7-day MA")
      )
    )

  ggplot(p1_data, aes(x = value, fill = metric, colour = metric)) +
    geom_density(alpha = 0.38, linewidth = 0.8) +
    annotation_logticks(sides = "b", colour = "grey55", linewidth = 0.3) +
    scale_x_log10(
      breaks = c(0.1, 0.5, 1, 5, 10, 50, 100),
      labels = label_number(suffix = " mm"),
      expand = expansion(mult = c(0.02, 0.02))
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    scale_fill_manual(
      values = c(
        "Raw daily" = "#C0392B",
        "3-day MA" = "#E67E22",
        "7-day MA" = "#1A7A4A"
      )
    ) +
    scale_colour_manual(
      values = c(
        "Raw daily" = "#C0392B",
        "3-day MA" = "#E67E22",
        "7-day MA" = "#1A7A4A"
      )
    ) +
    labs(
      title = "Moving Averages Filter High-Frequency Noise",
      subtitle = "Raw rainfall (red) is severely right-skewed. The 7-day average (green) concentrates\nprobability mass near the regime centre.",
      x = "Rainfall intensity (log scale)",
      y = "Density",
      caption = "Restricted to non-zero observations. Log scale on x-axis.",
      fill = NULL,
      colour = NULL
    ) +
    feat_theme +
    theme(legend.position = "top", legend.key.width = unit(0.8, "cm"))
}

plot_variance_reduction <- function(ma_dat) {
  variance_data <- ma_dat %>%
    filter(rainfall > 0) %>%
    summarise(
      Raw = sd(rainfall, na.rm = TRUE),
      `3-day MA` = sd(rainfall_ma3, na.rm = TRUE),
      `7-day MA` = sd(rainfall_ma7, na.rm = TRUE)
    ) %>%
    pivot_longer(everything(), names_to = "metric", values_to = "sd") %>%
    mutate(
      metric = factor(metric, levels = c("Raw", "3-day MA", "7-day MA")),
      variance_reduction = (sd[metric == "Raw"] - sd) /
        sd[metric == "Raw"] *
        100
    )

  ggplot(variance_data, aes(metric, sd, fill = metric)) +
    geom_col(alpha = 0.85, width = 0.58) +
    geom_text(
      aes(
        label = sprintf("%.1f mm\n(%.0f%% reduction)", sd, variance_reduction)
      ),
      vjust = -0.35,
      fontface = "bold",
      size = 3.6,
      colour = "grey15"
    ) +
    scale_fill_manual(
      values = c(
        "Raw" = "#C0392B",
        "3-day MA" = "#E67E22",
        "7-day MA" = "#1A7A4A"
      )
    ) +
    scale_y_continuous(
      expand = expansion(mult = c(0, 0.18)),
      breaks = pretty_breaks(n = 5)
    ) +
    labs(
      title = "Variance Reduction by Moving Average Window",
      subtitle = "Standard deviation falls as the averaging window widens.",
      y = "Standard deviation (mm)",
      x = NULL
    ) +
    feat_theme +
    theme(legend.position = "none", panel.grid.major.x = element_blank())
}

plot_ma_ridgeline <- function(ma_dat) {
  ridge_data <- ma_dat %>%
    filter(rainfall > 0) %>%
    select(rainfall, rainfall_ma3, rainfall_ma7) %>%
    pivot_longer(everything(), names_to = "metric", values_to = "value") %>%
    filter(!is.na(value)) %>%
    mutate(
      metric = factor(
        metric,
        levels = c("rainfall_ma7", "rainfall_ma3", "rainfall"),
        labels = c("7-day MA", "3-day MA", "Raw daily")
      )
    )

  ggplot(ridge_data, aes(value, metric, fill = metric)) +
    geom_density_ridges(
      alpha = 0.72,
      scale = 1.5,
      quantile_lines = TRUE,
      quantiles = 2,
      colour = "grey30",
      linewidth = 0.3
    ) +
    scale_x_log10(
      breaks = c(1, 5, 10, 25, 50, 100),
      labels = c("1", "5", "10", "25", "50", "100"),
      expand = expansion(mult = c(0.02, 0.04))
    ) +
    scale_fill_manual(
      values = c(
        "Raw daily" = "#C0392B",
        "3-day MA" = "#E67E22",
        "7-day MA" = "#1A7A4A"
      )
    ) +
    labs(
      title = "Distribution Tightening with Moving Averages",
      subtitle = "Peaks become sharper and tails compress as window size increases. Median lines are stable.",
      x = "Rainfall (mm, log scale)",
      y = NULL
    ) +
    feat_theme +
    theme(legend.position = "none", panel.grid.major.y = element_blank())
}
