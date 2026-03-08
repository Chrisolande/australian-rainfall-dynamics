# eda_seasonality.R
# Seasonal rainfall intensity analysis:
# build_seasonal_data() - shared data prep (called once, reused)
# plot_monthly_ridgeline() - log-rainfall ridgeline by month
# plot_seasonal_facet() - density ridges faceted by season
# plot_mean_rain_bar() - monthly mean intensity bar chart
# render_seasonal_desc_stats() - descriptive stats table by season
# run_seasonal_tests() - Kruskal-Wallis + Dunn's + letters
# render_kruskal_table() - KW summary kable
# render_dunns_table() - Dunn pairwise kable
# plot_seasonal_stat_groups() - bar chart with compact letter display

build_seasonal_data <- function(dat = df_final) {
  dat %>%
    filter(rainfall > 0) %>%
    mutate(
      season = case_when(
        month %in% c(12, 1, 2) ~ "Summer",
        month %in% c(3, 4, 5) ~ "Autumn",
        month %in% c(6, 7, 8) ~ "Winter",
        month %in% c(9, 10, 11) ~ "Spring"
      ),
      season = factor(
        season,
        levels = c("Summer", "Autumn", "Winter", "Spring")
      )
    )
}

build_monthly_stats <- function(dat = df_final) {
  dat %>%
    filter(rainfall > 0) %>%
    group_by(month) %>%
    summarise(
      median_rain = median(rainfall),
      mean_rain = mean(rainfall),
      rain_days = n(),
      .groups = "drop"
    ) %>%
    mutate(month_label = factor(month.abb[month], levels = rev(month.abb)))
}

plot_monthly_ridgeline <- function(dat = df_final) {
  monthly_stats <- build_monthly_stats(dat)

  plot_data <- dat %>%
    filter(rainfall > 0) %>%
    mutate(
      month_label = factor(month.abb[month], levels = rev(month.abb)),
      log_rain = log(rainfall)
    ) %>%
    left_join(monthly_stats, by = c("month", "month_label"))

  global_med <- median(log(dat$rainfall[dat$rainfall > 0]))

  ggplot(plot_data, aes(log_rain, month_label, fill = after_stat(x))) +
    geom_density_ridges_gradient(
      scale = 2.5,
      rel_min_height = 0.01,
      quantile_lines = TRUE,
      quantiles = 2,
      colour = "grey30",
      linewidth = 0.3
    ) +
    geom_vline(
      xintercept = global_med,
      linetype = "dashed",
      colour = "grey40",
      linewidth = 0.5
    ) +
    scale_fill_viridis_c(
      option = "mako",
      name = "Log\nrainfall",
      direction = -1,
      begin = 0.1,
      end = 0.9
    ) +
    scale_x_continuous(
      breaks = pretty_breaks(),
      expand = expansion(mult = c(0.02, 0.02))
    ) +
    labs(
      title = "Monthly Rainfall Distribution Patterns",
      subtitle = "Solid lines mark monthly medians; dashed line is the global median.",
      x = "Rainfall amount (mm, log scale)",
      y = NULL
    ) +
    eda_theme +
    theme(
      legend.position = "right",
      legend.key.height = unit(1.2, "cm"),
      legend.key.width = unit(0.4, "cm")
    )
}

plot_seasonal_facet <- function(seasonal_dat) {
  seasonal_dat %>%
    mutate(month_label = factor(month.abb[month], levels = month.abb)) %>%
    ggplot(aes(rainfall, month_label, fill = season)) +
    geom_density_ridges(
      scale = 1.5,
      alpha = 0.72,
      quantile_lines = TRUE,
      quantiles = c(0.25, 0.5, 0.75),
      colour = "grey30",
      linewidth = 0.3
    ) +
    scale_x_log10(
      breaks = c(1, 10, 50, 100, 300),
      labels = label_number(accuracy = 1),
      expand = expansion(mult = c(0.02, 0.04))
    ) +
    scale_fill_manual(
      values = c(
        "Summer" = "#E69F00",
        "Autumn" = "#D55E00",
        "Winter" = "#2471A3",
        "Spring" = "#27AE60"
      )
    ) +
    facet_wrap(~season, scales = "free_y", ncol = 2) +
    labs(
      title = "Seasonal Rainfall Patterns",
      subtitle = "Quartile lines show 25th, 50th, and 75th percentiles within each month.",
      x = "Rainfall amount (mm, log scale)",
      y = NULL
    ) +
    eda_theme +
    theme(legend.position = "none")
}

plot_mean_rain_bar <- function(dat = df_final) {
  build_monthly_stats(dat) %>%
    mutate(month_label = factor(month.abb[month], levels = month.abb)) %>%
    ggplot(aes(month_label, mean_rain)) +
    geom_col(aes(fill = mean_rain), width = 0.72, alpha = 0.88) +
    geom_text(
      aes(label = round(mean_rain, 1)),
      vjust = -0.45,
      size = 3.2,
      fontface = "bold",
      colour = "grey15"
    ) +
    scale_fill_viridis_c(
      option = "mako",
      direction = -1,
      begin = 0.15,
      end = 0.85
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
    labs(
      title = "Mean Rainfall by Month (Non-Zero Days)",
      subtitle = "Seasonal variation in intensity is distinct from seasonal variation in frequency.",
      y = "Mean rainfall (mm)",
      x = NULL
    ) +
    eda_theme +
    theme(
      legend.position = "none",
      panel.grid.major.x = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1, face = "bold")
    )
}

render_seasonal_desc_stats <- function(seasonal_dat) {
  seasonal_dat %>%
    select(season, rainfall) %>%
    group_by(season) %>%
    get_summary_stats(rainfall, type = "mean_sd") %>%
    kable(
      caption = "Descriptive Statistics of Rainfall Intensity by Season",
      col.names = c("Season", "Variable", "N (Events)", "Mean (mm)", "SD (mm)"),
      booktabs = TRUE
    ) %>%
    kable_styling(
      bootstrap_options = c("striped", "hover"),
      full_width = FALSE,
      latex_options = c("hold_position")
    )
}

run_seasonal_tests <- function(seasonal_dat) {
  kw_result <- kruskal_test(rainfall ~ season, data = seasonal_dat)
  epsilon_sq <- kruskal_effsize(rainfall ~ season, data = seasonal_dat)
  dunn_result <- dunn_test(
    rainfall ~ season,
    data = seasonal_dat,
    p.adjust.method = "bonferroni"
  )
  list(
    kw_result = kw_result,
    epsilon_sq = epsilon_sq,
    dunn_result = dunn_result
  )
}

render_kruskal_table <- function(seasonal_tests) {
  kw_result <- seasonal_tests$kw_result
  epsilon_sq <- seasonal_tests$epsilon_sq

  tibble(
    Test = "Kruskal-Wallis Rank Sum Test",
    `Chi-squared` = round(kw_result$statistic, 2),
    df = kw_result$df,
    `P-value` = pvalue(kw_result$p, accuracy = 0.001),
    `Effect Size` = round(epsilon_sq$effsize, 4),
    Magnitude = as.character(epsilon_sq$magnitude)
  ) %>%
    kable(
      caption = "Statistical Significance of Seasonal Differences (Non-Parametric)",
      align = "lccccr",
      booktabs = TRUE
    ) %>%
    kable_styling(
      bootstrap_options = c("striped", "hover", "condensed"),
      full_width = FALSE,
      position = "left",
      latex_options = c("hold_position")
    ) %>%
    add_footnote(
      c("Effect size: Epsilon-squared.", "Alpha = 0.05"),
      notation = "symbol"
    )
}

render_dunns_table <- function(seasonal_tests) {
  seasonal_tests$dunn_result %>%
    select(group1, group2, statistic, p.adj, p.adj.signif) %>%
    kable(
      caption = "Dunn's Pairwise Comparison Test (Bonferroni Corrected)",
      col.names = c(
        "Group 1",
        "Group 2",
        "Z-Statistic",
        "Adj. P-Value",
        "Significance"
      ),
      booktabs = TRUE,
      digits = 3
    ) %>%
    kable_styling(
      bootstrap_options = c("striped", "hover"),
      full_width = FALSE,
      latex_options = c("hold_position")
    )
}

plot_seasonal_stat_groups <- function(seasonal_dat, seasonal_tests) {
  dunn_result <- seasonal_tests$dunn_result
  epsilon_sq <- seasonal_tests$epsilon_sq

  p_vals <- dunn_result$p.adj
  names(p_vals) <- paste(dunn_result$group1, dunn_result$group2, sep = "-")
  letters_vec <- multcompLetters(p_vals)$Letters
  letters_df <- data.frame(season = names(letters_vec), Letter = letters_vec)

  seasonal_summary <- seasonal_dat %>%
    group_by(season) %>%
    summarise(
      mean_rain = mean(rainfall, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    ) %>%
    left_join(letters_df, by = "season")

  ggplot(seasonal_summary, aes(x = season, y = mean_rain, fill = season)) +
    geom_col(alpha = 0.85, width = 0.68) +
    geom_text(
      aes(label = Letter),
      vjust = -0.5,
      size = 7,
      fontface = "bold",
      colour = "grey20"
    ) +
    geom_text(
      aes(label = round(mean_rain, 1)),
      vjust = 1.6,
      colour = "white",
      fontface = "bold",
      size = 4.5
    ) +
    scale_fill_manual(
      values = c(
        "Summer" = "#E69F00",
        "Autumn" = "#D55E00",
        "Winter" = "#2471A3",
        "Spring" = "#27AE60"
      )
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(
      title = "Seasonal Rainfall with Statistical Groupings",
      subtitle = sprintf(
        "Kruskal-Wallis: p < 0.001, effect size: %s | Seasons sharing a letter are not significantly different.",
        as.character(epsilon_sq$magnitude)
      ),
      y = "Mean rainfall (mm)",
      x = NULL
    ) +
    eda_theme +
    theme(legend.position = "none", panel.grid.major.x = element_blank())
}
