# eda_pressure.R
# Atmospheric pressure analysis:
#   plot_pressure_qq() - normality check
#   render_pressure_ttests() - Welch t-tests with Holm correction
#   render_pressure_cohens_d() - effect size table
#   plot_pressure_violin() - violin + boxplot by rainfall state
#   plot_pressure_means() - mean bar chart with diurnal drop

build_pressure_data <- function(dat = df_final) {
  dat %>%
    mutate(pressure_change = pressure3pm - pressure9am) %>%
    select(rain_today, pressure9am, pressure3pm, pressure_change) %>%
    filter(!is.na(pressure9am), !is.na(rain_today))
}

plot_pressure_qq <- function(pressure_dat) {
  pressure_dat %>%
    sample_n(5000) %>%
    pivot_longer(
      cols = c(pressure9am, pressure3pm, pressure_change),
      names_to = "metric",
      values_to = "value"
    ) %>%
    ggplot(aes(sample = value)) +
    stat_qq(alpha = 0.4, size = 0.6, colour = "grey30") +
    stat_qq_line(colour = "#C0392B", linewidth = 0.8) +
    facet_wrap(~metric, scales = "free") +
    labs(
      title = "Q-Q Plots: Normality Check for Pressure Variables",
      subtitle = "Modest tail deviations acceptable at N > 140,000 (CLT applies).",
      x = "Theoretical quantiles",
      y = "Sample quantiles"
    ) +
    eda_theme
}

render_pressure_ttests <- function(pressure_dat) {
  test_data <- pressure_dat %>%
    pivot_longer(
      cols = c(pressure9am, pressure3pm, pressure_change),
      names_to = "metric",
      values_to = "value"
    )

  test_data %>%
    group_by(metric) %>%
    t_test(value ~ rain_today, var.equal = FALSE) %>%
    adjust_pvalue(method = "holm") %>%
    add_significance() %>%
    select(metric, group1, group2, statistic, df, p.adj, p.adj.signif) %>%
    kable(
      caption = "Welch Two-Sample t-test Results (Bonferroni-Holm Corrected)",
      digits = 3,
      col.names = c(
        "Metric",
        "Group 1",
        "Group 2",
        "t-statistic",
        "df",
        "Adj. P-Value",
        "Significance"
      ),
      booktabs = TRUE
    ) %>%
    kable_styling(
      bootstrap_options = c("striped", "hover"),
      full_width = FALSE,
      latex_options = c("hold_position")
    )
}

render_pressure_cohens_d <- function(pressure_dat) {
  test_data <- pressure_dat %>%
    pivot_longer(
      cols = c(pressure9am, pressure3pm, pressure_change),
      names_to = "metric",
      values_to = "value"
    )

  test_data %>%
    group_by(metric) %>%
    cohens_d(value ~ rain_today, var.equal = FALSE) %>%
    mutate(
      magnitude = case_when(
        abs(effsize) < 0.2 ~ "Negligible",
        abs(effsize) < 0.5 ~ "Small",
        abs(effsize) < 0.8 ~ "Medium",
        TRUE ~ "Large"
      )
    ) %>%
    select(metric, effsize, magnitude) %>%
    kable(
      caption = "Cohen's d Effect Size Analysis",
      digits = 3,
      col.names = c("Metric", "Effect Size (d)", "Interpretation"),
      booktabs = TRUE
    ) %>%
    kable_styling(
      bootstrap_options = c("striped", "hover"),
      full_width = FALSE,
      latex_options = c("hold_position")
    )
}

plot_pressure_violin <- function(pressure_dat) {
  test_data <- pressure_dat %>%
    pivot_longer(
      cols = c(pressure9am, pressure3pm, pressure_change),
      names_to = "metric",
      values_to = "value"
    )

  stats_results <- test_data %>%
    group_by(metric) %>%
    t_test(value ~ rain_today, var.equal = FALSE) %>%
    adjust_pvalue(method = "holm") %>%
    add_significance()

  effect_sizes <- test_data %>%
    group_by(metric) %>%
    cohens_d(value ~ rain_today, var.equal = FALSE) %>%
    mutate(
      magnitude = case_when(
        abs(effsize) < 0.2 ~ "Negligible",
        abs(effsize) < 0.5 ~ "Small",
        abs(effsize) < 0.8 ~ "Medium",
        TRUE ~ "Large"
      )
    )

  metric_labels <- c(
    pressure9am = "Pressure 9 AM (hPa)",
    pressure3pm = "Pressure 3 PM (hPa)",
    pressure_change = "Pressure Change (hPa)"
  )

  plot_annotations <- stats_results %>%
    left_join(effect_sizes, by = "metric") %>%
    mutate(
      label_text = sprintf(
        "p %s\nd = %.2f (%s)",
        p.adj.signif,
        effsize,
        magnitude
      ),
      y_pos = case_when(
        metric == "pressure9am" ~ 1046,
        metric == "pressure3pm" ~ 1041,
        metric == "pressure_change" ~ 15
      )
    )

  test_data_labelled <- test_data %>%
    mutate(metric = recode(metric, !!!metric_labels))
  plot_annotations_labelled <- plot_annotations %>%
    mutate(metric = recode(metric, !!!metric_labels))

  ggplot(test_data_labelled, aes(rain_today, value, fill = rain_today)) +
    geom_violin(alpha = 0.55, trim = TRUE, colour = NA) +
    geom_boxplot(
      width = 0.18,
      outlier.shape = NA,
      alpha = 0.85,
      colour = "grey20",
      linewidth = 0.4
    ) +
    facet_wrap(~metric, scales = "free_y") +
    geom_text(
      data = plot_annotations_labelled,
      aes(x = 1.5, y = y_pos, label = label_text),
      inherit.aes = FALSE,
      vjust = 0,
      fontface = "italic",
      size = 3.2,
      colour = "grey25"
    ) +
    scale_fill_manual(values = c("No" = "#AEB6BF", "Yes" = "#2471A3")) +
    scale_x_discrete(labels = c("No" = "Dry", "Yes" = "Rainy")) +
    labs(
      title = "Atmospheric Pressure vs. Rainfall State",
      subtitle = "Welch's t-test with Cohen's d effect sizes; Bonferroni-Holm correction applied",
      y = NULL,
      x = NULL
    ) +
    eda_theme +
    theme(legend.position = "none")
}

plot_pressure_means <- function(dat = df_final) {
  data_wide <- dat %>%
    group_by(rain_today) %>%
    summarise(
      `9:00 AM` = mean(pressure9am, na.rm = TRUE),
      `3:00 PM` = mean(pressure3pm, na.rm = TRUE),
      `Pressure drop` = mean(pressure9am, na.rm = TRUE) -
        mean(pressure3pm, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_longer(
      cols = -rain_today,
      names_to = "metric",
      values_to = "value"
    ) %>%
    mutate(
      metric = factor(
        metric,
        levels = c("9:00 AM", "3:00 PM", "Pressure drop")
      ),
      label_txt = round(value, 1)
    )

  ggplot(data_wide, aes(x = rain_today, y = value, fill = rain_today)) +
    geom_col(width = 0.6, alpha = 0.88) +
    geom_text(
      aes(label = label_txt),
      vjust = -0.5,
      fontface = "bold",
      size = 3.8
    ) +
    facet_wrap(~metric, scales = "free_y", nrow = 1) +
    scale_fill_manual(values = c("No" = "#AEB6BF", "Yes" = "#2471A3")) +
    scale_x_discrete(labels = c("No" = "Dry", "Yes" = "Rainy")) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
    labs(
      title = "Rainy Days Show Lower Baseline Pressure and Suppressed Diurnal Variation",
      subtitle = "The diurnal pressure drop is the stronger discriminating signal",
      y = "Pressure (hPa)",
      x = NULL
    ) +
    eda_theme +
    theme(legend.position = "none", panel.grid.major.x = element_blank())
}
