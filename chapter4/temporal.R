# eda_temporal.R
# Three temporal analyses:
# A. Frequency tables (weekly, monthly, cross-tab)
# B. Markov chain (contingency table, chi-sq, Cramer's V, heatmap)
# C. Dry spell dynamics (logistic model, spline LRT, decay plot)

# A. Frequency tables

render_weekly_frequency_table <- function(dat = df_clean) {
  dat %>%
    filter(rainfall > 0) %>%
    tabyl(day) %>%
    adorn_pct_formatting() %>%
    arrange(desc(n)) %>%
    kable(
      caption = "Frequency of Rainfall Days by Day of the Week",
      col.names = c("Day", "Count (n)", "Percentage"),
      booktabs = TRUE
    ) %>%
    kable_styling(
      bootstrap_options = c("striped", "hover"),
      full_width = FALSE,
      latex_options = c("hold_position")
    )
}

render_monthly_frequency_table <- function(dat = df_clean) {
  dat %>%
    filter(rainfall > 0) %>%
    tabyl(month) %>%
    adorn_pct_formatting() %>%
    arrange(desc(n)) %>%
    kable(
      caption = "Frequency of Rainfall Days by Month",
      col.names = c("Month", "Count (n)", "Percentage"),
      booktabs = TRUE
    ) %>%
    kable_styling(
      bootstrap_options = c("striped", "hover"),
      full_width = FALSE,
      latex_options = c("hold_position")
    )
}

render_month_day_crosstab <- function(dat = df_clean) {
  dat %>%
    filter(rainfall > 0) %>%
    tabyl(month, day) %>%
    adorn_totals(c("row", "col")) %>%
    kable(caption = "Cross-tabulation of Rainfall Frequency: Month vs. Day") %>%
    kable_styling(
      bootstrap_options = c("striped", "condensed"),
      font_size = 11
    ) %>%
    scroll_box(width = "100%")
}


# B. Markov chain

build_markov_table <- function(dat = df_final) {
  dat %>%
    group_by(location) %>%
    arrange(date) %>%
    mutate(yesterday_rain = lag(rain_today)) %>%
    ungroup() %>%
    filter(!is.na(rain_today), !is.na(yesterday_rain)) %>%
    count(yesterday_rain, rain_today)
}

build_cont_table <- function(markov_tbl) {
  markov_tbl %>%
    pivot_wider(names_from = rain_today, values_from = n, values_fill = 0) %>%
    column_to_rownames("yesterday_rain") %>%
    as.matrix()
}

run_markov_stats <- function(cont_tbl) {
  chi_result <- rstatix::chisq_test(as.table(cont_tbl))
  cramers_v <- cramer_v(cont_tbl)

  cat("\nEffect Size Interpretation\n")
  cat(sprintf("V = %.4f: ", cramers_v))

  if (cramers_v < 0.1) {
    cat("Negligible Association\n")
  } else if (cramers_v < 0.3) {
    cat("Weak Association\n")
  } else if (cramers_v < 0.5) {
    cat("Moderate Association\n")
  } else {
    cat("Strong Association\n")
  }

  list(chi_result = chi_result, cramers_v = cramers_v)
}

plot_markov_chain <- function(dat = df_final, markov_stats) {
  markov_data <- dat %>%
    group_by(location) %>%
    arrange(date) %>%
    mutate(yesterday_rain = lag(rain_today)) %>%
    ungroup() %>%
    filter(!is.na(rain_today), !is.na(yesterday_rain))

  markov_plot_df <- markov_data %>%
    count(yesterday_rain, rain_today) %>%
    group_by(yesterday_rain) %>%
    mutate(prob = n / sum(n)) %>%
    ungroup()

  ggplot(markov_plot_df, aes(x = yesterday_rain, y = rain_today, fill = prob)) +
    geom_tile(colour = "white", linewidth = 1) +
    geom_text(
      aes(label = percent(prob)),
      colour = "white",
      size = 7,
      fontface = "bold"
    ) +
    scale_fill_gradientn(
      colours = c("#D4E6F1", "#2471A3", "#154360"),
      values = c(0, 0.5, 1),
      limits = c(0, 1),
      labels = percent_format(accuracy = 1),
      name = "Transition\nprobability"
    ) +
    labs(
      title = "Markov Chain: Daily Rain Persistence",
      subtitle = sprintf(
        "X\u00b2 = %.0f, p < 0.001, Cramer's V = %.3f (moderate association)",
        markov_stats$chi_result$statistic,
        markov_stats$cramers_v
      ),
      x = "Did it rain yesterday?",
      y = "Did it rain today?"
    ) +
    eda_theme +
    theme(
      panel.grid.major = element_blank(),
      legend.position = "right"
    )
}


# C. Dry spell dynamics

build_dry_spell_data <- function(dat = df_final) {
  dat %>%
    group_by(location) %>%
    arrange(date) %>%
    mutate(
      did_rain_yesterday = lag(rainfall > 0, default = FALSE),
      dry_spell_id = cumsum(did_rain_yesterday)
    ) %>%
    group_by(location, dry_spell_id) %>%
    mutate(days_since_rain = row_number()) %>%
    ungroup() %>%
    filter(days_since_rain <= 30) %>%
    mutate(rain_binary = as.numeric(rainfall > 0))
}

run_dry_spell_models <- function(dry_spell_dat) {
  logit_model <- glm(
    rain_binary ~ days_since_rain,
    data = dry_spell_dat,
    family = binomial(link = "logit")
  )
  logit_spline <- glm(
    rain_binary ~ splines::ns(days_since_rain, df = 4),
    data = dry_spell_dat,
    family = binomial
  )

  wald_test <- aod::wald.test(
    b = coef(logit_model),
    Sigma = vcov(logit_model),
    Terms = 2
  )
  or_results <- tidy(
    logit_model,
    conf.int = TRUE,
    exponentiate = TRUE
  ) %>%
    filter(term == "days_since_rain")
  lrt_result <- anova(logit_model, logit_spline, test = "LRT")

  print(wald_test)
  cat(sprintf(
    "\nFor each additional day without rain, odds of rainfall decrease by %.1f%%\n",
    (1 - or_results$estimate) * 100
  ))
  cat(sprintf(
    "95%% CI: [%.3f, %.3f]\n",
    or_results$conf.low,
    or_results$conf.high
  ))
  print(lrt_result)

  list(
    logit_model = logit_model,
    logit_spline = logit_spline,
    wald_test = wald_test,
    or_results = or_results,
    lrt_result = lrt_result
  )
}

plot_dry_spell <- function(dry_spell_dat, dry_spell_models) {
  logit_model <- dry_spell_models$logit_model
  wald_test <- dry_spell_models$wald_test
  or_results <- dry_spell_models$or_results

  pred_data <- tibble(days_since_rain = 1:30)
  pred_out <- predict(
    logit_model,
    newdata = pred_data,
    type = "response",
    se.fit = TRUE
  )
  pred_data <- pred_data %>%
    mutate(pred_prob = pred_out$fit, pred_se = pred_out$se.fit)

  empirical_probs <- dry_spell_dat %>%
    group_by(days_since_rain) %>%
    summarise(
      prob_rain = mean(rain_binary, na.rm = TRUE),
      n = n(),
      se = sqrt(prob_rain * (1 - prob_rain) / n),
      .groups = "drop"
    )

  ggplot() +
    geom_ribbon(
      data = pred_data,
      aes(
        x = days_since_rain,
        ymin = pred_prob - 1.96 * pred_se,
        ymax = pred_prob + 1.96 * pred_se
      ),
      alpha = 0.18,
      fill = "#C0392B"
    ) +
    geom_line(
      data = pred_data,
      aes(x = days_since_rain, y = pred_prob),
      colour = "#C0392B",
      linewidth = 1,
      linetype = "dashed"
    ) +
    geom_pointrange(
      data = empirical_probs,
      aes(
        x = days_since_rain,
        y = prob_rain,
        ymin = prob_rain - 1.96 * se,
        ymax = prob_rain + 1.96 * se
      ),
      size = 0.35,
      colour = "grey20",
      linewidth = 0.5
    ) +
    scale_y_continuous(
      labels = percent_format(accuracy = 1),
      breaks = pretty_breaks(n = 6),
      expand = expansion(mult = c(0.02, 0.04))
    ) +
    scale_x_continuous(
      breaks = pretty_breaks(),
      expand = expansion(mult = c(0.02, 0.02))
    ) +
    labs(
      x = "Days since last rain",
      y = "Probability of rainfall",
      title = "Dry Spell Effect on Rain Probability",
      subtitle = sprintf(
        "Logistic regression: B = %.4f, Wald chi\u00b2 = %.0f, p < 0.001 | Each additional dry day reduces odds of rain by %.1f%%",
        coef(logit_model)[2],
        wald_test$result$chi2[1],
        (1 - or_results$estimate) * 100
      ),
      caption = "Points: empirical probabilities +/- 95% CI | Line: linear logistic model fit"
    ) +
    eda_theme
}
