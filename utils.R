librarian::shelf(tidyverse, kableExtra, performance)

missing_val <- function(df) {
  missing_tab <- df %>%
    summarise(across(everything(), ~ mean(is.na(.)) * 100)) %>%
    pivot_longer(
      everything(),
      names_to = "column",
      values_to = "pct_missing"
    ) %>%
    arrange(desc(pct_missing))

  missing_tab %>%
    kable(
      caption = "Percentage of Missing Values by Feature",
      digits = 2,
      col.names = c("Feature", "Missing (%)"),
      booktabs = TRUE
    ) %>%
    kable_styling(
      bootstrap_options = c("striped", "hover"),
      full_width = FALSE,
      latex_options = c("hold_position")
    )
}

mc_check <- function(data) {
  vif_fit <- lm(rainfall ~ ., data = data)
  check_collinearity(vif_fit)
}

select_model_features <- function(data, keep_location = TRUE) {
  cols_to_drop <- c(
    "month",
    "day",
    "day_of_year",
    "date",
    "temp9am",
    "temp3pm",
    "min_temp",
    "max_temp",
    "pressure3pm",
    "pressure9am",
    "cloud3pm",
    "cloud9am",
    "dewpoint_3pm",
    "wind_dir3pm",
    "wind_speed3pm",
    "wind_gust_dir",
    "wind_gust_speed",
    "wind_speed9am",
    "wind_dir9am",
    "wind9am_rad",
    "gust_rad",
    "moisture_index",
    "rain_today"
  )

  if (!keep_location) {
    cols_to_drop <- c(cols_to_drop, "location")
  }

  result <- data %>%
    ungroup() %>%
    select(-any_of(cols_to_drop))

  result
}

scale_data <- function(data) {
  data %>%
    mutate(across(
      .cols = where(is.numeric) & !any_of("rainfall"),
      .fns = ~ as.numeric(scale(.))
    ))
}

render_pooled_model_table <- function(model_result, model_name = "Model") {
  fs <- model_result$fit_stats

  fit_footnote <- sprintf(
    paste0(
      '<span style="font-size:10px; color:#666;">',
      '\u2020 p<0.1 &ensp; * p<0.05 &ensp; ** p<0.01 &ensp; *** p<0.001',
      ' &ensp;\u2502&ensp; ',
      'AIC: %.1f &ensp; BIC: %.1f &ensp; log-Lik: %.1f',
      ' &ensp;\u2502&ensp; ',
      'Pooled via Rubin\u2019s rules across imputed datasets.',
      '</span>'
    ),
    fs$AIC,
    fs$BIC,
    fs$logLik
  )

  # significance stars
  sig_stars <- function(p) {
    case_when(
      p < 0.001 ~ "***",
      p < 0.01 ~ "**",
      p < 0.05 ~ "*",
      p < 0.1 ~ "\u2020",
      TRUE ~ ""
    )
  }

  # clean up term names for readability
  clean_term <- function(term) {
    term %>%
      gsub("_", " ", .) %>%
      gsub(
        "\\bns\\((.+),\\s*df\\s*=\\s*(\\d+)\\)",
        "\\1 (spline, df=\\2)",
        .
      ) %>%
      gsub("\\(Intercept\\)", "Intercept", .) %>%
      gsub("rain yesterdayYes", "Rain yesterday (yes)", .) %>%
      gsub("\\b([a-z])", "\\U\\1", ., perl = TRUE)
  }

  # format p-values
  fmt_pval <- function(p) {
    ifelse(p < 0.001, "<0.001", sprintf("%.3f", p))
  }

  # Build table data
  tbl <- model_result$pooled %>%
    select(
      component,
      term,
      estimate,
      std.error,
      statistic,
      df,
      p.value,
      `2.5 %`,
      `97.5 %`
    ) %>%
    mutate(
      stars = sig_stars(p.value),
      exp_est = sprintf("%.3f", exp(estimate)),
      ci = sprintf("[%.3f,\u00a0%.3f]", `2.5 %`, `97.5 %`),
      estimate = sprintf("%.3f%s", estimate, stars),
      SE = sprintf("%.3f", std.error),
      t = sprintf("%.2f", statistic),
      df = sprintf("%.1f", df),
      p = fmt_pval(p.value),
      Term = clean_term(term),
      Component = case_when(
        component == "cond" ~ "Conditional (log link)",
        component == "zi" ~ "Zero-inflation (logit link)",
        TRUE ~ component
      )
    ) %>%
    select(Component, Term, estimate, exp_est, ci, SE, t, df, p)

  # Row grouping index
  group_idx <- table(factor(tbl$Component, levels = unique(tbl$Component)))

  tbl %>%
    select(-Component) %>%
    kable(
      caption = model_name,
      align = c("l", "r", "r", "c", "r", "r", "r", "r"),
      col.names = c(
        "Term",
        "Estimate",
        "exp(\u03B2)",
        "95% CI",
        "SE",
        "\\(t\\)",
        "df",
        "\\(p\\)"
      ),
      escape = FALSE
    ) %>%
    kable_styling(
      bootstrap_options = c("striped", "hover", "condensed"),
      full_width = FALSE,
      font_size = 12,
      html_font = '"Source Sans Pro", "Helvetica Neue", Helvetica, Arial, sans-serif'
    ) %>%
    add_header_above(
      c(
        " " = 1,
        "Estimates" = 2,
        "Confidence" = 1,
        "Inference" = 4
      ),
      bold = TRUE,
      line = TRUE,
      font_size = 13
    ) %>%
    pack_rows(
      index = group_idx,
      bold = TRUE,
      italic = TRUE,
      hline_before = TRUE,
      hline_after = TRUE
    ) %>%
    row_spec(0, bold = TRUE, color = "#2c3e50") %>%
    footnote(
      general = fit_footnote,
      general_title = "",
      footnote_as_chunk = TRUE,
      escape = FALSE
    ) %>%
    column_spec(1, width = "14em") %>%
    column_spec(2, width = "7em") %>%
    column_spec(4, width = "10em") %>%
    column_spec(8, width = "5em")
}
