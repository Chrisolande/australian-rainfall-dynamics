librarian::shelf(tidyverse, kableExtra, performance)

# Function to display missing values
missing_val <- function(df) {
  missing_tab <- df %>%
    summarise(across(everything(), ~ mean(is.na(.)) * 100)) %>%
    pivot_longer(
      everything(),
      names_to = "column",
      values_to = "pct_missing"
    ) %>%
    arrange(desc(pct_missing))

  return(missing_tab %>% kable())
}

# Function to check multicollinearity
mc_check <- function(data) {
  vif_check <- lm(rainfall ~ ., data = data)
  test_collinearity <- check_collinearity(vif_check)
  return(test_collinearity)
}

# Function to select model features
select_model_features <- function(data, keep_location = TRUE) {
  cols_to_drop = c(
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

  cols_to_drop <- c(cols_to_drop)

  data <- data %>%
    select(-any_of(cols_to_drop)) %>%
    ungroup()

  return(data)
}

# Function to scale data
scale_data <- function(data) {
  df_scaled <- data %>%
    mutate(across(
      .cols = where(is.numeric) & !c("rainfall"),
      .fns = ~ as.numeric(scale(.x))
    ))

  return(df_scaled)
}

render_pooled_model_table <- function(model_result, model_name = "Model") {
  fs <- model_result$fit_stats

  sig_stars <- function(p) {
    dplyr::case_when(
      p < 0.001 ~ "***",
      p < 0.01 ~ "**",
      p < 0.05 ~ "*",
      p < 0.1 ~ "\u2020",
      TRUE ~ ""
    )
  }

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

  fmt_pval <- function(p) {
    ifelse(p < 0.001, "<0.001", sprintf("%.3f", p))
  }

  tbl <- model_result$pooled %>%
    dplyr::select(
      component,
      term,
      estimate,
      std.error,
      statistic,
      df,
      p.value,
      conf.low,
      conf.high
    ) %>%
    dplyr::mutate(
      stars = sig_stars(p.value),
      exp_est = sprintf("%.3f", exp(estimate)),
      ci = sprintf("[%.3f,\u00A0%.3f]", conf.low, conf.high),
      estimate = sprintf("%.3f%s", estimate, stars),
      SE = sprintf("%.3f", std.error),
      t = sprintf("%.2f", statistic),
      df = sprintf("%.1f", df),
      p = fmt_pval(p.value),
      Term = clean_term(term),
      Component = dplyr::case_when(
        component == "cond" ~ "Conditional (log link)",
        component == "zi" ~ "Zero-inflation (logit link)",
        TRUE ~ component
      )
    ) %>%
    dplyr::select(Component, Term, estimate, exp_est, ci, SE, t, df, p)

  group_idx <- table(factor(tbl$Component, levels = unique(tbl$Component)))

  tbl %>%
    dplyr::select(-Component) %>%
    knitr::kable(
      caption = model_name,
      align = c("l", "r", "r", "c", "r", "r", "r", "r"),
      col.names = c(
        "Term",
        "Estimate",
        "exp(\u03B2)",
        "95% CI",
        "SE",
        "t",
        "df",
        "p"
      ),
      escape = FALSE
    ) %>%
    kableExtra::kable_styling(
      bootstrap_options = c("striped", "hover", "condensed"),
      full_width = FALSE,
      font_size = 12,
      html_font = '"Source Sans Pro", "Helvetica Neue", Helvetica, Arial, sans-serif'
    ) %>%
    kableExtra::add_header_above(
      c(" " = 1, "Estimates" = 2, "Confidence" = 1, "Inference" = 4),
      bold = TRUE,
      line = TRUE,
      font_size = 13
    ) %>%
    kableExtra::pack_rows(
      index = group_idx,
      bold = TRUE,
      italic = TRUE,
      hline_before = TRUE,
      hline_after = TRUE
    ) %>%
    kableExtra::row_spec(0, bold = TRUE, color = "#2c3e50") %>%
    kableExtra::footnote(
      general = c(
        "\u2020 p<0.1   * p<0.05   ** p<0.01   *** p<0.001",
        sprintf(
          "AIC: %.1f   BIC: %.1f   log-Lik: %.1f   (averaged across imputed datasets)",
          fs$heuristic_AIC,
          fs$heuristic_BIC,
          fs$mean_logLik
        ),
        "Pooled via Rubin\u2019s rules."
      ),
      general_title = "Note: ",
      footnote_as_chunk = FALSE,
      escape = TRUE
    ) %>%
    kableExtra::column_spec(1, width = "14em") %>%
    kableExtra::column_spec(2, width = "7em") %>%
    kableExtra::column_spec(
      4,
      width = "14em",
      extra_css = "white-space: nowrap;"
    ) %>%
    kableExtra::column_spec(8, width = "5em")
}
