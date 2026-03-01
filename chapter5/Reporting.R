source(here::here("config.R"))

# Shared formatting helpers
# %%
sig_stars <- function(p) {
  dplyr::case_when(
    p < 0.001 ~ "***",
    p < 0.01 ~ "**",
    p < 0.05 ~ "*",
    p < 0.1 ~ "\u2020",
    TRUE ~ ""
  )
}

fmt_pval <- function(p) ifelse(p < 0.001, "&lt;0.001", sprintf("%.3f", p))

clean_term <- function(term, zi_prefix = "ZI: ") {
  term %>%
    gsub("^zi\\.", zi_prefix, .) %>%
    gsub("_", " ", .) %>%
    gsub("\\bns\\((.+),\\s*df\\s*=\\s*(\\d+)\\)", "\\1 (spline, df=\\2)", .) %>%
    gsub("\\(Intercept\\)", "Intercept", .) %>%
    gsub("rain yesterdayYes", "Rain yesterday (yes)", .) %>%
    gsub("\\b([a-z])", "\\U\\1", ., perl = TRUE)
}

base_kable_style <- function(kbl) {
  kbl %>%
    kableExtra::kable_styling(
      bootstrap_options = c("striped", "hover", "condensed"),
      full_width = FALSE,
      font_size = 12,
      html_font = '"Source Sans Pro", "Helvetica Neue", Helvetica, Arial, sans-serif'
    ) %>%
    kableExtra::row_spec(0, bold = TRUE, color = "#2c3e50")
}

std_footnote <- function(kbl, extra = NULL) {
  kableExtra::footnote(
    kbl,
    general = c(
      "\u2020 p<0.1   * p<0.05   ** p<0.01   *** p<0.001",
      extra,
      "Pooled via Rubin\u2019s rules."
    ),
    general_title = "Note: ",
    footnote_as_chunk = FALSE,
    escape = TRUE
  )
}

# %%
render_pooled_model_table <- function(model_result, model_name = "Model") {
  fs <- model_result$fit_stats

  tbl <- model_result$pooled %>%
    dplyr::mutate(
      Component = dplyr::if_else(
        component == "cond",
        "Conditional (log link)",
        "Zero-inflation (logit link)"
      ),
      Term = clean_term(term),
      exp_est = sprintf("%.3f", exp(estimate)),
      estimate = sprintf("%.3f%s", estimate, sig_stars(p.value)),
      ci = sprintf("[%.3f,\u00A0%.3f]", conf.low, conf.high),
      SE = sprintf("%.3f", std.error),
      t = sprintf("%.2f", statistic),
      df = sprintf("%.1f", df),
      p = fmt_pval(p.value)
    ) %>%
    dplyr::select(Component, Term, estimate, exp_est, ci, SE, t, df, p)

  group_idx <- table(factor(tbl$Component, levels = unique(tbl$Component)))

  tbl %>%
    dplyr::select(-Component) %>%
    knitr::kable(
      format = "html",
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
    base_kable_style() %>%
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
    std_footnote(
      extra = sprintf(
        "AIC: %.1f   BIC: %.1f   log-Lik: %.1f   (averaged across imputed datasets)",
        fs$heuristic_AIC,
        fs$heuristic_BIC,
        fs$mean_logLik
      )
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

# %%
render_comparison_table <- function(comp, names = c("larger", "smaller")) {
  ic_note <- stringr::str_glue(
    "delta AIC = {round(comp$delta_AIC, 2)}, delta BIC = {round(comp$delta_BIC, 2)} ",
    "(negative favours {names[1]}; heuristic only). ",
    "Based on {comp$m_compared} common imputations."
  )

  if (!is.null(comp$mitml_result)) {
    d1 <- comp$mitml_result

    tibble::tibble(
      Test = "D1 pooled Wald",
      Parameters = paste(clean_term(comp$actual_diff), collapse = "<br>"),
      F = sprintf("%.3f", d1$test[1, "F.value"]),
      df1 = sprintf("%.0f", d1$test[1, "df1"]),
      df2 = sprintf("%.1f", d1$test[1, "df2"]),
      RIV = sprintf("%.3f", d1$test[1, "RIV"]),
      p = fmt_pval(d1$test[1, "P(>F)"])
    ) %>%
      knitr::kable(
        format = "html",
        caption = stringr::str_glue(
          "Nested Model Comparison: {names[1]} vs {names[2]}"
        ),
        align = c("l", "l", "r", "r", "r", "r", "r"),
        col.names = c(
          "Test",
          "Parameters tested",
          "F",
          "df1",
          "df2",
          "RIV",
          "p"
        ),
        escape = FALSE
      ) %>%
      base_kable_style() %>%
      kableExtra::column_spec(1, width = "10em") %>%
      kableExtra::column_spec(
        2,
        width = "20em",
        extra_css = "white-space: pre-wrap; word-break: break-word;"
      ) %>%
      kableExtra::column_spec(3:7, width = "5em") %>%
      std_footnote(extra = ic_note)
  } else {
    tbl <- comp$fallback_result %>%
      dplyr::mutate(
        Component = dplyr::if_else(
          component == "cond",
          "Conditional",
          "Zero-inflation"
        ),
        term = clean_term(term),
        estimate = sprintf("%.3f%s", estimate, sig_stars(p.value)),
        std.error = sprintf("%.3f", std.error),
        statistic = sprintf("%.2f", statistic),
        df = sprintf("%.1f", df),
        p = fmt_pval(p.value)
      ) %>%
      dplyr::select(Component, term, estimate, std.error, statistic, df, p)

    group_idx <- table(factor(tbl$Component, levels = unique(tbl$Component)))

    tbl %>%
      dplyr::select(-Component) %>%
      knitr::kable(
        format = "html",
        caption = stringr::str_glue(
          "Nested Model Comparison: {names[1]} vs {names[2]} (fallback)"
        ),
        align = c("l", "r", "r", "r", "r", "r"),
        col.names = c("Term", "Estimate", "SE", "t", "df", "p"),
        escape = FALSE
      ) %>%
      base_kable_style() %>%
      kableExtra::pack_rows(
        index = group_idx,
        bold = TRUE,
        italic = TRUE,
        hline_before = TRUE,
        hline_after = TRUE
      ) %>%
      std_footnote(
        extra = c("WARNING: individual tests only, not a joint test.", ic_note)
      )
  }
}
