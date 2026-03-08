# sensitivity_analysis.R
# Four sensitivity analysis functions operating on the mids object.
# Each returns a ggplot or kable and is called directly from QMD chunks.

CONTINUOUS_GHOST_VARS <- c("sunshine", "evaporation", "cloud9am", "cloud3pm")

FLAG_MAP <- c(
  sunshine = "sunshine_imp_flagged",
  evaporation = "evap_imp_flagged",
  cloud9am = "cloud9am_imp_flagged",
  cloud3pm = "cloud3pm_imp_flagged"
)


# MCMC convergence trace plots

#' Plot chain means and SDs across iterations for the four PMM variables.
#' @param mids A mids object.
#' @return A patchwork ggplot.
plot_convergence_trace <- function(mids = imp_mids) {
  trace_vars <- CONTINUOUS_GHOST_VARS

  cm <- mids$chainMean
  cv <- mids$chainVar
  n_iter <- dim(cm)[2]
  n_chain <- dim(cm)[3]

  trace_df <- map_dfr(trace_vars, function(var) {
    map_dfr(seq_len(n_chain), function(i) {
      mean_v <- as.numeric(cm[var, , i])
      sd_v <- sqrt(pmax(as.numeric(cv[var, , i]), 0))
      tibble(
        variable = var,
        chain = factor(i),
        iteration = seq_len(n_iter),
        mean_val = mean_v,
        sd_val = sd_v
      )
    })
  }) %>%
    filter(!is.nan(mean_val))

  p_mean <- ggplot(trace_df, aes(x = iteration, y = mean_val, colour = chain)) +
    geom_line(linewidth = 0.6, alpha = 0.8) +
    facet_wrap(~variable, scales = "free_y", ncol = 2) +
    scale_colour_viridis_d(option = "turbo", guide = guide_legend(nrow = 2)) +
    scale_x_continuous(breaks = seq_len(n_iter)) +
    labs(y = "Chain mean", x = NULL, colour = "Chain") +
    report_theme +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

  p_sd <- ggplot(trace_df, aes(x = iteration, y = sd_val, colour = chain)) +
    geom_line(linewidth = 0.6, alpha = 0.8) +
    facet_wrap(~variable, scales = "free_y", ncol = 2) +
    scale_colour_viridis_d(option = "turbo", guide = guide_legend(nrow = 2)) +
    scale_x_continuous(breaks = seq_len(n_iter)) +
    labs(y = "Chain SD", x = "Iteration", colour = "Chain") +
    report_theme

  (p_mean / p_sd) +
    plot_layout(guides = "collect") +
    plot_annotation(
      title = "MCMC Convergence Diagnostics",
      subtitle = "Chain means (top) and standard deviations (bottom) across iterations",
      theme = theme(
        plot.title = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(size = 10, colour = "grey40")
      )
    ) &
    theme(legend.position = "bottom")
}


# Distributional fidelity

#' Density plot: observed vs imputed marginal distributions.
#' @param final A completed data frame (df_final).
#' @return A ggplot.
plot_dist_fidelity <- function(final = df_final) {
  fidelity_df <- map_dfr(CONTINUOUS_GHOST_VARS, function(var) {
    flag <- FLAG_MAP[[var]]
    bind_rows(
      final %>%
        filter(.data[[flag]] == 0, !is.na(.data[[var]])) %>%
        transmute(value = .data[[var]], source = "Observed", variable = var),
      final %>%
        filter(.data[[flag]] == 1, !is.na(.data[[var]])) %>%
        transmute(value = .data[[var]], source = "Imputed", variable = var)
    )
  })

  ks_results <- fidelity_df %>%
    group_by(variable) %>%
    summarise(
      ks_stat = ks.test(
        value[source == "Observed"],
        value[source == "Imputed"]
      )$statistic,
      .groups = "drop"
    ) %>%
    mutate(label = sprintf("D = %.3f", ks_stat))

  ggplot(fidelity_df, aes(x = value, fill = source, colour = source)) +
    geom_density(alpha = 0.35, linewidth = 0.6) +
    geom_text(
      data = ks_results,
      aes(label = label, x = Inf, y = Inf),
      inherit.aes = FALSE,
      hjust = 1.1,
      vjust = 1.4,
      size = 3.2,
      colour = "grey30"
    ) +
    facet_wrap(~variable, scales = "free", ncol = 2) +
    scale_fill_manual(
      values = c("Observed" = "#2C7BB6", "Imputed" = "#D7191C")
    ) +
    scale_colour_manual(
      values = c("Observed" = "#2C7BB6", "Imputed" = "#D7191C")
    ) +
    labs(
      title = "Distributional Fidelity: Observed vs. Imputed Values",
      subtitle = "KS statistic D annotated per panel. P-values not reported (uninformative at this sample size).",
      x = "Value",
      y = "Density",
      fill = "Source",
      colour = "Source",
      caption = "Observed = instrumented stations (flag = 0). Imputed = ghost sensor stations (flag = 1)."
    ) +
    report_theme +
    theme(legend.position = "bottom")
}

#' KS results table with interpretation column.
#' @param final A completed data frame (df_final).
render_ks_table <- function(final = df_final) {
  fidelity_df <- map_dfr(CONTINUOUS_GHOST_VARS, function(var) {
    flag <- FLAG_MAP[[var]]
    bind_rows(
      final %>%
        filter(.data[[flag]] == 0, !is.na(.data[[var]])) %>%
        transmute(value = .data[[var]], source = "Observed", variable = var),
      final %>%
        filter(.data[[flag]] == 1, !is.na(.data[[var]])) %>%
        transmute(value = .data[[var]], source = "Imputed", variable = var)
    )
  })

  fidelity_df %>%
    group_by(variable) %>%
    summarise(
      ks_stat = ks.test(
        value[source == "Observed"],
        value[source == "Imputed"]
      )$statistic,
      .groups = "drop"
    ) %>%
    mutate(
      interpretation = case_when(
        ks_stat <
          0.05 ~ "Negligible shift; distributions effectively identical",
        ks_stat < 0.10 ~ "Small shift; acceptable fidelity",
        ks_stat < 0.20 ~ "Moderate shift; review predictor set",
        TRUE ~ "Large shift; imputation has drifted from observed"
      )
    ) %>%
    select(variable, ks_stat, interpretation) %>%
    kable(
      digits = 3,
      col.names = c("Variable", "KS Statistic (D)", "Interpretation"),
      booktabs = TRUE
    ) %>%
    kable_styling(
      bootstrap_options = c("striped", "hover"),
      full_width = FALSE,
      latex_options = c("hold_position")
    )
}


# Between-imputation variance

#' Ribbon plot showing seed-to-seed spread across m completions.
#' @param mids A mids object.
#' @return A ggplot.
plot_between_imputation_variance <- function(mids = imp_mids) {
  M <- mids$m
  trace_vars <- CONTINUOUS_GHOST_VARS

  all_completions <- map_dfr(seq_len(M), function(m) {
    complete(mids, action = m) %>%
      select(all_of(trace_vars)) %>%
      mutate(imputation = factor(m), row_id = row_number())
  })

  ribbon_df <- all_completions %>%
    pivot_longer(
      cols = all_of(trace_vars),
      names_to = "variable",
      values_to = "value"
    ) %>%
    group_by(variable, row_id) %>%
    summarise(
      mean_val = mean(value, na.rm = TRUE),
      min_val = min(value, na.rm = TRUE),
      max_val = max(value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(variable, mean_val) %>%
    group_by(variable) %>%
    mutate(rank = row_number()) %>%
    ungroup()

  ggplot(ribbon_df, aes(x = rank)) +
    geom_ribbon(
      aes(ymin = min_val, ymax = max_val),
      fill = "#2C7BB6",
      alpha = 0.25
    ) +
    geom_line(aes(y = mean_val), colour = "#2C7BB6", linewidth = 0.5) +
    facet_wrap(~variable, scales = "free", ncol = 2) +
    labs(
      title = "Between-Imputation Variance Across Ten Completions",
      subtitle = "Ribbon spans the full range of imputed values; line is the cross-completion mean.",
      x = "Observation rank (ascending mean)",
      y = "Imputed value",
      caption = paste0(
        "m = ",
        M,
        " completions. Observations sorted independently within each panel."
      )
    ) +
    report_theme
}

#' Summary table of between-imputation variance statistics.
#' @param mids A mids object.
render_between_variance_table <- function(mids = imp_mids) {
  M <- mids$m
  trace_vars <- CONTINUOUS_GHOST_VARS

  all_completions <- map_dfr(seq_len(M), function(m) {
    complete(mids, action = m) %>%
      select(all_of(trace_vars)) %>%
      mutate(imputation = factor(m), row_id = row_number())
  })

  all_completions %>%
    pivot_longer(
      cols = all_of(trace_vars),
      names_to = "variable",
      values_to = "value"
    ) %>%
    group_by(variable, row_id) %>%
    summarise(
      range_val = max(value, na.rm = TRUE) - min(value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(variable) %>%
    summarise(
      mean_range = mean(range_val, na.rm = TRUE),
      median_range = median(range_val, na.rm = TRUE),
      cv_range = sd(range_val, na.rm = TRUE) / mean(range_val, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    kable(
      digits = 3,
      col.names = c("Variable", "Mean Range", "Median Range", "CV of Range"),
      booktabs = TRUE
    ) %>%
    kable_styling(
      bootstrap_options = c("striped", "hover"),
      full_width = FALSE,
      latex_options = c("hold_position")
    )
}


# Rubin's Rules variance decomposition

RUBIN_PREDICTOR_MAP <- list(
  sunshine = c("cloud9am", "cloud3pm", "max_temp", "humidity3pm"),
  evaporation = c("wind_gust_speed", "max_temp", "humidity3pm"),
  cloud9am = c("humidity9am", "pressure9am", "humidity3pm"),
  cloud3pm = c("humidity3pm", "pressure3pm", "humidity9am")
)

#' Compute and render the Rubin FMI decomposition table.
#' @param mids A mids object.
render_rubins_rules_table <- function(mids = imp_mids) {
  rubin_results <- map_dfr(CONTINUOUS_GHOST_VARS, function(outcome) {
    flag_col <- FLAG_MAP[[outcome]]
    preds <- RUBIN_PREDICTOR_MAP[[outcome]]
    available <- intersect(preds, names(mids$data))

    if (length(available) == 0) {
      return(NULL)
    }

    fml <- as.formula(paste(outcome, "~", paste(available, collapse = " + ")))

    tryCatch(
      {
        fits <- lapply(seq_len(mids$m), function(m) {
          df_m <- complete(mids, action = m) %>%
            filter(.data[[flag_col]] == 0L)
          lm(fml, data = df_m)
        })
        pooled <- pool(as.mira(fits))
        pr <- pooled$pooled %>% filter(term == "(Intercept)")

        tibble(
          variable = outcome,
          W = pr$ubar,
          B = pr$b,
          T_total = pr$t,
          FMI = pr$fmi
        )
      },
      error = function(e) {
        message(
          "Rubin decomposition failed for ",
          outcome,
          ": ",
          conditionMessage(e)
        )
        NULL
      }
    )
  })

  stopifnot(
    "All Rubin decompositions failed; check imp_mids structure" = nrow(
      rubin_results
    ) >
      0
  )

  rubin_results %>%
    mutate(
      pct_between = B / T_total * 100,
      fmi_class = case_when(
        FMI < 0.10 ~ "Low; single completion adequate",
        FMI < 0.30 ~ "Moderate; pooling recommended",
        TRUE ~ "High; pooling required"
      )
    ) %>%
    kable(
      digits = 4,
      col.names = c(
        "Variable",
        "Within (W)",
        "Between (B)",
        "Total (T)",
        "FMI",
        "Between (%)",
        "FMI Classification"
      ),
      booktabs = TRUE
    ) %>%
    kable_styling(
      bootstrap_options = c("striped", "hover"),
      full_width = FALSE,
      latex_options = c("hold_position")
    )
}
