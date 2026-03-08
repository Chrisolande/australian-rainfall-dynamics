# eda_correlation.R
# Three outputs for the bivariate correlation section:
# 1. render_correlation_table() - ranked Spearman table
# 2. plot_correlation_matrix() - heatmap
# 3. run_cocor_test() - Steiger Z-test comparing humidity vs sunshine

render_correlation_table <- function(dat = df_clean) {
  numeric_cols <- dat %>%
    select(where(is.numeric)) %>%
    names()
  numeric_cols <- numeric_cols[numeric_cols != "rainfall"]

  dat %>%
    cor_test(
      vars = "rainfall",
      vars2 = numeric_cols,
      method = "spearman"
    ) %>%
    filter(!is.na(cor)) %>%
    arrange(desc(abs(cor))) %>%
    select(var2, cor, p) %>%
    mutate(
      interpretation = case_when(
        abs(cor) < 0.1 ~ "Negligible",
        abs(cor) < 0.3 ~ "Small",
        abs(cor) < 0.5 ~ "Moderate",
        TRUE ~ "Large"
      )
    ) %>%
    kable(
      caption = "Spearman Correlation with Rainfall (Ranked by Strength)",
      col.names = c("Predictor", "Correlation (r)", "P-Value", "Strength"),
      booktabs = TRUE,
      digits = 3
    ) %>%
    kable_styling(
      bootstrap_options = c("striped", "hover"),
      full_width = FALSE,
      latex_options = c("hold_position")
    )
}

plot_correlation_matrix <- function(dat = df_clean) {
  cor_matrix <- dat %>%
    select(where(is.numeric)) %>%
    cor(use = "pairwise.complete.obs", method = "spearman")

  cor_melt <- cor_matrix %>%
    as.data.frame() %>%
    rownames_to_column(var = "Var1") %>%
    pivot_longer(cols = -Var1, names_to = "Var2", values_to = "Correlation")

  ggplot(cor_melt, aes(x = Var1, y = Var2, fill = Correlation)) +
    geom_tile(colour = "white", linewidth = 0.3) +
    scale_fill_gradient2(
      low = "#B2182B",
      mid = "white",
      high = "#2166AC",
      midpoint = 0,
      limit = c(-1, 1),
      name = "Spearman\nr"
    ) +
    geom_text(
      data = filter(cor_melt, abs(Correlation) > 0.3),
      aes(label = sprintf("%.2f", Correlation)),
      colour = "grey15",
      size = 2.8,
      fontface = "plain"
    ) +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    labs(
      title = "Feature Correlation Matrix",
      subtitle = "Strongest predictors: Humidity3pm (positive) and Sunshine (negative)",
      x = NULL,
      y = NULL
    ) +
    eda_theme +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 9),
      axis.text.y = element_text(size = 9),
      panel.grid.major = element_blank(),
      legend.position = "right",
      legend.key.height = unit(1.2, "cm"),
      legend.key.width = unit(0.4, "cm")
    )
}

run_cocor_test <- function(dat = df_clean) {
  cor_humidity <- cor.test(dat$rainfall, dat$humidity3pm, method = "spearman")
  cor_sunshine <- cor.test(dat$rainfall, dat$sunshine, method = "spearman")

  cocor_result <- cocor.dep.groups.overlap(
    r.jk = cor_humidity$estimate,
    r.jh = cor_sunshine$estimate,
    r.kh = cor(
      dat$humidity3pm,
      dat$sunshine,
      use = "complete.obs",
      method = "spearman"
    ),
    n = nrow(dat),
    alternative = "two.sided",
    test = "steiger1980",
    return.htest = TRUE
  )

  print(cocor_result)
}
