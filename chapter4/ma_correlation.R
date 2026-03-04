# ma_correlation.R
# Spearman correlation heatmap between raw variables and their MA counterparts.

plot_ma_correlation <- function(ma_dat) {
  cor_data <- ma_dat %>%
    select(
      rainfall,
      rainfall_ma3,
      rainfall_ma7,
      humidity3pm,
      humidity_ma3,
      humidity_ma7
    ) %>%
    cor(use = "complete.obs", method = "spearman") %>%
    as.data.frame() %>%
    rownames_to_column("var1") %>%
    pivot_longer(-var1, names_to = "var2", values_to = "correlation") %>%
    mutate(
      var1 = factor(
        var1,
        levels = c(
          "rainfall",
          "rainfall_ma3",
          "rainfall_ma7",
          "humidity3pm",
          "humidity_ma3",
          "humidity_ma7"
        )
      ),
      var2 = factor(
        var2,
        levels = c(
          "rainfall",
          "rainfall_ma3",
          "rainfall_ma7",
          "humidity3pm",
          "humidity_ma3",
          "humidity_ma7"
        )
      )
    )

  ggplot(cor_data, aes(var2, var1, fill = correlation)) +
    geom_tile(colour = "white", linewidth = 0.8) +
    geom_text(
      aes(label = sprintf("%.2f", correlation)),
      size = 3.2,
      colour = "grey15",
      fontface = "plain"
    ) +
    scale_fill_gradient2(
      low = "#2166AC",
      mid = "white",
      high = "#B2182B",
      midpoint = 0,
      limits = c(-1, 1),
      name = "Spearman\nr"
    ) +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    labs(
      title = "Correlations: Raw Variables vs. Moving Averages",
      subtitle = "High within-variable correlations warn of multicollinearity for linear model families.",
      x = NULL,
      y = NULL
    ) +
    feat_theme +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.major = element_blank(),
      legend.position = "right",
      legend.key.height = unit(1.1, "cm"),
      legend.key.width = unit(0.4, "cm")
    )
}
