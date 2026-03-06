# eval_random_effects.R
# Spatial heterogeneity via random intercepts:
#  plot_location_effects() - dot-and-CI plot of pooled random intercepts

plot_location_effects <- function(model = m6_mixed) {
  ranef_data <- model$good %>%
    map(\(r) as.data.frame(ranef(r$fit)$cond$location)) %>%
    reduce(`+`) /
    length(model$good)

  loc_effects <- as.data.frame(ranef_data) %>%
    rownames_to_column("Location") %>%
    rename(Effect = `(Intercept)`) %>%
    arrange(Effect) %>%
    mutate(
      Location = factor(Location, levels = Location),
      CI_lower = Effect - 1.96 * sd(Effect) / sqrt(n()),
      CI_upper = Effect + 1.96 * sd(Effect) / sqrt(n()),
      Category = case_when(
        Effect > sd(Effect) ~ "Significantly Wetter",
        Effect < -sd(Effect) ~ "Significantly Drier",
        TRUE ~ "Near Average"
      ),
      Category = factor(
        Category,
        levels = c(
          "Significantly Drier",
          "Near Average",
          "Significantly Wetter"
        )
      )
    )

  ggplot(loc_effects, aes(x = Effect, y = Location)) +
    annotate(
      "rect",
      xmin = -Inf,
      xmax = -sd(loc_effects$Effect),
      ymin = -Inf,
      ymax = Inf,
      fill = "#c0392b",
      alpha = 0.05
    ) +
    annotate(
      "rect",
      xmin = sd(loc_effects$Effect),
      xmax = Inf,
      ymin = -Inf,
      ymax = Inf,
      fill = "#2980b9",
      alpha = 0.05
    ) +
    geom_vline(
      xintercept = 0,
      linetype = "dashed",
      color = "grey30",
      linewidth = 0.8
    ) +
    geom_vline(
      xintercept = c(-sd(loc_effects$Effect), sd(loc_effects$Effect)),
      linetype = "dotted",
      color = "grey50",
      linewidth = 0.5
    ) +
    geom_segment(
      aes(
        x = CI_lower,
        xend = CI_upper,
        y = Location,
        yend = Location,
        color = Category
      ),
      linewidth = 1.5,
      alpha = 0.4
    ) +
    geom_point(aes(color = Category), size = 4, alpha = 0.9) +
    geom_text(
      data = filter(loc_effects, abs(Effect) > sd(Effect)),
      aes(
        label = sprintf("%.2f", Effect),
        x = Effect,
        hjust = ifelse(Effect > 0, -0.3, 1.3)
      ),
      size = 3,
      fontface = "bold",
      color = "grey20"
    ) +
    scale_color_manual(
      values = c(
        "Significantly Drier" = "#c0392b",
        "Near Average" = "#7f8c8d",
        "Significantly Wetter" = "#2980b9"
      ),
      name = "Effect Size"
    ) +
    labs(
      title = "The Geography of Rain: Location-Specific Baselines",
      subtitle = "Random intercepts show how much wetter or drier each city is, holding all weather variables constant.\nBars show 95% confidence intervals; points beyond \u00B11 SD are labeled.",
      x = "Baseline Rainfall Adjustment (Log mm)",
      y = NULL,
      caption = "Interpretation: A value of +0.5 means approximately 65% more rain than an average location with identical conditions [exp(0.5) \u2248 1.65]"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "grey90", linewidth = 0.3),
      plot.title = element_text(
        face = "bold",
        size = 16,
        margin = margin(b = 5)
      ),
      plot.subtitle = element_text(
        color = "grey30",
        size = 11,
        margin = margin(b = 15)
      ),
      plot.caption = element_text(
        color = "grey50",
        size = 9,
        hjust = 0,
        margin = margin(t = 10)
      ),
      axis.text.y = element_text(size = 10, face = "bold"),
      axis.text.x = element_text(size = 10),
      axis.title.x = element_text(
        size = 11,
        face = "bold",
        margin = margin(t = 10)
      ),
      legend.position = "top",
      legend.justification = "left",
      legend.title = element_text(face = "bold", size = 10),
      legend.text = element_text(size = 9),
      plot.margin = margin(15, 15, 15, 15)
    )
}

