# eda_interactions.R
# Bivariate density plot justifying the humidity x sunshine interaction term.

plot_rain_corner <- function(dat = df_final) {
  dat %>%
    group_by(location) %>%
    arrange(date) %>%
    mutate(
      rain_index_ref = ifelse(rainfall > 0, row_number(), NA_integer_)
    ) %>%
    fill(rain_index_ref, .direction = "down") %>%
    mutate(days_since_rain = row_number() - lag(rain_index_ref)) %>%
    select(-rain_index_ref) %>%
    mutate(
      rain_label = factor(
        rain_today,
        levels = c("No", "Yes"),
        labels = c("Dry day (rain_today = No)", "Rainy day (rain_today = Yes)")
      )
    ) %>%
    ggplot(aes(sunshine, humidity3pm)) +
    geom_density2d_filled(contour_var = "ndensity", bins = 8) +
    facet_wrap(~rain_label) +
    scale_fill_brewer(palette = "Blues", direction = 1) +
    scale_x_continuous(
      expand = expansion(mult = c(0.02, 0.02)),
      breaks = pretty_breaks(n = 6)
    ) +
    scale_y_continuous(
      expand = expansion(mult = c(0.02, 0.02)),
      breaks = pretty_breaks(n = 6)
    ) +
    labs(
      title = "Justifying an Interaction Term: The 'Rain Corner'",
      subtitle = paste0(
        "Rain concentrates where high humidity and low sunshine coincide simultaneously.\n",
        "Dry days are dispersed broadly, confirming that neither condition alone is sufficient."
      ),
      x = "Sunshine (hours)",
      y = "Humidity 3 PM (%)"
    ) +
    eda_theme +
    theme(
      legend.position = "none",
      strip.text = element_text(size = 10, face = "bold")
    )
}
