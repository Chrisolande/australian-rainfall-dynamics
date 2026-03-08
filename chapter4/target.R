# eda_target.R
# Descriptive statistics and zero-inflation summary for the rainfall target.

rainfall_stats <- df_clean %>%
  summarise(
    n = n(),
    mean = mean(rainfall),
    median = median(rainfall),
    sd = sd(rainfall),
    min = min(rainfall),
    max = max(rainfall),
    q25 = quantile(rainfall, 0.25),
    q75 = quantile(rainfall, 0.75),
    iqr = IQR(rainfall),
    n_zeros = sum(rainfall == 0),
    pct_zeros = mean(rainfall == 0) * 100,
    n_large = sum(rainfall > 100),
    pct_large = mean(rainfall > 100) * 100,
    skewness = moments::skewness(rainfall),
    kurtosis = moments::kurtosis(rainfall)
  )

render_descriptive_stats <- function() {
  rainfall_stats %>%
    pivot_longer(everything(), names_to = "Statistic", values_to = "Value") %>%
    mutate(
      Value = ifelse(
        Value > 1000,
        format(Value, scientific = TRUE, digits = 3),
        round(Value, 3)
      )
    ) %>%
    kable(
      caption = "Descriptive Statistics: Daily Rainfall (mm)",
      align = "lr",
      booktabs = TRUE
    ) %>%
    kable_styling(
      bootstrap_options = c("striped", "hover"),
      full_width = FALSE,
      latex_options = c("hold_position")
    )
}
rain_check <- df_clean %>%
  summarise(
    total_days = n(),
    dry_days = sum(rainfall == 0),
    rainy_days = sum(rainfall > 0),
    zero_inflation_pct = (dry_days / total_days) * 100
  )

render_prevalence_table <- function() {
  rain_check %>%
    kable(
      caption = "Prevalence of Zero-Inflation (Dry Days)",
      col.names = c(
        "Total Days",
        "Dry Days (0 mm)",
        "Rainy Days (>0 mm)",
        "Zero Inflation (%)"
      ),
      booktabs = TRUE
    ) %>%
    kable_styling(
      bootstrap_options = c("striped", "bordered"),
      full_width = FALSE,
      latex_options = c("hold_position")
    )
}
