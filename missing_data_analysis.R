
# Missing Data Analysis Script

# Purpose: Analyze missingness patterns in weather data
# Focus: High-missingness variables and their co-occurrence patterns

# Load required packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(naniar)
library(purrr)
library(glue)

# Define high-missingness variables 
high_miss_cols <- c("sunshine", "evaporation", "cloud3pm", "cloud9am")

high_miss <- df %>% 
  select(all_of(high_miss_cols)) %>% 
  colnames()

cat("High missingness variables:", paste(high_miss, collapse = ", "), "\n\n")

# Analyze co-missingness patterns
cat("Analyzing co-missingness patterns...\n\n")

co_missing_stats <- expand_grid(var1 = high_miss, var2 = high_miss) %>% 
  filter(var1 < var2) %>% 
  rowwise() %>% 
  mutate(
    n_var1_miss = sum(is.na(df[[var1]])),
    n_both_miss = sum(is.na(df[[var1]]) & is.na(df[[var2]])),
    pct_co_miss = if_else(n_var1_miss > 0, (n_both_miss / n_var1_miss) * 100, 0)
  ) %>% 
  ungroup() %>% 
  arrange(desc(pct_co_miss))

# Display missing pattern combinations
cat("Missing pattern combinations:\n")
df %>% 
  select(all_of(high_miss)) %>% 
  miss_case_table() %>% 
  print()

cat("\n")
print(co_missing_stats)

# Print detailed co-missingness summary
cat("\nCo-missingness Summary:\n")
co_missing_stats %>% 
  mutate(
    msg = glue("  {var1} | {var2}: {round(pct_co_miss, 1)}% (when {var1} is missing, {var2} is also missing)")
  ) %>% 
  pull(msg) %>% 
  walk(cat, "\n")

cat("\n\n")

# Analyze missingness by location
cat("Analyzing missingness by location...\n\n")

location_summary <- df %>% 
  group_by(location) %>% 
  miss_var_summary()

location_summary %>% 
  filter(variable %in% high_miss) %>% 
  arrange(desc(pct_miss)) %>% 
  head(10) %>% 
  print()

cat("\n")

# Analyze temporal trends in missingness
cat("Analyzing temporal trends...\n\n")

temporal_trend <- df %>%
  mutate(month = floor_date(date, "month")) %>%
  select(month, any_of(high_miss)) %>%
  pivot_longer(cols = -month, names_to = "variable", values_to = "value") %>%
  group_by(month, variable) %>%
  summarise(pct_missing = mean(is.na(value)) * 100, .groups = "drop")

# Plot temporal trends
p_time <- ggplot(temporal_trend, aes(x = month, y = pct_missing, color = variable)) +
  geom_line(linewidth = 1) +
  facet_wrap(~variable, scales = "free_y", ncol = 1) +
  labs(
    title = "Timeline of Systematic Missingness",
    subtitle = "Notice the jump in missing data starting around 2008-2009",
    y = "Missingness (%)",
    x = "Year"
  ) +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 100))

print(p_time)


if (all(c("rainfall", "sunshine") %in% names(df))) {

  
  # Calculate sunshine missingness by rainfall status
  weather_missing_stats <- df %>%
    mutate(is_rainy = if_else(rainfall > 1, "Rainy (>1mm)", "Dry (≤1mm)")) %>%
    group_by(is_rainy) %>%
    summarise(
      n_obs = n(),
      pct_sunshine_missing = mean(is.na(sunshine)) * 100,
      .groups = "drop"
    )
  
  print(weather_missing_stats)
  cat("\n")
  
  # Plot rainfall density by sunshine missingness
  p_rainfall <- df %>%
    bind_shadow() %>%
    filter(rainfall > 0 & rainfall < 50) %>% 
    ggplot(aes(x = rainfall, fill = sunshine_NA)) +
    geom_density(alpha = 0.6) +
    scale_fill_viridis_d(labels = c("!NA" = "Present", "NA" = "Missing")) +
    labs(
      title = "Does Sunshine go missing on rainy days?",
      subtitle = "Density of rainfall amounts for Missing vs. Present sunshine data",
      x = "Rainfall (mm)",
      y = "Density",
      fill = "Sunshine Status"
    ) +
    theme_minimal()
  
  print(p_rainfall)
}

cat("\nMissing data analysis complete!\n")