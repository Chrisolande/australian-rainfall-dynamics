#!/usr/bin/env Rscript
################################################################################
# Comprehensive Missing Data Diagnostics & Sensitivity Analysis
# Purpose: Analyze missingness patterns before imputation to determine if data
#          should be imputed or dropped based on missingness mechanism
# Author: Chris Olande
# Date: 2026-02-03
################################################################################

# Load required libraries
suppressPackageStartupMessages({
  library(tidyverse)
  library(janitor)
  library(naniar)      # Modern missing data analysis
  library(finalfit)    # Missing data patterns
  library(corrr)       # Tidy correlation analysis
})

# Set output options
options(width = 100)

################################################################################
# SECTION 1: DATA LOADING AND PREPARATION
################################################################################

cat("================================================================================\n")
cat("COMPREHENSIVE MISSING DATA DIAGNOSTICS & SENSITIVITY ANALYSIS\n")
cat("================================================================================\n\n")

# Load the raw data
cat("Loading data from weatherAUS.csv...\n")
df <- read_csv("data/weatherAUS.csv", show_col_types = FALSE) %>%
  clean_names() %>%
  filter(!is.na(rainfall))  # Keep only observations with rainfall data

cat(sprintf("Dataset loaded: %d observations, %d variables\n\n", nrow(df), ncol(df)))

################################################################################
# SECTION 2: CO-OCCURRENCE ANALYSIS
################################################################################

cat("================================================================================\n")
cat("REQUIREMENT 1: CO-OCCURRENCE ANALYSIS\n")
cat("================================================================================\n")
cat("Goal: Determine if instruments fail simultaneously\n")
cat("Threshold: Flag pairs where P(A missing | B missing) > 90%\n\n")

# Use naniar to identify columns with missing values
cols_with_missing <- df %>%
  miss_var_summary() %>%
  filter(n_miss > 0) %>%
  pull(variable)

# Calculate co-occurrence probabilities using tidyverse approach
# Create all variable pairs
co_occurrence_df <- expand_grid(
  Variable_A = cols_with_missing,
  Variable_B = cols_with_missing
) %>%
  filter(Variable_A < Variable_B) %>%  # Only unique pairs
  rowwise() %>%
  mutate(
    # Use naniar functions for missing data operations
    Both_Missing = sum(is.na(df[[Variable_A]]) & is.na(df[[Variable_B]])),
    A_Missing_Total = sum(is.na(df[[Variable_A]])),
    B_Missing_Total = sum(is.na(df[[Variable_B]])),
    P_A_Given_B = if_else(B_Missing_Total > 0, Both_Missing / B_Missing_Total, 0),
    P_B_Given_A = if_else(A_Missing_Total > 0, Both_Missing / A_Missing_Total, 0),
    Max_Conditional_Prob = max(P_A_Given_B, P_B_Given_A)
  ) %>%
  ungroup() %>%
  arrange(desc(Max_Conditional_Prob))

# Display top co-occurring pairs
cat("Top 15 Co-Occurring Missingness Pairs:\n")
cat("--------------------------------------\n")
print(co_occurrence_df %>% 
  head(15) %>%
  mutate(across(where(is.numeric), ~round(., 3))) %>%
  select(Variable_A, Variable_B, P_A_Given_B, P_B_Given_A, Max_Conditional_Prob),
  row.names = FALSE)

# Flag pairs exceeding 90% threshold
flagged_pairs <- co_occurrence_df %>%
  filter(Max_Conditional_Prob > 0.90)

cat("\n")
if (nrow(flagged_pairs) > 0) {
  cat(sprintf("⚠️  WARNING: %d variable pairs exceed 90%% co-missing threshold!\n", nrow(flagged_pairs)))
  cat("These pairs likely share the same measurement instrument:\n")
  print(flagged_pairs %>%
    select(Variable_A, Variable_B, Max_Conditional_Prob) %>%
    mutate(Max_Conditional_Prob = round(Max_Conditional_Prob * 100, 1)) %>%
    rename(`Co-Missing %` = Max_Conditional_Prob),
    row.names = FALSE)
  cat("\n")
} else {
  cat("✓ No variable pairs exceed 90% co-missing threshold.\n\n")
}

################################################################################
# SECTION 3: LOCATION-BASED STRATIFICATION
################################################################################

cat("================================================================================\n")
cat("REQUIREMENT 2: LOCATION-BASED STRATIFICATION\n")
cat("================================================================================\n")
cat("Goal: Identify if missingness is systematic to specific stations\n")
cat("Threshold: If Range > 50%, imputation must be location-aware\n\n")

# Calculate missingness rate by location for key variables using naniar
key_vars <- c("sunshine", "evaporation", "cloud9am", "cloud3pm")

# Use naniar's miss_var_summary with grouping for cleaner approach
location_missingness <- df %>%
  group_by(location) %>%
  miss_var_summary() %>%
  filter(variable %in% key_vars) %>%
  mutate(missing_pct = pct_miss) %>%
  select(location, variable, missing_pct, n_miss) %>%
  ungroup()

# Pivot to wide format for easier viewing
location_wide <- location_missingness %>%
  pivot_wider(
    names_from = variable,
    values_from = missing_pct,
    names_prefix = ""
  )

# Calculate statistics across locations using tidyverse
location_stats <- location_missingness %>%
  group_by(variable) %>%
  summarise(
    Min_Pct = min(missing_pct, na.rm = TRUE),
    Max_Pct = max(missing_pct, na.rm = TRUE),
    Mean_Pct = mean(missing_pct, na.rm = TRUE),
    SD_Pct = sd(missing_pct, na.rm = TRUE),
    Range_Pct = Max_Pct - Min_Pct,
    .groups = "drop"
  ) %>%
  mutate(across(where(is.numeric), ~round(., 1))) %>%
  rename(Variable = variable)

cat("Location-Level Missingness Statistics:\n")
cat("--------------------------------------\n")
print(location_stats, row.names = FALSE)
cat("\n")

# Check threshold
high_variance_vars <- location_stats %>%
  filter(Range_Pct > 50)

if (nrow(high_variance_vars) > 0) {
  cat(sprintf("⚠️  WARNING: %d variable(s) show high variance (Range > 50%%) across locations!\n", 
              nrow(high_variance_vars)))
  cat("Variables with location-dependent missingness:\n")
  print(high_variance_vars %>% select(Variable, Range_Pct), row.names = FALSE)
  cat("\n➡️  RECOMMENDATION: Use location-stratified or location-aware imputation.\n\n")
} else {
  cat("✓ Missingness variance across locations is within acceptable range.\n\n")
}

# Show top and bottom locations for sunshine (most problematic variable)
sunshine_by_location <- location_missingness %>%
  filter(variable == "sunshine") %>%
  arrange(desc(missing_pct))

cat("Top 5 Locations with Highest Sunshine Missingness:\n")
print(sunshine_by_location %>%
  select(location, missing_pct, n_miss) %>%
  head(5),
  row.names = FALSE)

cat("\nTop 5 Locations with Lowest Sunshine Missingness:\n")
print(sunshine_by_location %>%
  select(location, missing_pct, n_miss) %>%
  arrange(missing_pct) %>%
  head(5),
  row.names = FALSE)
cat("\n")

################################################################################
# SECTION 4: TEMPORAL & WEATHER DEPENDENCY (MAR TESTING)
################################################################################

cat("================================================================================\n")
cat("REQUIREMENT 3: TEMPORAL & WEATHER DEPENDENCY (MAR TESTING)\n")
cat("================================================================================\n")
cat("Goal: Test if data is Missing At Random (MAR) or Missing Not At Random (MNAR)\n\n")

# Add year column
df <- df %>%
  mutate(year = lubridate::year(date))

# 4.1: Temporal Drift Analysis
cat("3.1: TEMPORAL DRIFT ANALYSIS\n")
cat("-----------------------------\n")

# Use naniar for temporal missingness aggregation
temporal_missingness <- df %>%
  group_by(year) %>%
  miss_var_summary() %>%
  filter(variable %in% key_vars) %>%
  select(year, variable, pct_miss, n_miss) %>%
  pivot_wider(
    names_from = variable,
    values_from = pct_miss,
    id_cols = year
  ) %>%
  ungroup()

cat("Missingness Rate by Year:\n")
print(temporal_missingness %>%
  mutate(across(where(is.numeric) & !year, ~round(., 1))),
  row.names = FALSE)
cat("\n")

# Correlation with year using tidy approach
for (var in key_vars) {
  # Create missing indicator (0 = present, 1 = missing)
  missing_indicator <- as.numeric(is.na(df[[var]]))
  
  # Use Spearman correlation for ordinal relationship
  cor_test <- cor.test(df$year, missing_indicator, method = "spearman")
  
  cat(sprintf("Correlation between %s missingness and year:\n", var))
  cat(sprintf("  Spearman's rho = %.4f, p-value = %.4e\n", 
              cor_test$estimate, cor_test$p.value))
  
  if (cor_test$p.value < 0.05) {
    if (abs(cor_test$estimate) > 0.1) {
      cat("  ⚠️  Significant temporal drift detected!\n")
    } else {
      cat("  ✓ Statistically significant but weak correlation.\n")
    }
  } else {
    cat("  ✓ No significant temporal drift.\n")
  }
  cat("\n")
}

# 4.2: Weather Dependency Analysis
cat("3.2: WEATHER DEPENDENCY ANALYSIS\n")
cat("--------------------------------\n")
cat("Testing if missingness correlates with observed weather conditions.\n\n")

# Create rainfall categories using tidyverse
df <- df %>%
  mutate(
    rainfall_category = case_when(
      rainfall == 0 ~ "Dry",
      rainfall > 0 & rainfall <= 5 ~ "Light",
      rainfall > 5 ~ "Heavy"
    ),
    rainfall_category = factor(rainfall_category, levels = c("Dry", "Light", "Heavy"))
  )

# Chi-square test: Is sunshine missingness related to rainfall?
cat("Chi-Square Test: Sunshine Missingness vs Rainfall Category\n")

# Use tidyverse approach for contingency table and chi-square
chi_data <- df %>%
  select(sunshine, rainfall_category) %>%
  mutate(sunshine_missing = is.na(sunshine))

chi_test <- chisq.test(chi_data$sunshine_missing, chi_data$rainfall_category)
cat(sprintf("  Chi-square = %.2f, df = %d, p-value = %.4e\n", 
            chi_test$statistic, chi_test$parameter, chi_test$p.value))

if (chi_test$p.value < 0.05) {
  cat("  ⚠️  Sunshine missingness is significantly correlated with rainfall!\n")
  cat("  This suggests Missing At Random (MAR) mechanism.\n")
} else {
  cat("  ✓ No significant correlation between sunshine missingness and rainfall.\n")
}
cat("\n")

# Show missingness rates by rainfall category using naniar
missingness_by_rainfall <- df %>%
  group_by(rainfall_category) %>%
  miss_var_summary() %>%
  filter(variable %in% c("sunshine", "evaporation")) %>%
  select(rainfall_category, variable, pct_miss, n_miss) %>%
  pivot_wider(
    names_from = variable,
    values_from = pct_miss
  ) %>%
  ungroup()

cat("Missingness Rates by Rainfall Category:\n")
print(missingness_by_rainfall %>%
  mutate(across(where(is.numeric), ~round(., 1))),
  row.names = FALSE)
cat("\n")

################################################################################
# SECTION 5: SENSITIVITY SCENARIOS
################################################################################

cat("================================================================================\n")
cat("REQUIREMENT 4: SENSITIVITY SCENARIOS\n")
cat("================================================================================\n")
cat("Goal: Quantify the cost of dropping data vs. imputing it\n\n")

# Calculate overall missingness using naniar
overall_missingness <- df %>%
  miss_var_summary()

# Scenario 1: Baseline - Keep all variables
n_baseline <- nrow(df)
n_vars_baseline <- ncol(df)
n_complete_baseline <- df %>% n_complete()

# Scenario 2: Conservative - Drop sunshine, evaporation, and cloud
df_conservative <- df %>%
  select(-sunshine, -evaporation, -cloud9am, -cloud3pm)
n_complete_conservative <- df_conservative %>% n_complete()
n_vars_conservative <- ncol(df_conservative)

# Scenario 3: Strict - Drop any column with >10% missingness
high_missing_cols <- overall_missingness %>%
  filter(pct_miss > 10) %>%
  pull(variable)

df_strict <- df %>%
  select(-any_of(high_missing_cols))
n_complete_strict <- df_strict %>% n_complete()
n_vars_strict <- ncol(df_strict)

# Create comparison table using tribble for clarity
scenario_comparison <- tribble(
  ~Scenario, ~Variables_Retained, ~Total_Observations, ~Complete_Cases, ~Data_Retention_Pct, ~Strategy,
  "Baseline (Keep All)", n_vars_baseline, n_baseline, n_complete_baseline, round(n_complete_baseline / n_baseline * 100, 1), "Requires imputation",
  "Conservative (Drop Sunshine/Evap/Cloud)", n_vars_conservative, n_baseline, n_complete_conservative, round(n_complete_conservative / n_baseline * 100, 1), "Moderate imputation needed",
  "Strict (Drop >10% Missing)", n_vars_strict, n_baseline, n_complete_strict, round(n_complete_strict / n_baseline * 100, 1), "Minimal imputation needed"
)

cat("Data Retention Comparison:\n")
cat("--------------------------\n")
print(scenario_comparison, row.names = FALSE)
cat("\n")

# Variables dropped in each scenario
cat("Variables Dropped in Conservative Scenario:\n")
cat(paste("  -", c("sunshine", "evaporation", "cloud9am", "cloud3pm"), collapse = "\n"))
cat("\n\n")

cat("Variables Dropped in Strict Scenario (>10% missing):\n")
cat(paste("  -", high_missing_cols, collapse = "\n"))
cat("\n\n")

################################################################################
# SECTION 6: FINAL RECOMMENDATION
################################################################################

cat("================================================================================\n")
cat("FINAL RECOMMENDATION\n")
cat("================================================================================\n\n")

# Determine recommendation based on findings
has_high_cooccurrence <- nrow(flagged_pairs) > 0
has_location_variance <- nrow(high_variance_vars) > 0

# Check temporal drift using simple missing indicators
has_temporal_drift <- any(sapply(key_vars, function(var) {
  missing_indicator <- as.numeric(is.na(df[[var]]))
  cor_test <- suppressWarnings(cor.test(
    df$year, 
    missing_indicator,
    method = "spearman"
  ))
  cor_test$p.value < 0.05 && abs(cor_test$estimate) > 0.1
}))

has_weather_dependency <- chi_test$p.value < 0.05

cat("DECISION CRITERIA:\n")
cat("------------------\n")
cat(sprintf("1. High Co-Occurrence (>90%%): %s\n", 
            ifelse(has_high_cooccurrence, "YES ⚠️", "NO ✓")))
cat(sprintf("2. High Location Variance (Range >50%%): %s\n", 
            ifelse(has_location_variance, "YES ⚠️", "NO ✓")))
cat(sprintf("3. Significant Temporal Drift: %s\n", 
            ifelse(has_temporal_drift, "YES ⚠️", "NO ✓")))
cat(sprintf("4. Weather Dependency (MAR indicator): %s\n", 
            ifelse(has_weather_dependency, "YES (MAR likely)", "NO")))
cat("\n")

cat("INTERPRETATION:\n")
cat("---------------\n")

if (has_weather_dependency) {
  cat("• Missingness appears to be Missing At Random (MAR)\n")
  cat("  → Sensors may fail preferentially during certain weather conditions\n")
} else {
  cat("• Missingness pattern suggests MCAR or MNAR\n")
}

if (has_location_variance) {
  cat("• High location-based variance indicates systematic station-level differences\n")
  cat("  → Some stations lack certain instruments entirely\n")
}

if (has_high_cooccurrence) {
  cat("• High co-occurrence suggests shared measurement systems\n")
  cat("  → Variables likely measured by same instrument or station\n")
}

cat("\n")
cat("FINAL DECISION:\n")
cat("===============\n")

# Make recommendation
if (has_location_variance && any(location_stats$Range_Pct > 80)) {
  cat("🔴 RECOMMENDATION: **DROP COLUMNS** with severe location-dependent missingness\n\n")
  cat("REASONING:\n")
  cat("• Extreme variance across locations (Range >80%) indicates systematic\n")
  cat("  instrument absence at many stations\n")
  cat("• Imputation would introduce artificial patterns not grounded in reality\n")
  cat("• Better to model with fewer but more reliable variables\n\n")
  cat("SUGGESTED ACTION:\n")
  cat("→ Drop: sunshine, evaporation, cloud9am, cloud3pm\n")
  cat("→ Retain: temperature, pressure, humidity, wind (low missingness)\n")
} else if (has_weather_dependency || has_high_cooccurrence) {
  cat("🟡 RECOMMENDATION: **IMPUTE with CAUTION** using location-aware strategy\n\n")
  cat("REASONING:\n")
  cat("• Evidence of MAR mechanism (weather dependency)\n")
  cat("• Location-based variance requires stratified approach\n")
  cat("• Imputation feasible but must account for spatial structure\n\n")
  cat("SUGGESTED ACTION:\n")
  cat("→ Use location-stratified Random Forest imputation (missRanger)\n")
  cat("→ Include location as predictor in imputation model\n")
  cat("→ Create 'imputation flags' to control for uncertainty in downstream models\n")
  cat("→ Perform sensitivity analysis comparing results with/without imputed vars\n")
} else {
  cat("🟢 RECOMMENDATION: **SAFE TO IMPUTE** using standard methods\n\n")
  cat("REASONING:\n")
  cat("• No strong evidence of MNAR or systematic bias\n")
  cat("• Missingness appears random or weakly structured\n")
  cat("• Standard imputation methods (Random Forest, PMM) should perform well\n\n")
  cat("SUGGESTED ACTION:\n")
  cat("→ Use missRanger with PMM for robust imputation\n")
  cat("→ Standard approach as currently implemented is appropriate\n")
}

cat("\n")
cat("================================================================================\n")
cat("END OF DIAGNOSTIC REPORT\n")
cat("================================================================================\n")
cat("\nReport generated on:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
