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
  library(kableExtra)
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

# Identify columns with missing values
cols_with_missing <- names(df)[colSums(is.na(df)) > 0]

# Calculate co-occurrence probabilities
co_occurrence_results <- list()

for (i in 1:(length(cols_with_missing) - 1)) {
  for (j in (i + 1):length(cols_with_missing)) {
    var_a <- cols_with_missing[i]
    var_b <- cols_with_missing[j]
    
    # Count observations where both are missing
    both_missing <- sum(is.na(df[[var_a]]) & is.na(df[[var_b]]))
    
    # Count observations where var_b is missing
    b_missing <- sum(is.na(df[[var_b]]))
    
    # Count observations where var_a is missing
    a_missing <- sum(is.na(df[[var_a]]))
    
    # Calculate conditional probabilities
    # P(A missing | B missing)
    prob_a_given_b <- ifelse(b_missing > 0, both_missing / b_missing, 0)
    
    # P(B missing | A missing)
    prob_b_given_a <- ifelse(a_missing > 0, both_missing / a_missing, 0)
    
    # Store results
    co_occurrence_results[[length(co_occurrence_results) + 1]] <- data.frame(
      Variable_A = var_a,
      Variable_B = var_b,
      Both_Missing = both_missing,
      A_Missing_Total = a_missing,
      B_Missing_Total = b_missing,
      P_A_Given_B = prob_a_given_b,
      P_B_Given_A = prob_b_given_a,
      Max_Conditional_Prob = max(prob_a_given_b, prob_b_given_a)
    )
  }
}

# Combine results
co_occurrence_df <- bind_rows(co_occurrence_results) %>%
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

# Calculate missingness rate by location for key variables
key_vars <- c("sunshine", "evaporation", "cloud9am", "cloud3pm")

location_missingness <- df %>%
  group_by(location) %>%
  summarise(
    n_obs = n(),
    sunshine_missing_pct = mean(is.na(sunshine)) * 100,
    evaporation_missing_pct = mean(is.na(evaporation)) * 100,
    cloud9am_missing_pct = mean(is.na(cloud9am)) * 100,
    cloud3pm_missing_pct = mean(is.na(cloud3pm)) * 100
  ) %>%
  arrange(desc(sunshine_missing_pct))

# Calculate statistics across locations
location_stats <- data.frame(
  Variable = key_vars,
  Min_Pct = sapply(key_vars, function(v) {
    min(location_missingness[[paste0(v, "_missing_pct")]], na.rm = TRUE)
  }),
  Max_Pct = sapply(key_vars, function(v) {
    max(location_missingness[[paste0(v, "_missing_pct")]], na.rm = TRUE)
  }),
  Mean_Pct = sapply(key_vars, function(v) {
    mean(location_missingness[[paste0(v, "_missing_pct")]], na.rm = TRUE)
  }),
  SD_Pct = sapply(key_vars, function(v) {
    sd(location_missingness[[paste0(v, "_missing_pct")]], na.rm = TRUE)
  }),
  Range_Pct = sapply(key_vars, function(v) {
    max(location_missingness[[paste0(v, "_missing_pct")]], na.rm = TRUE) -
    min(location_missingness[[paste0(v, "_missing_pct")]], na.rm = TRUE)
  })
) %>%
  mutate(across(where(is.numeric), ~round(., 1)))

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
cat("Top 5 Locations with Highest Sunshine Missingness:\n")
print(location_missingness %>%
  select(location, n_obs, sunshine_missing_pct) %>%
  head(5),
  row.names = FALSE)

cat("\nTop 5 Locations with Lowest Sunshine Missingness:\n")
print(location_missingness %>%
  select(location, n_obs, sunshine_missing_pct) %>%
  arrange(sunshine_missing_pct) %>%
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

temporal_missingness <- df %>%
  group_by(year) %>%
  summarise(
    sunshine_missing_pct = mean(is.na(sunshine)) * 100,
    evaporation_missing_pct = mean(is.na(evaporation)) * 100,
    cloud9am_missing_pct = mean(is.na(cloud9am)) * 100,
    cloud3pm_missing_pct = mean(is.na(cloud3pm)) * 100,
    n_obs = n()
  )

cat("Missingness Rate by Year:\n")
print(temporal_missingness %>%
  mutate(across(where(is.numeric) & !n_obs, ~round(., 1))),
  row.names = FALSE)
cat("\n")

# Correlation with year
for (var in key_vars) {
  missing_indicator <- as.numeric(is.na(df[[var]]))
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

# Create rainfall categories
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

# Create contingency table
sunshine_rainfall_table <- table(
  Sunshine_Missing = is.na(df$sunshine),
  Rainfall = df$rainfall_category
)

chi_test <- chisq.test(sunshine_rainfall_table)
cat(sprintf("  Chi-square = %.2f, df = %d, p-value = %.4e\n", 
            chi_test$statistic, chi_test$parameter, chi_test$p.value))

if (chi_test$p.value < 0.05) {
  cat("  ⚠️  Sunshine missingness is significantly correlated with rainfall!\n")
  cat("  This suggests Missing At Random (MAR) mechanism.\n")
} else {
  cat("  ✓ No significant correlation between sunshine missingness and rainfall.\n")
}
cat("\n")

# Show missingness rates by rainfall category
missingness_by_rainfall <- df %>%
  group_by(rainfall_category) %>%
  summarise(
    n = n(),
    sunshine_missing_pct = mean(is.na(sunshine)) * 100,
    evaporation_missing_pct = mean(is.na(evaporation)) * 100
  ) %>%
  mutate(across(where(is.numeric) & !n, ~round(., 1)))

cat("Missingness Rates by Rainfall Category:\n")
print(missingness_by_rainfall, row.names = FALSE)
cat("\n")

################################################################################
# SECTION 5: SENSITIVITY SCENARIOS
################################################################################

cat("================================================================================\n")
cat("REQUIREMENT 4: SENSITIVITY SCENARIOS\n")
cat("================================================================================\n")
cat("Goal: Quantify the cost of dropping data vs. imputing it\n\n")

# Calculate overall missingness
overall_missingness <- df %>%
  summarise(across(everything(), ~mean(is.na(.)) * 100)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "missing_pct")

# Scenario 1: Baseline - Keep all variables
n_baseline <- nrow(df)
n_vars_baseline <- ncol(df)

# Scenario 2: Conservative - Drop sunshine, evaporation, and cloud
df_conservative <- df %>%
  select(-sunshine, -evaporation, -cloud9am, -cloud3pm)
n_complete_conservative <- sum(complete.cases(df_conservative))
n_vars_conservative <- ncol(df_conservative)

# Scenario 3: Strict - Drop any column with >10% missingness
high_missing_cols <- overall_missingness %>%
  filter(missing_pct > 10) %>%
  pull(variable)

df_strict <- df %>%
  select(-any_of(high_missing_cols))
n_complete_strict <- sum(complete.cases(df_strict))
n_vars_strict <- ncol(df_strict)

# Create comparison table
scenario_comparison <- data.frame(
  Scenario = c("Baseline (Keep All)", 
               "Conservative (Drop Sunshine/Evap/Cloud)",
               "Strict (Drop >10% Missing)"),
  Variables_Retained = c(n_vars_baseline, n_vars_conservative, n_vars_strict),
  Total_Observations = c(n_baseline, n_baseline, n_baseline),
  Complete_Cases = c(sum(complete.cases(df)), 
                     n_complete_conservative, 
                     n_complete_strict),
  Data_Retention_Pct = round(c(
    sum(complete.cases(df)) / n_baseline * 100,
    n_complete_conservative / n_baseline * 100,
    n_complete_strict / n_baseline * 100
  ), 1),
  Strategy = c("Requires imputation",
               "Moderate imputation needed",
               "Minimal imputation needed")
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
has_temporal_drift <- any(sapply(key_vars, function(var) {
  cor_test <- suppressWarnings(cor.test(df$year, as.numeric(is.na(df[[var]])), 
                                        method = "spearman"))
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
