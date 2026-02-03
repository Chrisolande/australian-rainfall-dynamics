# Missing Data Diagnostics & Sensitivity Analysis

## Overview

The `diagnose_missingness.R` script provides a comprehensive analysis of missing data patterns in the Australian weather dataset before imputation. This diagnostic tool helps determine whether data should be **imputed** or **dropped** based on the underlying missingness mechanism.

## Purpose

The current `clean_and_impute_weather` function applies blanket `missRanger` imputation. However, preliminary analysis shows severe missingness in:

- **Sunshine:** ~47.7%
- **Evaporation:** ~42.5%
- **Cloud cover:** ~37-40%

Blindly imputing nearly half the dataset without understanding the **mechanism of missingness** (MCAR, MAR, or MNAR) risks introducing significant bias.

## Usage

### Prerequisites

Ensure you have the following R packages installed:

```r
install.packages(c("tidyverse", "janitor", "kableExtra"))
```

### Running the Script

From the repository root directory:

```bash
Rscript diagnose_missingness.R
```

Or from within R:

```r
source("diagnose_missingness.R")
```

### Output

The script generates a comprehensive text-based report to the console covering:

1. **Co-occurrence Analysis**
2. **Location-Based Stratification**
3. **Temporal & Weather Dependency (MAR Testing)**
4. **Sensitivity Scenarios**
5. **Final Recommendation**

## What the Script Does

### 1. Co-Occurrence Analysis

**Goal:** Determine if instruments fail simultaneously.

- Calculates conditional probability of variable A being missing given that variable B is missing
- Flags any pair where co-missing probability exceeds **90%**
- Indicates shared measurement systems or instruments

### 2. Location-Based Stratification

**Goal:** Identify if missingness is systematic to specific stations.

- Groups missingness rates by location
- Calculates variance statistics (Min, Max, Mean, SD, Range)
- Flags variables where Range > **50%** (indicating location-dependent patterns)
- Shows top/bottom stations for problematic variables

### 3. Temporal & Weather Dependency (MAR Testing)

**Goal:** Test if data is Missing At Random (MAR) or Missing Not At Random (MNAR).

**3.1 Temporal Drift Analysis:**
- Correlates missingness rates with year using Spearman's rank correlation
- Identifies if data collection improved/deteriorated over time

**3.2 Weather Dependency Analysis:**
- Performs Chi-square test to determine if missingness correlates with weather conditions
- Tests hypothesis: "Do sensors fail when it rains?"
- Significant correlation suggests MAR mechanism

### 4. Sensitivity Scenarios

**Goal:** Quantify the cost of dropping data vs. imputing it.

Compares data retention under three strategies:

| Scenario | Description | Variables |
|----------|-------------|-----------|
| **Baseline** | Keep all variables | All columns retained |
| **Conservative** | Drop high-missingness vars | Drop sunshine, evaporation, cloud9am, cloud3pm |
| **Strict** | Drop any column >10% missing | Drop all highly incomplete columns |

Shows:
- Number of variables retained
- Number of complete cases
- Data retention percentage

## Interpreting the Results

### Decision Logic

The script provides a recommendation based on four criteria:

1. **High Co-Occurrence (>90%):** Suggests shared instruments
2. **High Location Variance (Range >50%):** Indicates systematic station differences
3. **Significant Temporal Drift:** Suggests changing data quality over time
4. **Weather Dependency:** Indicates MAR (Missing At Random) mechanism

### Possible Recommendations

#### 🔴 DROP COLUMNS
**When:** Extreme location variance (Range >80%) indicates many stations completely lack certain instruments.

**Action:**
- Remove: sunshine, evaporation, cloud9am, cloud3pm
- Retain: temperature, pressure, humidity, wind variables

#### 🟡 IMPUTE WITH CAUTION
**When:** Evidence of MAR mechanism or high co-occurrence with moderate location variance.

**Action:**
- Use location-stratified Random Forest imputation
- Include location as predictor
- Create imputation flags for downstream models
- Perform sensitivity analysis

#### 🟢 SAFE TO IMPUTE
**When:** No strong evidence of MNAR or systematic bias.

**Action:**
- Standard missRanger with PMM is appropriate
- Current implementation is acceptable

## Example Output

```
================================================================================
REQUIREMENT 1: CO-OCCURRENCE ANALYSIS
================================================================================
⚠️  WARNING: 3 variable pairs exceed 90% co-missing threshold!
These pairs likely share the same measurement instrument:
  Variable_A  Variable_B  Max_Conditional_Prob
  cloud9am    cloud3pm    92.3
  sunshine    evaporation 88.7
  ...

================================================================================
REQUIREMENT 2: LOCATION-BASED STRATIFICATION
================================================================================
⚠️  WARNING: 2 variable(s) show high variance (Range > 50%) across locations!
Variables with location-dependent missingness:
  Variable     Range_Pct
  sunshine     85.2
  evaporation  78.9

================================================================================
FINAL RECOMMENDATION
================================================================================
🔴 RECOMMENDATION: **DROP COLUMNS** with severe location-dependent missingness

REASONING:
• Extreme variance across locations (Range >80%) indicates systematic
  instrument absence at many stations
• Imputation would introduce artificial patterns not grounded in reality
```

## Integration with Main Analysis

After running this diagnostic:

1. **Review the recommendation** in the final section
2. **If dropping columns:** Update `clean_and_impute_weather()` function to remove problematic variables
3. **If imputing:** Ensure location-based stratification is included in imputation model
4. **If safe:** Proceed with current imputation strategy

## Technical Details

### Missingness Mechanisms

- **MCAR (Missing Completely At Random):** Missingness unrelated to any variables
- **MAR (Missing At Random):** Missingness depends on observed data
- **MNAR (Missing Not At Random):** Missingness depends on unobserved data

### Statistical Tests

- **Spearman Correlation:** Non-parametric test for temporal trends
- **Chi-Square Test:** Tests independence between missingness and weather conditions
- **Conditional Probability:** P(A missing | B missing) measures co-occurrence

## References

- Little, R. J., & Rubin, D. B. (2019). *Statistical Analysis with Missing Data* (3rd ed.)
- van Buuren, S. (2018). *Flexible Imputation of Missing Data* (2nd ed.)
- Schafer, J. L., & Graham, J. W. (2002). Missing data: Our view of the state of the art. *Psychological Methods*, 7(2), 147-177.

## Author

**Chris Olande**  
Statistician & Programmer  
Date: 2026-02-03

---

*This diagnostic tool was created in response to Issue: "Implement Comprehensive Missing Data Diagnostics & Sensitivity Analysis"*
