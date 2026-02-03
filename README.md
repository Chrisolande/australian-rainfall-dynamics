# **Predicting the Unpredictable: Australian Rainfall Dynamics**

![R Version](https://img.shields.io/badge/R-4.4.x-blue?logo=R)
![Quarto](https://img.shields.io/badge/Quarto-1.5+-blue?logo=quarto)
![Statistician](https://img.shields.io/badge/Role-Statistician-orange)
![Methodology](https://img.shields.io/badge/Method-Hurdle%20Gamma%20GLMM-red)
![License](https://img.shields.io/badge/License-MIT-green)

This repository contains a rigorous frequentist analysis of daily rainfall patterns across Australia. Utilizing a **Hurdle Gamma Generalized Linear Mixed Model (Hurdle Gamma GLMM)**, this work decomposes the stochastic nature of precipitation into two distinct physical regimes: the Bernoulli process of occurrence and the truncated Gamma process of intensity conditional on occurrence.

## 🔬 **The Statistical Challenge**

Australian rainfall violates the fundamental assumptions of Gaussian linear theory. With approximately **64% structural zeros** (dry days), extreme right-skewness, and a leptokurtic distribution (Kurtosis ), standard OLS models fail by predicting impossible negative values and underestimating tail-end variance.

This project implements a joint-likelihood framework to address these irregularities across **142,000+ observations** and **49 geographic locations**.

---

## 🎯 **Core Objectives**

* **Regime Separation**: Model the "hurdle" of rain occurrence (Logit link) separately from the magnitude of rain (Log link) using a truncated Gamma distribution that cannot generate zeros, ensuring strict separation between occurrence and intensity processes.
* **Spatial Variance Partitioning**: Quantify regional heterogeneity using random intercepts and slopes.
* **Temporal Persistence**: Account for Markovian dependencies and dry-spell decay using natural splines.
* **Reproducible Inference**: Provide a fully documented Quarto workflow, from hybrid Random Forest imputation to DHARMa residual diagnostics.

---

## 🏗️ **Methodological Framework**

### **The Likelihood Model**

The response  is treated as a mixture distribution:


### **Engineering from First Principles**

* **Circular Wind Decomposition**: Transformed wind direction into  (Zonal) and  (Meridional) vectors to maintain 360° continuity.
* **The "Rain Corner"**: Formulated a centered interaction () to capture the thermodynamic threshold for convective initiation.
* **Cyclical Encoding**: Applied sine/cosine transforms to the day of the year to preserve the proximity of December to January.

---

## 🚀 **Key Statistical Findings**

### **Model Progression & Information Loss (AIC)**

| Model | Specification | AIC |  AIC |
| --- | --- | --- | --- |
| **M0** | Null Baseline | 461,670 | — |
| **M1** | Moisture & Pressure Dynamics | 422,847 | -38,823 |
| **M2** | Temporal Persistence (Markov) | 406,659 | -16,188 |
| **M3** | Historical Moving Averages | 404,936 | -1,723 |
| **M4** | Energy Interactions (Rain Corner) | 403,052 | -1,884 |
| **M5** | Circular Wind Vectors | 402,453 | -599 |
| **M6** | **Mixed Effects (Random Slopes)** | **394,854** | **-7,599** |

### **Variance Analysis (Nakagawa's R²)**

* **Marginal R² (Fixed Effects)**: **0.345**
* **Conditional R² (Total Variance)**: **0.441**
* **Important Note**: These R² values specifically measure the fit of the **intensity model** (truncated Gamma component) conditional on rain occurring. They do not reflect the full two-stage rainfall process. The hurdle component (occurrence) is evaluated separately through classification metrics.
* **Inference**: Geography accounts for ~10% of explainable variance in rainfall intensity, proving that "global" models mask significant regional physics.

---

## ✅ **Model Validation & Diagnostics**

The final model (M6) was validated using simulation-based residuals (**DHARMa**):

* **Distributional Fit**: KS Test () confirmed the truncated Gamma distribution as the optimal choice for the wet-regime intensity conditional on occurrence.
* **Hurdle Calibration**: The ratio of observed to simulated zeros was **1.00**, indicating perfect calibration of the Logit hurdle component.
* **Independence**: Durbin-Watson statistic of **2.04** confirmed that including Markov persistence successfully "bleached" temporal autocorrelation from the residuals.

---

## 📁 **Repository Structure**

```text
.
├── index.qmd          # Master Quarto document (The "Thesis")
├── _quarto.yml        # Site configuration
├── data/              # Raw data (weatherAUS.csv) and imputed sets
├── models/            # Serialized glmmTMB objects (.RData)
├── docs/              # Rendered HTML report (GitHub Pages)
├── ausweather.ipynb   # Initial exploratory notebook
└── refactored.ipynb   # Refined computational workflow

```

---

## 🛠️ **Computational Environment**

### **Requirements**

* **R 4.4.x**
* **Quarto CLI**
* **Key Libraries**: `glmmTMB`, `DHARMa`, `performance`, `missRanger`, `tidyverse`, `tidymodels`.

### **Installation & Reproduction**

1. **Clone the Repository**:
```bash
git clone https://github.com/Chrisolande/australian-rainfall-dynamics.git

```


2. **Restore Environment**:
Open R and run:
```r
librarian::shelf(tidyverse, glmmTMB, DHARMa, missRanger, performance, pROC)

```


3. **Render Analysis**:
```bash
quarto render index.qmd

```



---

## 🤝 **Usage & Constraints**

* **Suitable For**: Short-range probabilistic forecasting, regional climate characterization, and agricultural moisture planning.
* **Unsuitable For**: Extreme Value Analysis (EVA). This model uses a Gamma tail; for 100-year flood risk or catastrophic event engineering, Generalized Pareto Distributions (EVT) are recommended.

## 👤 **Author**

**Chris Olande** *Statistician & Programmer* *Nairobi, Kenya* *"Statistical models should reflect physical reality, not just mathematical convenience."*

---

**License**: MIT

**Citation**: Olande, C. (2026). *Predicting the Unpredictable: Australian Rainfall Dynamics. A Zero-Inflated Gamma Mixed Model Approach.*
