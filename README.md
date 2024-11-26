# Association Between Coffee Intake and Peripheral Arterial Disease (PAD)

This repository contains the analysis of the association between coffee consumption (caffeinated and decaffeinated) and the presence of Peripheral Arterial Disease (PAD) using data from the National Health and Nutrition Examination Survey (NHANES, 1999-2004). The study employs logistic regression, odds ratios, and exploratory data analysis to determine significant factors influencing PAD.

---

## Overview

Peripheral Arterial Disease (PAD) is characterized by reduced blood flow to the limbs due to atherosclerotic plaque buildup. This study investigates:
1. The relationship between coffee intake and PAD incidence.
2. The differences between caffeinated and decaffeinated coffee consumption.
3. The role of additional factors such as age, smoking, physical activity, and cholesterol levels.

### Key Findings
- Coffee drinkers are **27% more likely** to have PAD compared to non-drinkers (OR ≈ 1.27).
- Decaffeinated coffee shows a protective association, reducing the odds of PAD (OR ≈ 0.42).
- Age and smoking habits are the strongest predictors of PAD incidence.

---

## Data and Methodology

### Dataset
The data is derived from NHANES (1999-2004) and includes:
- Sociodemographic information (age, gender, ethnicity).
- Clinical measurements (LDL cholesterol, triglycerides, ankle-brachial pressure index).
- Lifestyle variables (coffee consumption, smoking, physical activity).

### Preprocessing
1. Missing values were handled by:
   - Replacing numeric variables with their median.
   - Replacing categorical variables with their mode.
2. Outliers were identified and removed using Robust PCA.
3. Logistic regression was used as the primary statistical method, with backward elimination for variable selection.

---

## Results

### Key Metrics
| **Variable**         | **Odds Ratio (OR)** | **Interpretation**                         |
|-----------------------|---------------------|-------------------------------------------|
| Total Coffee Intake   | 1.27               | Coffee drinkers have higher odds of PAD.  |
| Decaffeinated Coffee  | 0.42               | Protective association with PAD.          |
| Age                  | 2.29               | Strongest predictor of PAD.               |

### Model Fit
- **McFadden’s Pseudo R²**: 0.13
- **AIC (Before Outliers)**: 1395
- **AIC (After Outliers)**: 1350.7

### Visualization
The analysis includes:
1. Histograms and boxplots of coffee consumption patterns.
2. Pearson correlation matrices for numerical variables.
3. Cramer’s V matrices for categorical variable associations.

---

## Technologies
- **Programming Language**: R
- **Libraries**:
  - `ggplot2` (Visualization)
  - `dplyr` (Data Manipulation)
  - `MASS` (Logistic Regression)
  - `FactoMineR` (Robust PCA)

---

## Usage

1. **Clone the repository**:
   ```bash
   git clone https://github.com/anibalpires4/Coffee-Intake-and-PAD-Association.git
