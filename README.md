# First-Time Homebuyer Analysis (R, Logistic Regression)

## Goal
Identify which borrower and loan factors are associated with first-time homebuyer status and translate results into marketing recommendations.

## Data
2019 mortgage lending dataset (89,767 loans, 56 variables).  
**Note:** Dataset not included due to course/data-sharing restrictions.

## Methods
- Exploratory Data Analysis (EDA)
- Logistic Regression (odds ratios with 95% CI)
- Model evaluation (Accuracy, AUC)

## Key Results
- **AUC:** 0.795  
- **Accuracy:** 71.8% (0.50 threshold)  
- Key drivers: note rate, mortgage type (FHA/USDA/VA vs conventional), loan purpose (purchase vs refi)

## Files
- [report/](report/) - Final report (PDF)
- `code/` R script used for analysis
- `figures/` Plots used in the report
