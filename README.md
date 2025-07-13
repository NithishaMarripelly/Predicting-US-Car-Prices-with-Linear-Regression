# Car Price Prediction in the US using Multiple Linear Regression

This project aims to build a multiple linear regression (MLR) model in **R** to predict car prices in the United States. The analysis is based on a dataset sourced from Kaggle and explores how various car attributes influence pricing, offering actionable insights for automotive business strategies.

---

## 📊 Dataset

- **Source**: Kaggle (Car Price Dataset)
- **Size**: 205 rows × 26 columns
- **Features**:
  - **Categorical**: fuel type, brand, engine location, body style, etc.
  - **Numeric**: engine size, curb weight, horsepower, mileage, etc.
- **Target**: `price` (numeric)

> 🔹 *Note: Full dataset is included.*

---

## 🔁 Workflow

1. **Data Cleaning & Preprocessing**
   - Extracted brand names from car names
   - Encoded categorical variables and grouped categories
   - Created dummy variables and interaction terms
   - Centered continuous features and added squared terms

2. **Model Building**
   - Performed forward selection using AIC criteria
   - Reduced predictors using `regsubsets` and VIF filtering
   - Final model includes significant main and interaction effects

3. **Model Evaluation**
   - Final model: `log10(price)`
