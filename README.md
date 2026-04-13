# Financial Market Economic Indicators

Analysis of macroeconomic indicator impact on inflation using Machine Learning.

## Project Overview
Predicts 3-month and 6-month inflation using FRED API data and multiple ML models.

## Models Trained
- Linear Regression (Best: RMSE 1.52, R² 0.93)
- Random Forest
- XGBoost
- ARIMA (Time Series)

## Setup Instructions

### Prerequisites
- R 4.x + RStudio
- Power BI Desktop

### Install R Packages
```r
install.packages(c("httr","jsonlite","dplyr","readr",
                   "ggplot2","lubridate","zoo","forecast",
                   "randomForest","xgboost","caret",
                   "shiny","shinydashboard","shinyWidgets"))
```

### API Key Setup
1. Get free FRED API key at https://fredaccount.stlouisfed.org
2. Copy config_template.R and rename to config.R
3. Add your API key to config.R

### Execution Order
1. scripts/01_data_collection.R — Fetches 10 indicators from FRED API
2. scripts/02_preprocessing.R — Cleans data, creates 29 features
3. scripts/03_eda.R — Creates 5 ggplot2 visualizations
4. scripts/04_modelling.R — Trains 4 ML models
5. scripts/05_export_predictions.R — Exports data for Power BI
6. Open powerbi/finance.pbix in Power BI Desktop
7. Run app.R in RStudio for interactive Shiny forecasting app

## Results
- Best Model: Linear Regression
- 3M RMSE: 1.52 | R²: 0.93
- 6M RMSE: 2.85 | R²: 0.92

## Dataset
- Source: FRED Economic Data API (Federal Reserve)
- 10 macroeconomic indicators collected
- 20,000+ raw records across all indicators
- Date range: 2017-2025
- Final dataset: 101 monthly observations x 29 features

## Key Indicators
- CPI (Inflation)
- Unemployment Rate
- Federal Funds Interest Rate
- GDP Growth Rate
- WTI Oil Price
- M2 Money Supply
- S&P 500 Index
- VIX Volatility Index
- 10-Year Treasury Yield
- Producer Price Index

## Repository Structure
- data/raw/ — Raw API data (10 CSV files)
- data/processed/ — Cleaned and merged datasets
- scripts/ — All R scripts (01 through 06)
- visuals/ — 5 ggplot2 visualization charts
- powerbi/ — Power BI dashboard file
- app.R — Interactive Shiny web application
- Dockerfile — Container configuration

## GitHub
- 12+ commits across 2 branches (main and development)
- All development done on development branch first
- Merged to main after testing