# \# Financial Market Economic Indicators

#### Analysis of macroeconomic indicator impact on inflation using Machine Learning.



##### \## Project Overview

Predicts \*\*3-month and 6-month inflation\*\* using FRED API data and multiple ML models.



##### \## Models Trained

\- Linear Regression (Best: RMSE 1.52)

\- Random Forest

\- XGBoost

\- ARIMA (Time Series)



##### \## Setup Instructions



##### \### Prerequisites

\- R 4.x + RStudio

\- Power BI Desktop



##### \### Install R Packages

```r

install.packages(c("httr","jsonlite","dplyr","readr",

&#x20;                  "ggplot2","lubridate","zoo","forecast",

&#x20;                  "randomForest","xgboost","caret"))

```



##### \### API Key Setup

1\. Get free FRED API key at https://fredaccount.stlouisfed.org

2\. Copy config\_template.R → rename to config.R

3\. Add your API key to config.R



##### \### Execution Order

1\. scripts/01\_data\_collection.R

2\. scripts/02\_preprocessing.R

3\. scripts/03\_eda.R

4\. scripts/04\_modelling.R

5\. scripts/05\_export\_predictions.R

6\. Open powerbi/finance.pbix in Power BI Desktop



##### \## Results

\- Best Model: Linear Regression

\- 3M RMSE: 1.52 | R²: 0.93

\- 6M RMSE: 2.85 | R²: 0.92



##### \## Data Source

FRED Economic Data API (Federal Reserve)

\- 10 indicators collected

\- Date range: 2017-2025

\- 101 monthly observations



##### \## Repository Structure

\- data/raw/ — Raw API data

\- data/processed/ — Cleaned datasets

\- scripts/ — All R scripts

\- visuals/ — ggplot2 charts

\- powerbi/ — Power BI dashboard

