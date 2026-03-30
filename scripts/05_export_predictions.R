# ============================================================
# 05_export_predictions.R
# Export final predictions and summary for Power BI
# ============================================================

library(dplyr)
library(readr)
library(lubridate)

cat("Loading data...\n")

df          <- read_csv("data/processed/merged_indicators.csv")
predictions <- read_csv("data/processed/predictions.csv")
model_results <- read_csv("data/processed/model_results.csv")

# ============================================================
# EXPORT 1: Full dataset for Power BI trends dashboard
# ============================================================

powerbi_main <- df %>%
  select(date, cpi, unemployment, interest_rate,
         oil_price, money_supply, sp500, vix,
         treasury_yield, ppi, gdp_growth,
         cpi_growth, oil_growth, money_supply_growth,
         market_trend, target_3m, target_6m) %>%
  mutate(
    year    = year(date),
    month   = month(date),
    quarter = quarter(date)
  )

write_csv(powerbi_main, "data/processed/powerbi_main.csv")
cat("Saved: powerbi_main.csv -", nrow(powerbi_main), "rows\n")

# ============================================================
# EXPORT 2: Predictions comparison for Power BI
# ============================================================

powerbi_predictions <- predictions %>%
  mutate(
    year  = year(date),
    month = month(date),
    # Forecast error for each model
    error_lm_3m    = abs(target_3m - pred_lm_3m),
    error_rf_3m    = abs(target_3m - pred_rf_3m),
    error_xgb_3m   = abs(target_3m - pred_xgb_3m),
    error_arima_3m = abs(target_3m - pred_arima_3m),
    error_lm_6m    = abs(target_6m - pred_lm_6m),
    error_rf_6m    = abs(target_6m - pred_rf_6m),
    error_xgb_6m   = abs(target_6m - pred_xgb_6m),
    error_arima_6m = abs(target_6m - pred_arima_6m)
  )

write_csv(powerbi_predictions, "data/processed/powerbi_predictions.csv")
cat("Saved: powerbi_predictions.csv -", nrow(powerbi_predictions), "rows\n")

# ============================================================
# EXPORT 3: Model performance summary for Power BI
# ============================================================

write_csv(model_results, "data/processed/powerbi_model_results.csv")
cat("Saved: powerbi_model_results.csv\n")

# ============================================================
# EXPORT 4: Key statistics summary
# ============================================================

key_stats <- tibble(
  metric = c(
    "Total Records", "Date Range Start", "Date Range End",
    "Avg CPI", "Max CPI", "Min CPI",
    "Avg Unemployment", "Avg Interest Rate",
    "Avg Oil Price", "Best Model 3M", "Best Model 6M",
    "Best RMSE 3M", "Best RMSE 6M"
  ),
  value = c(
    nrow(df),
    as.character(min(df$date)),
    as.character(max(df$date)),
    round(mean(df$cpi), 2),
    round(max(df$cpi), 2),
    round(min(df$cpi), 2),
    round(mean(df$unemployment), 2),
    round(mean(df$interest_rate), 2),
    round(mean(df$oil_price), 2),
    "Linear Regression",
    "Linear Regression",
    round(min(model_results$rmse[model_results$horizon == "3M"]), 4),
    round(min(model_results$rmse[model_results$horizon == "6M"]), 4)
  )
)

write_csv(key_stats, "data/processed/powerbi_key_stats.csv")
cat("Saved: powerbi_key_stats.csv\n")

# ============================================================
# PRINT SUMMARY
# ============================================================

cat("\n========================================\n")
cat("EXPORT SUMMARY FOR POWER BI\n")
cat("========================================\n")
cat("Files saved to data/processed/:\n")
cat("  1. powerbi_main.csv        - Main trends data\n")
cat("  2. powerbi_predictions.csv - Model predictions\n")
cat("  3. powerbi_model_results.csv - RMSE comparison\n")
cat("  4. powerbi_key_stats.csv   - Key statistics\n")

cat("\nKey Statistics:\n")
print(key_stats)

cat("\nModel Results:\n")
print(model_results)

cat("\nExport complete! Ready for Power BI.\n")
