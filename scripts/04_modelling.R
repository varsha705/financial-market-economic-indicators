# ============================================================
# 04_modelling.R
# Model Building: Linear Regression, Random Forest, 
# XGBoost, ARIMA
# Predicts: target_3m and target_6m (inflation forecasts)
# ============================================================

library(dplyr)
library(readr)
library(caret)
library(randomForest)
library(xgboost)
library(forecast)

cat("Loading processed data...\n")
df <- read_csv("data/processed/merged_indicators.csv")

# ============================================================
# STEP 1: Prepare features and targets
# ============================================================

cat("Preparing features...\n")

features <- c("cpi_lag_1", "cpi_lag_3", "cpi_lag_6", "cpi_lag_12",
              "interest_rate", "interest_lag_3",
              "unemployment", "unemployment_lag3",
              "oil_price", "oil_lag_3",
              "money_supply", "money_supply_growth",
              "vix", "vix_rolling_3",
              "sp500", "treasury_yield", "ppi",
              "cpi_rolling_3", "cpi_rolling_6",
              "oil_rolling_3", "cpi_growth", "oil_growth")

# Remove rows with NA in features
model_df <- df %>%
  select(date, all_of(features), target_3m, target_6m) %>%
  filter(complete.cases(.))

cat("Model dataset:", nrow(model_df), "rows x", 
    ncol(model_df), "cols\n")

# Train/test split (80/20)
set.seed(42)
n         <- nrow(model_df)
train_idx <- 1:floor(0.8 * n)
test_idx  <- (floor(0.8 * n) + 1):n

train <- model_df[train_idx, ]
test  <- model_df[test_idx, ]

cat("Training set:", nrow(train), "rows\n")
cat("Test set:", nrow(test), "rows\n")

X_train <- train %>% select(all_of(features)) %>% as.data.frame()
X_test  <- test  %>% select(all_of(features)) %>% as.data.frame()
y_train_3m <- train$target_3m
y_train_6m <- train$target_6m
y_test_3m  <- test$target_3m
y_test_6m  <- test$target_6m

# RMSE function
rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

results <- tibble(
  model    = character(),
  horizon  = character(),
  rmse     = numeric(),
  mae      = numeric(),
  r2       = numeric()
)

# ============================================================
# MODEL 1: Linear Regression
# ============================================================

cat("\n--- Training Linear Regression ---\n")

lm_3m <- lm(target_3m ~ ., data = cbind(X_train, target_3m = y_train_3m))
lm_6m <- lm(target_6m ~ ., data = cbind(X_train, target_6m = y_train_6m))

pred_lm_3m <- predict(lm_3m, newdata = X_test)
pred_lm_6m <- predict(lm_6m, newdata = X_test)

rmse_lm_3m <- rmse(y_test_3m, pred_lm_3m)
rmse_lm_6m <- rmse(y_test_6m, pred_lm_6m)
mae_lm_3m  <- mean(abs(y_test_3m - pred_lm_3m))
mae_lm_6m  <- mean(abs(y_test_6m - pred_lm_6m))
r2_lm_3m   <- cor(y_test_3m, pred_lm_3m)^2
r2_lm_6m   <- cor(y_test_6m, pred_lm_6m)^2

cat("Linear Regression 3M - RMSE:", round(rmse_lm_3m, 4),
    "| MAE:", round(mae_lm_3m, 4),
    "| R2:", round(r2_lm_3m, 4), "\n")
cat("Linear Regression 6M - RMSE:", round(rmse_lm_6m, 4),
    "| MAE:", round(mae_lm_6m, 4),
    "| R2:", round(r2_lm_6m, 4), "\n")

results <- bind_rows(results, tibble(
  model = "Linear Regression", horizon = "3M",
  rmse = rmse_lm_3m, mae = mae_lm_3m, r2 = r2_lm_3m
))
results <- bind_rows(results, tibble(
  model = "Linear Regression", horizon = "6M",
  rmse = rmse_lm_6m, mae = mae_lm_6m, r2 = r2_lm_6m
))

# ============================================================
# MODEL 2: Random Forest
# ============================================================

cat("\n--- Training Random Forest ---\n")

set.seed(42)
rf_3m <- randomForest(
  x = X_train, y = y_train_3m,
  ntree = 500, importance = TRUE
)
rf_6m <- randomForest(
  x = X_train, y = y_train_6m,
  ntree = 500, importance = TRUE
)

pred_rf_3m <- predict(rf_3m, newdata = X_test)
pred_rf_6m <- predict(rf_6m, newdata = X_test)

rmse_rf_3m <- rmse(y_test_3m, pred_rf_3m)
rmse_rf_6m <- rmse(y_test_6m, pred_rf_6m)
mae_rf_3m  <- mean(abs(y_test_3m - pred_rf_3m))
mae_rf_6m  <- mean(abs(y_test_6m - pred_rf_6m))
r2_rf_3m   <- cor(y_test_3m, pred_rf_3m)^2
r2_rf_6m   <- cor(y_test_6m, pred_rf_6m)^2

cat("Random Forest 3M - RMSE:", round(rmse_rf_3m, 4),
    "| MAE:", round(mae_rf_3m, 4),
    "| R2:", round(r2_rf_3m, 4), "\n")
cat("Random Forest 6M - RMSE:", round(rmse_rf_6m, 4),
    "| MAE:", round(mae_rf_6m, 4),
    "| R2:", round(r2_rf_6m, 4), "\n")

results <- bind_rows(results, tibble(
  model = "Random Forest", horizon = "3M",
  rmse = rmse_rf_3m, mae = mae_rf_3m, r2 = r2_rf_3m
))
results <- bind_rows(results, tibble(
  model = "Random Forest", horizon = "6M",
  rmse = rmse_rf_6m, mae = mae_rf_6m, r2 = r2_rf_6m
))

# ============================================================
# MODEL 3: XGBoost
# ============================================================

cat("\n--- Training XGBoost ---\n")

dtrain_3m <- xgb.DMatrix(data = as.matrix(X_train), label = y_train_3m)
dtrain_6m <- xgb.DMatrix(data = as.matrix(X_train), label = y_train_6m)
dtest     <- xgb.DMatrix(data = as.matrix(X_test))

params <- list(
  objective = "reg:squarederror",
  eta = 0.05,
  max_depth = 4,
  subsample = 0.8,
  colsample_bytree = 0.8
)

set.seed(42)
xgb_3m <- xgb.train(
  params = params, data = dtrain_3m,
  nrounds = 200, verbose = 0
)
xgb_6m <- xgb.train(
  params = params, data = dtrain_6m,
  nrounds = 200, verbose = 0
)

pred_xgb_3m <- predict(xgb_3m, dtest)
pred_xgb_6m <- predict(xgb_6m, dtest)

rmse_xgb_3m <- rmse(y_test_3m, pred_xgb_3m)
rmse_xgb_6m <- rmse(y_test_6m, pred_xgb_6m)
mae_xgb_3m  <- mean(abs(y_test_3m - pred_xgb_3m))
mae_xgb_6m  <- mean(abs(y_test_6m - pred_xgb_6m))
r2_xgb_3m   <- cor(y_test_3m, pred_xgb_3m)^2
r2_xgb_6m   <- cor(y_test_6m, pred_xgb_6m)^2

cat("XGBoost 3M - RMSE:", round(rmse_xgb_3m, 4),
    "| MAE:", round(mae_xgb_3m, 4),
    "| R2:", round(r2_xgb_3m, 4), "\n")
cat("XGBoost 6M - RMSE:", round(rmse_xgb_6m, 4),
    "| MAE:", round(mae_xgb_6m, 4),
    "| R2:", round(r2_xgb_6m, 4), "\n")

results <- bind_rows(results, tibble(
  model = "XGBoost", horizon = "3M",
  rmse = rmse_xgb_3m, mae = mae_xgb_3m, r2 = r2_xgb_3m
))
results <- bind_rows(results, tibble(
  model = "XGBoost", horizon = "6M",
  rmse = rmse_xgb_6m, mae = mae_xgb_6m, r2 = r2_xgb_6m
))

# ============================================================
# MODEL 4: ARIMA (Time Series)
# ============================================================

cat("\n--- Training ARIMA ---\n")

cpi_ts <- ts(df$cpi, start = c(2017, 1), frequency = 12)

arima_model <- auto.arima(cpi_ts, seasonal = TRUE, stepwise = FALSE)
cat("Best ARIMA model:", as.character(arima_model), "\n")

arima_forecast <- forecast(arima_model, h = 12)

arima_3m_pred <- as.numeric(arima_forecast$mean[3])
arima_6m_pred <- as.numeric(arima_forecast$mean[6])

cat("ARIMA 3M forecast:", round(arima_3m_pred, 2), "\n")
cat("ARIMA 6M forecast:", round(arima_6m_pred, 2), "\n")

arima_test_forecast <- forecast(arima_model, h = nrow(test))
pred_arima <- as.numeric(arima_test_forecast$mean)

if (length(pred_arima) >= length(y_test_3m)) {
  pred_arima_3m <- pred_arima[1:length(y_test_3m)]
  pred_arima_6m <- pred_arima[1:length(y_test_6m)]
} else {
  pred_arima_3m <- c(pred_arima,
                     rep(tail(pred_arima,1), length(y_test_3m) - length(pred_arima)))
  pred_arima_6m <- pred_arima_3m
}

rmse_arima_3m <- rmse(y_test_3m, pred_arima_3m)
rmse_arima_6m <- rmse(y_test_6m, pred_arima_6m)
mae_arima_3m  <- mean(abs(y_test_3m - pred_arima_3m))
mae_arima_6m  <- mean(abs(y_test_6m - pred_arima_6m))
r2_arima_3m   <- cor(y_test_3m, pred_arima_3m)^2
r2_arima_6m   <- cor(y_test_6m, pred_arima_6m)^2

cat("ARIMA 3M - RMSE:", round(rmse_arima_3m, 4),
    "| MAE:", round(mae_arima_3m, 4),
    "| R2:", round(r2_arima_3m, 4), "\n")
cat("ARIMA 6M - RMSE:", round(rmse_arima_6m, 4),
    "| MAE:", round(mae_arima_6m, 4),
    "| R2:", round(r2_arima_6m, 4), "\n")

results <- bind_rows(results, tibble(
  model = "ARIMA", horizon = "3M",
  rmse = rmse_arima_3m, mae = mae_arima_3m, r2 = r2_arima_3m
))
results <- bind_rows(results, tibble(
  model = "ARIMA", horizon = "6M",
  rmse = rmse_arima_6m, mae = mae_arima_6m, r2 = r2_arima_6m
))

# ============================================================
# STEP 5: Model Comparison
# ============================================================

cat("\n========================================\n")
cat("MODEL COMPARISON RESULTS\n")
cat("========================================\n")
results <- results %>%
  mutate(across(c(rmse, mae, r2), ~ round(., 4)))
print(results)

cat("\nBest model for 3M prediction (lowest RMSE):\n")
best_3m <- results %>% filter(horizon == "3M") %>%
  slice_min(rmse, n = 1)
print(best_3m)

cat("\nBest model for 6M prediction (lowest RMSE):\n")
best_6m <- results %>% filter(horizon == "6M") %>%
  slice_min(rmse, n = 1)
print(best_6m)

# ============================================================
# STEP 6: Save everything
# ============================================================

cat("\nSaving results...\n")

write_csv(results, "data/processed/model_results.csv")

predictions_df <- test %>%
  select(date, target_3m, target_6m) %>%
  mutate(
    pred_lm_3m   = pred_lm_3m,
    pred_lm_6m   = pred_lm_6m,
    pred_rf_3m   = pred_rf_3m,
    pred_rf_6m   = pred_rf_6m,
    pred_xgb_3m  = pred_xgb_3m,
    pred_xgb_6m  = pred_xgb_6m,
    pred_arima_3m = pred_arima_3m,
    pred_arima_6m = pred_arima_6m
  )

write_csv(predictions_df, "data/processed/predictions.csv")

cat("Saved: data/processed/model_results.csv\n")
cat("Saved: data/processed/predictions.csv\n")
cat("\nModelling complete!\n")