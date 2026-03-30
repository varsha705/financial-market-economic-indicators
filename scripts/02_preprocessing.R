# ============================================================
# 02_preprocessing.R
# Data Cleaning, Merging & Feature Engineering
# ============================================================

library(dplyr)
library(readr)
library(lubridate)
library(zoo)

cat("Loading raw data files...\n")

cpi          <- read_csv("data/raw/cpi.csv")
unemployment <- read_csv("data/raw/unemployment.csv")
interest     <- read_csv("data/raw/interest_rate.csv")
gdp          <- read_csv("data/raw/gdp.csv")
oil          <- read_csv("data/raw/oil_price.csv")
money        <- read_csv("data/raw/money_supply.csv")
sp500        <- read_csv("data/raw/sp500.csv")
vix          <- read_csv("data/raw/vix.csv")
treasury     <- read_csv("data/raw/treasury.csv")
ppi          <- read_csv("data/raw/ppi.csv")

# ============================================================
# STEP 1: Keep only monthly data
# CPI, unemployment etc are monthly — oil, VIX, SP500 are daily
# We convert everything to monthly by taking the monthly average
# ============================================================

cat("Converting all series to monthly frequency...\n")

to_monthly <- function(df, col_name) {
  df %>%
    mutate(
      date  = as.Date(date),
      year  = year(date),
      month = month(date)
    ) %>%
    group_by(year, month) %>%
    summarise(!!col_name := mean(value, na.rm = TRUE), .groups = "drop") %>%
    mutate(date = as.Date(paste(year, month, "01", sep = "-"))) %>%
    select(date, !!col_name)
}

cpi_m          <- to_monthly(cpi,          "cpi")
unemployment_m <- to_monthly(unemployment, "unemployment")
interest_m     <- to_monthly(interest,     "interest_rate")
gdp_m          <- to_monthly(gdp,          "gdp_growth")
oil_m          <- to_monthly(oil,          "oil_price")
money_m        <- to_monthly(money,        "money_supply")
sp500_m        <- to_monthly(sp500,        "sp500")
vix_m          <- to_monthly(vix,          "vix")
treasury_m     <- to_monthly(treasury,     "treasury_yield")
ppi_m          <- to_monthly(ppi,          "ppi")

# ============================================================
# STEP 2: Merge all into one data frame
# ============================================================

cat("Merging all indicators into one data frame...\n")

df <- cpi_m %>%
  left_join(unemployment_m, by = "date") %>%
  left_join(interest_m,     by = "date") %>%
  left_join(gdp_m,          by = "date") %>%
  left_join(oil_m,          by = "date") %>%
  left_join(money_m,        by = "date") %>%
  left_join(sp500_m,        by = "date") %>%
  left_join(vix_m,          by = "date") %>%
  left_join(treasury_m,     by = "date") %>%
  left_join(ppi_m,          by = "date") %>%
  arrange(date)

cat("Merged data shape:", nrow(df), "rows x", ncol(df), "columns\n")

# ============================================================
# STEP 3: Handle missing values
# ============================================================

cat("Handling missing values...\n")

# GDP is quarterly — fill forward to monthly
df <- df %>%
  mutate(gdp_growth = na.locf(gdp_growth, na.rm = FALSE))

# For remaining NAs use linear interpolation
df <- df %>%
  mutate(across(where(is.numeric), ~ na.approx(., na.rm = FALSE))) %>%
  filter(complete.cases(.))

cat("After cleaning:", nrow(df), "rows remain\n")

# ============================================================
# STEP 4: Feature Engineering
# Create lag features, rolling averages, growth rates
# ============================================================

cat("Engineering features...\n")

df <- df %>%
  mutate(
    # Lag features — past CPI values
    cpi_lag_1  = lag(cpi, 1),
    cpi_lag_3  = lag(cpi, 3),
    cpi_lag_6  = lag(cpi, 6),
    cpi_lag_12 = lag(cpi, 12),
    
    # Lag features — other indicators
    interest_lag_3   = lag(interest_rate, 3),
    unemployment_lag3 = lag(unemployment, 3),
    oil_lag_3        = lag(oil_price, 3),
    
    # Rolling averages (3-month and 6-month)
    cpi_rolling_3   = rollmean(cpi, 3,  fill = NA, align = "right"),
    cpi_rolling_6   = rollmean(cpi, 6,  fill = NA, align = "right"),
    oil_rolling_3   = rollmean(oil_price, 3, fill = NA, align = "right"),
    oil_rolling_6   = rollmean(oil_price, 6, fill = NA, align = "right"),
    vix_rolling_3   = rollmean(vix, 3,  fill = NA, align = "right"),
    
    # Growth rates
    cpi_growth         = (cpi - lag(cpi, 12)) / lag(cpi, 12) * 100,
    money_supply_growth = (money_supply - lag(money_supply, 12)) / lag(money_supply, 12) * 100,
    oil_growth         = (oil_price - lag(oil_price, 12)) / lag(oil_price, 12) * 100,
    
    # Market trend category (for classification)
    market_trend = case_when(
      sp500 > lag(sp500, 3) * 1.05  ~ "Strong Up",
      sp500 > lag(sp500, 3)          ~ "Up",
      sp500 < lag(sp500, 3) * 0.95  ~ "Strong Down",
      TRUE                           ~ "Down"
    )
  )

# ============================================================
# STEP 5: Create Target Variables
# target_3m  = CPI 3 months into the future
# target_6m  = CPI 6 months into the future
# ============================================================

cat("Creating target variables...\n")

df <- df %>%
  mutate(
    target_3m = lead(cpi, 3),
    target_6m = lead(cpi, 6)
  )

# Remove rows where targets are NA (last 6 rows)
df <- df %>%
  filter(!is.na(target_3m) & !is.na(target_6m))

# Remove any remaining NAs from lag features
df <- df %>%
  filter(complete.cases(.))

# ============================================================
# STEP 6: Save processed data
# ============================================================

cat("Saving processed data...\n")

write_csv(df, "data/processed/merged_indicators.csv")

cat("\nFinal dataset summary:\n")
cat("Rows:", nrow(df), "\n")
cat("Columns:", ncol(df), "\n")
cat("Date range:", as.character(min(df$date)),
    "to", as.character(max(df$date)), "\n")
cat("\nColumn names:\n")
print(names(df))

cat("\nSample statistics:\n")
print(summary(df[, c("cpi","unemployment","interest_rate",
                     "oil_price","target_3m","target_6m")]))

cat("\nPreprocessing complete!\n")