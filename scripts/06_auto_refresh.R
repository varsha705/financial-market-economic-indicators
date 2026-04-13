# ============================================================
# 06_auto_refresh.R
# Automatically fetches latest FRED data and updates CSVs
# Run this script to get fresh data before opening Power BI
# ============================================================

library(httr)
library(jsonlite)
library(dplyr)
library(readr)
library(lubridate)
library(zoo)

source("config.R")

cat("Auto-refresh started at:", as.character(Sys.time()), "\n")

fetch_fred_series <- function(series_id, api_key,
                              start_date = "2000-01-01") {
  cat("Fetching:", series_id, "\n")
  url      <- "https://api.stlouisfed.org/fred/series/observations"
  response <- GET(url, query = list(
    series_id         = series_id,
    api_key           = api_key,
    observation_start = start_date,
    file_type         = "json",
    limit             = 10000,
    offset            = 0
  ))
  if (status_code(response) != 200) return(NULL)
  parsed <- fromJSON(content(response, as = "text",
                             encoding = "UTF-8"), flatten = TRUE)
  parsed$observations %>%
    as_tibble() %>%
    select(date, value) %>%
    mutate(date  = as.Date(date),
           value = as.numeric(value),
           series = series_id) %>%
    filter(!is.na(value))
}

# Fetch all indicators
cpi          <- fetch_fred_series("CPIAUCSL",        FRED_API_KEY)
unemployment <- fetch_fred_series("UNRATE",          FRED_API_KEY)
interest     <- fetch_fred_series("FEDFUNDS",        FRED_API_KEY)
gdp          <- fetch_fred_series("A191RL1Q225SBEA", FRED_API_KEY)
oil          <- fetch_fred_series("DCOILWTICO",      FRED_API_KEY)
money        <- fetch_fred_series("M2SL",            FRED_API_KEY)
sp500        <- fetch_fred_series("SP500",           FRED_API_KEY)
vix          <- fetch_fred_series("VIXCLS",          FRED_API_KEY)
treasury     <- fetch_fred_series("GS10",            FRED_API_KEY)
ppi          <- fetch_fred_series("PPIACO",          FRED_API_KEY)

# Save raw files
write_csv(cpi,          "data/raw/cpi.csv")
write_csv(unemployment, "data/raw/unemployment.csv")
write_csv(interest,     "data/raw/interest_rate.csv")
write_csv(gdp,          "data/raw/gdp.csv")
write_csv(oil,          "data/raw/oil_price.csv")
write_csv(money,        "data/raw/money_supply.csv")
write_csv(sp500,        "data/raw/sp500.csv")
write_csv(vix,          "data/raw/vix.csv")
write_csv(treasury,     "data/raw/treasury.csv")
write_csv(ppi,          "data/raw/ppi.csv")

cat("Raw data updated!\n")

# Re-run preprocessing
source("scripts/02_preprocessing.R")
cat("Preprocessing complete!\n")

# Re-run modelling
source("scripts/04_modelling.R")
cat("Models retrained!\n")

# Re-run export
source("scripts/05_export_predictions.R")
cat("Power BI files updated!\n")

# Log the refresh
refresh_log <- tibble(
  refresh_time   = Sys.time(),
  records_fetched = nrow(cpi) + nrow(unemployment) +
    nrow(interest) + nrow(gdp),
  status         = "Success"
)
write_csv(refresh_log, "data/processed/refresh_log.csv")

cat("\nAuto-refresh complete at:", as.character(Sys.time()), "\n")
cat("Now open Power BI and click Refresh to see updated data!\n")