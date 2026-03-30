# ============================================================
# 01_data_collection.R
# FRED API Data Collection
# ============================================================

library(httr)
library(jsonlite)
library(dplyr)
library(readr)

source("config.R")

fetch_fred_series <- function(series_id, api_key, start_date = "2000-01-01") {
  cat("Fetching series:", series_id, "\n")
  url <- "https://api.stlouisfed.org/fred/series/observations"
  response <- GET(url, query = list(
    series_id         = series_id,
    api_key           = api_key,
    observation_start = start_date,
    file_type         = "json",
    limit             = 10000,
    offset            = 0
  ))
  if (status_code(response) != 200) {
    cat("ERROR fetching", series_id, "\n")
    return(NULL)
  }
  content_text <- content(response, as = "text", encoding = "UTF-8")
  parsed       <- fromJSON(content_text, flatten = TRUE)
  df <- parsed$observations %>%
    as_tibble() %>%
    select(date, value) %>%
    mutate(
      date   = as.Date(date),
      value  = as.numeric(value),
      series = series_id
    ) %>%
    filter(!is.na(value))
  cat("  Got", nrow(df), "observations\n")
  return(df)
}

cat("Starting FRED API data collection...\n\n")

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

cat("\nSaving raw data files...\n")

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

api_log <- tibble(
  series_id   = c("CPIAUCSL","UNRATE","FEDFUNDS","A191RL1Q225SBEA",
                  "DCOILWTICO","M2SL","SP500","VIXCLS","GS10","PPIACO"),
  description = c("CPI Inflation","Unemployment Rate","Interest Rate",
                  "GDP Growth","Oil Price","Money Supply",
                  "S&P 500","VIX Volatility","10Y Treasury","Producer Price Index"),
  rows_fetched = c(nrow(cpi), nrow(unemployment), nrow(interest),
                   nrow(gdp), nrow(oil), nrow(money),
                   nrow(sp500), nrow(vix), nrow(treasury), nrow(ppi)),
  fetch_time  = Sys.time()
)

write_csv(api_log, "data/raw/api_call_log.csv")
cat("\nAPI log saved:\n")
print(api_log)
cat("\nData collection complete!\n")