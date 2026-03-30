# ============================================================
# 03_eda.R
# Exploratory Data Analysis + 5 ggplot2 Visualizations
# ============================================================

library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)

cat("Loading processed data...\n")
df <- read_csv("data/processed/merged_indicators.csv")

cat("Dataset dimensions:", nrow(df), "rows x", ncol(df), "cols\n")

# ============================================================
# SUMMARY STATISTICS
# ============================================================

cat("\n--- Summary Statistics ---\n")
print(summary(df[, c("cpi","unemployment","interest_rate",
                     "oil_price","vix","sp500","target_3m","target_6m")]))

cat("\n--- Correlation Matrix ---\n")
num_cols <- df %>%
  select(cpi, unemployment, interest_rate, oil_price,
         money_supply, vix, sp500, treasury_yield, ppi,
         target_3m, target_6m) %>%
  cor(use = "complete.obs")
print(round(num_cols, 2))

# ============================================================
# VISUALIZATION 1: CPI Inflation Trend Over Time
# ============================================================

cat("\nCreating visualization 1: CPI trend...\n")

p1 <- ggplot(df, aes(x = date, y = cpi)) +
  geom_line(color = "#2196F3", linewidth = 1) +
  geom_smooth(method = "loess", se = TRUE,
              color = "#F44336", fill = "#FFCDD2", alpha = 0.3) +
  labs(
    title    = "CPI Inflation Trend (2017-2025)",
    subtitle = "Consumer Price Index with trend line",
    x        = "Date",
    y        = "CPI Value",
    caption  = "Source: FRED Economic Data API"
  ) +
  theme_minimal() +
  theme(
    plot.title    = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11, color = "gray50")
  )

ggsave("visuals/01_cpi_trend.png", p1, width = 10, height = 6, dpi = 150)
print(p1)
cat("Saved: visuals/01_cpi_trend.png\n")

# ============================================================
# VISUALIZATION 2: Correlation Heatmap
# ============================================================

cat("Creating visualization 2: Correlation heatmap...\n")

cor_data <- df %>%
  select(cpi, unemployment, interest_rate, oil_price,
         money_supply, vix, sp500, treasury_yield, ppi) %>%
  cor(use = "complete.obs") %>%
  as.data.frame() %>%
  tibble::rownames_to_column("var1") %>%
  tidyr::pivot_longer(-var1, names_to = "var2", values_to = "correlation")

p2 <- ggplot(cor_data, aes(x = var1, y = var2, fill = correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(correlation, 2)),
            size = 3, color = "black") +
  scale_fill_gradient2(
    low  = "#2196F3", mid = "white", high = "#F44336",
    midpoint = 0, limits = c(-1, 1)
  ) +
  labs(
    title   = "Correlation Heatmap of Economic Indicators",
    x       = "",
    y       = "",
    caption = "Source: FRED Economic Data API"
  ) +
  theme_minimal() +
  theme(
    axis.text.x   = element_text(angle = 45, hjust = 1, size = 9),
    axis.text.y   = element_text(size = 9),
    plot.title    = element_text(size = 14, face = "bold"),
    legend.title  = element_text(size = 10)
  )

ggsave("visuals/02_correlation_heatmap.png", p2, width = 10, height = 8, dpi = 150)
print(p2)
cat("Saved: visuals/02_correlation_heatmap.png\n")

# ============================================================
# VISUALIZATION 3: Unemployment vs Inflation
# ============================================================

cat("Creating visualization 3: Unemployment vs CPI...\n")

p3 <- ggplot(df, aes(x = unemployment, y = cpi, color = year(date))) +
  geom_point(size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE,
              color = "darkred", linetype = "dashed") +
  scale_color_gradient(low = "#90CAF9", high = "#1565C0",
                       name = "Year") +
  labs(
    title    = "Unemployment Rate vs CPI Inflation",
    subtitle = "Phillips Curve relationship (2017-2025)",
    x        = "Unemployment Rate (%)",
    y        = "CPI Value",
    caption  = "Source: FRED Economic Data API"
  ) +
  theme_minimal() +
  theme(
    plot.title    = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11, color = "gray50")
  )

ggsave("visuals/03_unemployment_vs_cpi.png", p3, width = 10, height = 6, dpi = 150)
print(p3)
cat("Saved: visuals/03_unemployment_vs_cpi.png\n")

# ============================================================
# VISUALIZATION 4: Oil Price vs CPI
# ============================================================

cat("Creating visualization 4: Oil price vs CPI...\n")

p4 <- ggplot(df, aes(x = date)) +
  geom_line(aes(y = oil_price, color = "Oil Price (WTI)"), linewidth = 1) +
  geom_line(aes(y = cpi - 200, color = "CPI (scaled)"), linewidth = 1) +
  scale_color_manual(
    values = c("Oil Price (WTI)" = "#FF9800",
               "CPI (scaled)"   = "#4CAF50"),
    name = "Indicator"
  ) +
  scale_y_continuous(
    name     = "Oil Price (USD)",
    sec.axis = sec_axis(~ . + 200, name = "CPI Value")
  ) +
  labs(
    title    = "Oil Price vs CPI Inflation Over Time",
    subtitle = "Dual axis comparison (2017-2025)",
    x        = "Date",
    caption  = "Source: FRED Economic Data API"
  ) +
  theme_minimal() +
  theme(
    plot.title    = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11, color = "gray50"),
    legend.position = "top"
  )

ggsave("visuals/04_oil_vs_cpi.png", p4, width = 10, height = 6, dpi = 150)
print(p4)
cat("Saved: visuals/04_oil_vs_cpi.png\n")

# ============================================================
# VISUALIZATION 5: Market Volatility (VIX) Over Time
# ============================================================

cat("Creating visualization 5: VIX volatility...\n")

p5 <- ggplot(df, aes(x = date, y = vix)) +
  geom_area(fill = "#EF9A9A", alpha = 0.4) +
  geom_line(color = "#C62828", linewidth = 1) +
  geom_hline(yintercept = 20, linetype = "dashed",
             color = "gray40", linewidth = 0.8) +
  annotate("text", x = min(df$date), y = 21,
           label = "Threshold: VIX = 20 (High Volatility)",
           hjust = 0, size = 3.5, color = "gray40") +
  labs(
    title    = "Market Volatility Index (VIX) Over Time",
    subtitle = "Spikes indicate periods of market stress (COVID-19, Rate Hikes)",
    x        = "Date",
    y        = "VIX Value",
    caption  = "Source: FRED Economic Data API"
  ) +
  theme_minimal() +
  theme(
    plot.title    = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11, color = "gray50")
  )

ggsave("visuals/05_vix_volatility.png", p5, width = 10, height = 6, dpi = 150)
print(p5)
cat("Saved: visuals/05_vix_volatility.png\n")

cat("\nEDA complete! All 5 visualizations saved to visuals/ folder\n")