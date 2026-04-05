# ============================================================
# app.R - Inflation Forecast Shiny App
# ============================================================

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(dplyr)
library(readr)
library(randomForest)
library(xgboost)
library(forecast)
library(ggplot2)

# Load data and retrain models on startup
df         <- read_csv("data/processed/merged_indicators.csv")
model_results <- read_csv("data/processed/model_results.csv")

features <- c("cpi_lag_1","cpi_lag_3","cpi_lag_6","cpi_lag_12",
              "interest_rate","interest_lag_3",
              "unemployment","unemployment_lag3",
              "oil_price","oil_lag_3",
              "money_supply","money_supply_growth",
              "vix","vix_rolling_3",
              "sp500","treasury_yield","ppi",
              "cpi_rolling_3","cpi_rolling_6",
              "oil_rolling_3","cpi_growth","oil_growth")

model_df <- df %>%
  select(date, all_of(features), target_3m, target_6m) %>%
  filter(complete.cases(.))

train_idx  <- 1:floor(0.8 * nrow(model_df))
train      <- model_df[train_idx, ]
X_train    <- train %>% select(all_of(features)) %>% as.data.frame()
y_train_3m <- train$target_3m
y_train_6m <- train$target_6m

# Train models
lm_3m <- lm(target_3m ~ ., data = cbind(X_train, target_3m = y_train_3m))
lm_6m <- lm(target_6m ~ ., data = cbind(X_train, target_6m = y_train_6m))

set.seed(42)
rf_3m <- randomForest(x = X_train, y = y_train_3m, ntree = 300)
rf_6m <- randomForest(x = X_train, y = y_train_6m, ntree = 300)

dtrain_3m <- xgb.DMatrix(data = as.matrix(X_train), label = y_train_3m)
dtrain_6m <- xgb.DMatrix(data = as.matrix(X_train), label = y_train_6m)
params    <- list(objective = "reg:squarederror", eta = 0.05,
                  max_depth = 4, subsample = 0.8)
xgb_3m <- xgb.train(params = params, data = dtrain_3m,
                    nrounds = 200, verbose = 0)
xgb_6m <- xgb.train(params = params, data = dtrain_6m,
                    nrounds = 200, verbose = 0)

# Get recent values for defaults
latest <- tail(df, 1)

# ============================================================
# UI
# ============================================================

ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(title = "Inflation Forecast App"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Predict Inflation", tabName = "predict",
               icon = icon("chart-line")),
      menuItem("Model Performance", tabName = "performance",
               icon = icon("table")),
      menuItem("Historical Trends", tabName = "trends",
               icon = icon("history"))
    )
  ),
  
  dashboardBody(
    tabItems(
      
      # ---- TAB 1: PREDICTION ----
      tabItem(tabName = "predict",
              fluidRow(
                box(title = "Input Economic Indicators", width = 4,
                    status = "primary", solidHeader = TRUE,
                    
                    sliderInput("cpi", "Current CPI:",
                                min = 200, max = 350,
                                value = round(latest$cpi, 1), step = 0.1),
                    
                    sliderInput("interest_rate", "Interest Rate (%):",
                                min = 0, max = 6,
                                value = round(latest$interest_rate, 2),
                                step = 0.05),
                    
                    sliderInput("unemployment", "Unemployment Rate (%):",
                                min = 3, max = 15,
                                value = round(latest$unemployment, 1),
                                step = 0.1),
                    
                    sliderInput("oil_price", "Oil Price (USD):",
                                min = 20, max = 130,
                                value = round(latest$oil_price, 1),
                                step = 0.5),
                    
                    sliderInput("vix", "VIX Volatility:",
                                min = 10, max = 80,
                                value = round(latest$vix, 1),
                                step = 0.5),
                    
                    sliderInput("sp500", "S&P 500:",
                                min = 2000, max = 6000,
                                value = round(latest$sp500, 0),
                                step = 10),
                    
                    actionBttn("predict_btn", "Generate Forecast",
                               style = "fill", color = "primary",
                               icon = icon("play"))
                ),
                
                box(title = "Inflation Forecast Results",
                    width = 8, status = "success", solidHeader = TRUE,
                    
                    fluidRow(
                      valueBoxOutput("pred_3m_box", width = 6),
                      valueBoxOutput("pred_6m_box", width = 6)
                    ),
                    
                    fluidRow(
                      valueBoxOutput("change_3m_box", width = 6),
                      valueBoxOutput("change_6m_box", width = 6)
                    ),
                    
                    hr(),
                    h4("Forecast by Model"),
                    tableOutput("model_comparison_table"),
                    
                    hr(),
                    plotOutput("forecast_plot", height = "300px")
                )
              )
      ),
      
      # ---- TAB 2: MODEL PERFORMANCE ----
      tabItem(tabName = "performance",
              fluidRow(
                box(title = "Model RMSE Comparison",
                    width = 12, status = "info", solidHeader = TRUE,
                    plotOutput("rmse_plot", height = "400px")
                )
              ),
              fluidRow(
                box(title = "Detailed Metrics",
                    width = 12, status = "info", solidHeader = TRUE,
                    tableOutput("metrics_table")
                )
              )
      ),
      
      # ---- TAB 3: HISTORICAL TRENDS ----
      tabItem(tabName = "trends",
              fluidRow(
                box(title = "CPI Inflation Over Time",
                    width = 12, status = "warning", solidHeader = TRUE,
                    plotOutput("cpi_trend_plot", height = "350px")
                )
              ),
              fluidRow(
                box(title = "Key Indicators Over Time",
                    width = 12, status = "warning", solidHeader = TRUE,
                    selectInput("indicator_select", "Select Indicator:",
                                choices = c("unemployment", "interest_rate",
                                            "oil_price", "vix", "sp500"),
                                selected = "unemployment"),
                    plotOutput("indicator_plot", height = "300px")
                )
              )
      )
    )
  )
)

# ============================================================
# SERVER
# ============================================================

server <- function(input, output, session) {
  
  # Reactive prediction
  predictions <- eventReactive(input$predict_btn, {
    
    # Build input using latest data as base, override with user inputs
    input_data <- tail(model_df %>% select(all_of(features)), 1) %>%
      as.data.frame()
    
    input_data$cpi_lag_1    <- input$cpi
    input_data$cpi_lag_3    <- input$cpi
    input_data$interest_rate <- input$interest_rate
    input_data$unemployment  <- input$unemployment
    input_data$oil_price     <- input$oil_price
    input_data$vix           <- input$vix
    input_data$sp500         <- input$sp500
    
    dtest <- xgb.DMatrix(data = as.matrix(input_data))
    
    tibble(
      Model = c("Linear Regression", "Random Forest", "XGBoost"),
      Forecast_3M = round(c(
        predict(lm_3m,  newdata = input_data),
        predict(rf_3m,  newdata = input_data),
        predict(xgb_3m, dtest)
      ), 2),
      Forecast_6M = round(c(
        predict(lm_6m,  newdata = input_data),
        predict(rf_6m,  newdata = input_data),
        predict(xgb_6m, dtest)
      ), 2)
    )
  }, ignoreNULL = FALSE)
  
  # Best prediction (Linear Regression)
  best_3m <- reactive({
    round(predictions()$Forecast_3M[1], 2)
  })
  best_6m <- reactive({
    round(predictions()$Forecast_6M[1], 2)
  })
  
  # Value boxes
  output$pred_3m_box <- renderValueBox({
    valueBox(
      value = paste0(best_3m(), " CPI"),
      subtitle = "3-Month Inflation Forecast",
      icon = icon("arrow-trend-up"),
      color = "blue"
    )
  })
  
  output$pred_6m_box <- renderValueBox({
    valueBox(
      value = paste0(best_6m(), " CPI"),
      subtitle = "6-Month Inflation Forecast",
      icon = icon("calendar"),
      color = "purple"
    )
  })
  
  output$change_3m_box <- renderValueBox({
    change <- round(best_3m() - input$cpi, 2)
    color  <- ifelse(change > 0, "red", "green")
    valueBox(
      value = paste0(ifelse(change > 0, "+", ""), change),
      subtitle = "Expected CPI Change (3M)",
      icon = icon(ifelse(change > 0, "arrow-up", "arrow-down")),
      color = color
    )
  })
  
  output$change_6m_box <- renderValueBox({
    change <- round(best_6m() - input$cpi, 2)
    color  <- ifelse(change > 0, "red", "green")
    valueBox(
      value = paste0(ifelse(change > 0, "+", ""), change),
      subtitle = "Expected CPI Change (6M)",
      icon = icon(ifelse(change > 0, "arrow-up", "arrow-down")),
      color = color
    )
  })
  
  # Model comparison table
  output$model_comparison_table <- renderTable({
    predictions()
  })
  
  # Forecast plot
  output$forecast_plot <- renderPlot({
    preds <- predictions()
    plot_data <- tibble(
      Month    = c("Now", "3 Months", "6 Months"),
      LR       = c(input$cpi, preds$Forecast_3M[1], preds$Forecast_6M[1]),
      RF       = c(input$cpi, preds$Forecast_3M[2], preds$Forecast_6M[2]),
      XGBoost  = c(input$cpi, preds$Forecast_3M[3], preds$Forecast_6M[3])
    )
    plot_data$Month <- factor(plot_data$Month,
                              levels = c("Now","3 Months","6 Months"))
    
    ggplot(plot_data, aes(x = Month)) +
      geom_line(aes(y = LR,      color = "Linear Regression",
                    group = 1), linewidth = 1.2) +
      geom_line(aes(y = RF,      color = "Random Forest",
                    group = 1), linewidth = 1.2) +
      geom_line(aes(y = XGBoost, color = "XGBoost",
                    group = 1), linewidth = 1.2) +
      geom_point(aes(y = LR,      color = "Linear Regression"),
                 size = 3) +
      geom_point(aes(y = RF,      color = "Random Forest"),
                 size = 3) +
      geom_point(aes(y = XGBoost, color = "XGBoost"),
                 size = 3) +
      scale_color_manual(values = c(
        "Linear Regression" = "#2196F3",
        "Random Forest"     = "#4CAF50",
        "XGBoost"           = "#FF9800"
      )) +
      labs(title = "Inflation Forecast by Model",
           y = "CPI Value", x = "", color = "Model") +
      theme_minimal() +
      theme(plot.title = element_text(size = 14, face = "bold"))
  })
  
  # RMSE plot
  output$rmse_plot <- renderPlot({
    ggplot(model_results,
           aes(x = reorder(model, rmse), y = rmse, fill = horizon)) +
      geom_bar(stat = "identity", position = "dodge") +
      coord_flip() +
      scale_fill_manual(values = c("3M" = "#2196F3", "6M" = "#F44336")) +
      labs(title = "Model RMSE Comparison",
           subtitle = "Lower = Better",
           x = "Model", y = "RMSE", fill = "Horizon") +
      theme_minimal() +
      theme(plot.title = element_text(size = 14, face = "bold"))
  })
  
  # Metrics table
  output$metrics_table <- renderTable({
    model_results %>%
      arrange(horizon, rmse) %>%
      mutate(across(c(rmse, mae, r2), ~ round(., 4)))
  })
  
  # CPI trend plot
  output$cpi_trend_plot <- renderPlot({
    ggplot(df, aes(x = date, y = cpi)) +
      geom_line(color = "#2196F3", linewidth = 1) +
      geom_smooth(method = "loess", se = TRUE,
                  color = "#F44336", fill = "#FFCDD2", alpha = 0.3) +
      labs(title = "CPI Inflation Trend (2017-2025)",
           x = "Date", y = "CPI Value") +
      theme_minimal() +
      theme(plot.title = element_text(size = 14, face = "bold"))
  })
  
  # Indicator plot
  output$indicator_plot <- renderPlot({
    ggplot(df, aes_string(x = "date", y = input$indicator_select)) +
      geom_line(color = "#9C27B0", linewidth = 1) +
      labs(title = paste(input$indicator_select, "Over Time"),
           x = "Date",
           y = input$indicator_select) +
      theme_minimal() +
      theme(plot.title = element_text(size = 14, face = "bold"))
  })
}

# ============================================================
# RUN APP
# ============================================================
shinyApp(ui = ui, server = server)