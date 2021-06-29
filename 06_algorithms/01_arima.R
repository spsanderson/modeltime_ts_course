# BUSINESS SCIENCE UNIVERSITY
# DS4B 203-R: TIME SERIES FORECASTING FOR BUSINESS
# MODULE: ARIMA MODELING

# GOAL: Understand ARIMA

# OBJECTIVES ----
# - Describe ARIMA using Linear Regression
# - Extend to Seasonal ARIMA
# - Extend to XREGS (SARIMAX)
# - Show Modeltime ARIMA
# - Show Modeltime Auto ARIMA
# - Understand strengths & limitations

# LIBRARIES & SETUP ----

# Time Series ML
library(tidymodels)
library(modeltime)

# Core 
library(tidyverse)
library(lubridate)
library(timetk)

# DATA ----

artifacts_list <- read_rds("00_models/feature_engineering_artifacts_list.rds") 
data_prepared_tbl <- artifacts_list$data$data_prepared_tbl 

data_prepared_tbl

# TRAIN / TEST SPLITS ----

splits <- time_series_split(data_prepared_tbl, assess = "8 weeks", cumulative = TRUE)

splits %>%
    tk_time_series_cv_plan() %>%
    plot_time_series_cv_plan(optin_time, optins_trans)

train_tbl <- training(splits) %>%
    select(optin_time, optins_trans)


# 1.0 CONCEPTS ----

# * Auto Regression (AR) ----
?ar
ar(train_tbl$optins_trans, order.max = 3)
?arima
fit_arima_ar <- arima(train_tbl$optins_trans, order = c(3,0,0))

fit_lm_ar <- lm(optins_trans ~ lag_vec(optins_trans, 1) + 
       lag_vec(optins_trans, 2)+
       lag_vec(optins_trans, 3)
   , data = train_tbl)

fit_lm_ar
fit_arima_ar

# * Single Step Forecast ----
predict(fit_arima_ar, 1) %>%
    as_tibble()

predict(
    object = fit_lm_ar
    , newdata = tibble(
        optins_trans = c(train_tbl$optins_trans %>% tail(3), NA)
    )
) %>%
    as_tibble()

# * Multi-Step Forecast (Recursive) ----
predict(fit_arima_ar, 3) %>% 
    as_tibble()

predict(fit_lm_ar, newdata = tibble(optins_trans = c(train_tbl$optins_trans %>% tail(3), NA)))
predict(fit_lm_ar, newdata = tibble(optins_trans = c(train_tbl$optins_trans %>% tail(2), 0.911, NA)))

# * Integration (I) -----



# * ARIMA (MA) = Modeling Errors ----



# * Seasonal ARIMA ----




# * SARIMAX - Seasonal Regression w/ ARIMA Errors ----




# 2.0 ARIMA ----



# 3.0 AUTO ARIMA + XREGS ----

# * Model ----


# * Calibrate ----


# * Forecast Test ----


# * Accuracy Test -----


# * Refit ----


# 4.0 STRENGTHS & WEAKNESSES ----
# ARIMA is a simple algorithm that relies on Linear Regression
# Strengths: 
# - Automated Differencing & Recursive Lag Forecasting
# - Automated Parameter Search (auto_arima)
# - Single seasonality modeling included
# Weaknesses: 
# - Can only use 1 seasonality by default (XREGs can help go beyond 1 seasonality)
# - Becomes erratic with too many lags
# - Requires Expensive Parameter Search 
