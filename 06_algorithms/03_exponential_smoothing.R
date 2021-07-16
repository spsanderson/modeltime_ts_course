# BUSINESS SCIENCE UNIVERSITY
# DS4B 203-R: TIME SERIES FORECASTING FOR BUSINESS
# MODULE: EXPONENTIAL SMOOTHING

# GOAL: Understand Exponential Smoothing

# OBJECTIVES ----
# - ETS - Exponential Smoothing
# - TBATS - Multiple Seasonality Models
# - Seasonal Decomposition - Multiple Seasonality Models

# LIBRARIES & SETUP ----

# Time Series ML
library(tidymodels)
library(modeltime)
library(forecast)

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

# EXPONENTIAL SMOOTHING (ETS) -----
# - Error, Trend, Seasonal Model - Holt-Winters Seasonal
# - Automatic forecasting method based on Exponential Smoothing
# - Single Seasonality
# - Cannot use Xregs (purely univariate)


# * Exponential Smoothing ----



# * ETS Model ----


# * Modeltime ----



# TBATS ----
# - Multiple Seasonality Model
# - Extension of ETS for complex seasonality
# - Automatic
# - Does not support XREGS

# * TBATS Model ----


# * Modeltime ----


# SEASONAL DECOMPOSITION ----
# - Uses seasonal decomposition to model 
#   trend & seasonality separately
#   - Trend modeled with ARIMA or ETS
#   - Seasonality modeled with Seasonal Naive (SNAIVE)
# - Can handle multiple seasonality
# - ARIMA version accepts XREGS, ETS does not

# Multiple Seasonal Decomposition


# * STLM ETS Model ----


# * STLM ARIMA Model ----


# * Modeltime ----



# EVALUATION ----

# * Calibration ----


# * Forecast Test ----


# * Accuracy Test ----


# * Refit ----


