# BUSINESS SCIENCE UNIVERSITY
# DS4B 103-R: TIME SERIES FORECASTING FOR BUSINESS
# MODULE: FEATURE ENGINEERING

# GOAL ----
# - FIND ENGINEERED FEATURES BEFORE MODELING

# OBJECTIVES:
# - Time-Based Features - Trend-Based & Seasonal Features
# - Interactions
# - Fourier Series
# - Autocorrelated Lags
# - Special Events
# - External Regressor Lags

# LIBRARIES & DATA ----

# Core
library(tidyverse)
library(lubridate)
library(timetk)
library(plotly)

# Data
google_analytics_summary_tbl <- read_rds("00_data/google_analytics_summary_hourly.rds")

learning_labs_tbl <- read_rds("00_data/learning_labs.rds") 

subscribers_tbl   <- read_rds("00_data/mailchimp_users.rds")


# DATA PREPARATION ----
# - Apply Preprocessing to Target
data_prepared_tbl <- subscribers_tbl %>%
    summarize_by_time(
        optin_time
        , .by = "day"
        , optins = n()
    ) %>%
    pad_by_time(
        .date_var = optin_time
        , .pad_value = 0
    ) %>%
    
    # Preprocess
    mutate(optins_trans = log_interval_vec(optins, limit_lower = 0, offset = 1)) %>%
    mutate(optins_trans = standardize_vec(optins_trans)) %>%
    # fix missing values at beginning of series
    filter_by_time(.start_date = "2018-07-03") %>%
    
    # Cleaning
    mutate(optins_trans_cleaned = ts_clean_vec(optins_trans, period = 7)) %>%
    mutate(optins_trans = ifelse(optin_time %>% between_time("2018-11-18","2018-11-20"),
                                 optins_trans_cleaned,
                                 optins_trans)) %>%
    select(-optins, -optins_trans_cleaned)

data_prepared_tbl %>%
    pivot_longer(-optin_time) %>%
    plot_time_series(
        .date_var    = optin_time
        , .value     = value
        , .color_var = name
    )

# FEATURE INVESTIGATION ----

# 1.0 TIME-BASED FEATURES ----
# - tk_augment_timeseries_signature()

# * Time Series Signature ----


# * Trend-Based Features ----

# ** Linear Trend


# ** Nonlinear Trend - Basis Splines


# * Seasonal Features ----

# Weekly Seasonality


# ** Monthly Seasonality


# ** Together with Trend


# 2.0 INTERACTIONS ----



# 3.0 FOURIER SERIES ----
# - tk_augment_fourier

# Data Prep


# Model


# Visualize


# 4.0 LAGS ----
# - tk_augment_lags()

# Data Prep


# Model


# Visualize


# 5.0 SPECIAL EVENTS ----

# Data Prep


# Model


# Visualize

# 6.0 EXTERNAL LAGGED REGRESSORS ----
# - xregs

# Data Prep


# Model


# Visualize

# 7.0 RECOMMENDATION ----
# - Best model: 
# - Best Model Formula:

