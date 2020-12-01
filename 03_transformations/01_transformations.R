# BUSINESS SCIENCE UNIVERSITY
# DS4B 203-R: ADVANCED TIME SERIES FORECASTING FOR BUSINESS
# MODULE: TIME SERIES TRANSFORMATIONS -----

# GOAL ----
# - Exposure to Common Time Series Transformations

# OBJECTIVES ----
# - Variance Reduction - Log, Log1P, Box Cox
# - Rolling & Smoothing
# - Range Reduction - Normalization & Standardization
# - Imputation & Outliers
# - Lags & Differencing
# - Confined Interval Forecasting



# LIBRARIES ----

library(tidyverse)
library(timetk)
library(lubridate)

# DATA ----
google_analytics_summary_tbl <- read_rds("00_data/google_analytics_summary_hourly.rds")
google_analytics_summary_tbl 

mailchimp_users_tbl <- read_rds("00_data/mailchimp_users.rds")
mailchimp_users_tbl 

transactions_tbl <- read_rds("00_data/transactions_weekly.rds")
transactions_tbl 


# DATA PREP ----

google_analytics_summary_long_tbl <- google_analytics_summary_tbl %>%
    mutate(date = ymd_h(dateHour)) %>%
    select(-dateHour) %>%
    pivot_longer(-date) %>%
    group_by(name)

subscribers_daily_tbl <- mailchimp_users_tbl %>%
    summarise_by_time(optin_time, .by = "day", optins = n()) %>%
    pad_by_time(.pad_value = 0, .start_date = "2018-06-01")



# 1.0 VARIANCE REDUCTION ----

# * Log & Log Plus 1----

# ** Subscribers (Single)

# No transformation

subscribers_daily_tbl %>%
    plot_time_series(.date_var = optin_time, .value = optins)

subscribers_daily_tbl %>%
    plot_time_series_regression(
        .date_var = optin_time
        , .formula = optins ~ as.numeric(optin_time) +
            wday(optin_time, label = TRUE) +
            month(optin_time, label = TRUE)
        , .show_summary = TRUE
    )

# Log - Show Error 

subscribers_daily_tbl %>%
    plot_time_series(.date_var = optin_time, .value = log(optins))

# Log Plus 1
subscribers_daily_tbl %>%
    plot_time_series(.date_var = optin_time, .value = log1p(optins))

# Inversion
subscribers_daily_tbl %>%
    plot_time_series(.date_var = optin_time, .value = log1p(optins) %>% expm1())

# Benefit
subscribers_daily_tbl %>%
    plot_time_series_regression(
        .date_var = optin_time
        , .formula = log1p(optins) ~ as.numeric(optin_time) +
            wday(optin_time, label = TRUE) +
            month(optin_time, label = TRUE)
        , .show_summary = TRUE
    )

# Google Analytics (Groups)
google_analytics_summary_long_tbl %>%
    plot_time_series(.date_var = date, .value = log1p(value), .color_var = name)

# Inversion
google_analytics_summary_long_tbl %>%
    plot_time_series(.date_var = date, .value = log1p(value) %>% expm1(), .color_var = name)


# * Box Cox ----

# ** Subscribers (Single)

# Box Cox Vec
subscribers_daily_tbl %>%
    plot_time_series(optin_time, box_cox_vec(optins + 1, lambda = "auto"))

subscribers_daily_tbl %>%
    plot_time_series_regression(
        .date_var = optin_time
        , .formula = box_cox_vec(optins + 1) ~ as.numeric(optin_time) +
            wday(optin_time, label = TRUE) +
            month(optin_time, label = TRUE)
        )

# Box Cox Inversion
subscribers_daily_tbl %>%
    plot_time_series(
        .date_var = optin_time
        , .value = box_cox_vec(optins + 1, lambda = -0.1895) %>%
            box_cox_inv_vec(lambda = -0.1895)
    )


# ** Google Analytics (Groups)
google_analytics_summary_long_tbl %>%
    plot_time_series(
        .date_var = date
        , .value = box_cox_vec(value)
    )

google_analytics_summary_long_tbl %>%
    mutate(value_trans = box_cox_vec(value)) %>%
    group_split() %>%
    map2(.y = 1:3, .f = function(df, idx) {
        if(idx == 1) lambda <- 0.441313194936969
        if(idx == 2) lambda <- -0.0023793550944814
        if(idx == 3) lambda <- -0.116626712183629
        
        df %>%
            mutate(value_trans_inv = box_cox_inv_vec(x = value_trans, lambda = lambda))
    }) %>%
    bind_rows() %>%
    group_by(name) %>%
    plot_time_series(
        .date_var = date
        , .value = value_trans_inv
    )


# 2.0 ROLLING & SMOOTHING ----
# - Common time series operations to visualize trend
# - A simple transformation that can help create improve features 
# - Can help with outlier-effect reduction & trend detection
# - Note: Businesses often use a rolling average as a forecasting technique 
#   - A rolling average forecast is usually sub-optimal (good opportunity for you!)

# * Sliding / Rolling Functions ----



# * LOESS smoother ----



# * Rolling Correlations ----
# - Identify changing relationships




# * Problem with Moving Avg Forecasting ----





# 3.0 RANGE REDUCTION ----
# - Used in visualization to overlay series
# - Used in ML for models that are affected by feature magnitude (e.g. linear regression)

# * Normalize to Range (0,1) ----
# - INFO: recipes::step_range() is actually normalization to range(0,1)



# * Standardize to Mean = 0 (Center), SD = 1 (Scaling) -----
# - INFO: recipes::step_normalize() is actually standardization to mean = 0, sd = 1




# 4.0 IMPUTING & OUTLIER CLEANING ----
# - Imputation helps with filling gaps (if needed)
# - Outlier removal helps linear regression detect trend and reduces high leverage points
# WARNING: Make sure you check outliers against events 
# - usually there is a reason for large values

# * Imputation ----


# * Cleaning (Imputation + Outlier Removal) ----



# Outlier Effect - Before Cleaning



# Outlier Effect - After Cleaning




# 5.0 LAGS & DIFFERENCING -----
# - Used to go from growth to change
# - Makes a series "stationary" (potentially)
# - MOST IMPORTANT - Can possibly use lagged variables in a model, if lags are correlated & 

# * Lags ----
# - Often used for feature engineering
# - Autocorrelation
# - 



# * Differencing ----
# - Makes a series "stationary"
# - Used to get change
#   - Stock price changes
#   - Cumulative Revenue to change by day
#   - Total subs to change by day

# Cumulative Sum & Differencing



# Comparing Differences 



# Inversion 




# 6.0 FOURIER SERIES ----
# - Useful for incorporating seasonality & autocorrelation
# - BENEFIT: Don't need a lag, just need a frequency (based on your time index)

# * Vector (Single Fourier) ----


# * Augmenting (Multiple Fourier Series) ----




# 7.0 CONFINED INTERVAL FORECASTING ----
# - Showcase: log_interval_vec()
# - Transformation used to confine forecasts to a max/min interval

# * Data ----



# * Apply Transformation ----



# * Model ----



# * Create Future Data ----

 

# * Predict ----



# * Combine data ----



# * Invert Transformation ----


