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
google_analytics_summary_long_tbl %>%
    mutate(value_roll = slidify_vec(
        .x = value
        , .f = mean
        , .period = 24 * 7
        , .align = "center"
        , .partial = FALSE
    )) %>%
    pivot_longer(contains("value"), names_repair = "unique") %>%
    rename(names = `name...2`, names_2 = `name...3`) %>%
    group_by(names) %>%
    plot_time_series(
        .date_var = date
        , .value = value
        , .color_var = names_2
        , .smooth = FALSE
    )


# * LOESS smoother ----
google_analytics_summary_long_tbl %>%
    mutate(value_smooth = smooth_vec(x = value, period = 24 * 7)) %>%
    pivot_longer(contains("value"), names_repair = "unique") %>%
    rename(names = `name...2`, names_2 = `name...3`) %>%
    group_by(names) %>%
    plot_time_series(
        .date_var = date
        , .value = value
        , .color_var = names_2
        , .smooth = FALSE
    )


# * Rolling Correlations ----
# - Identify changing relationships
rolling_cor_24_7 <- slidify(
    .f = ~ cor(.x, .y, use = "pairwise.complete.obs")
    , .period  = 24 * 7
    , .align   = "right"
    , .partial = FALSE
)

google_analytics_summary_tbl %>%
    mutate(rolling_cor_pageviews_organic = rolling_cor_24_7(pageViews, organicSearches)) %>%
    mutate(dateHour = ymd_h(dateHour)) %>%
    plot_time_series(
        .date_var = dateHour
        , .value = rolling_cor_pageviews_organic
    )
    
rolling_cor_24_7_b <- slidify(
    .f = ~ cor(.x, .y, use = "pairwise.complete.obs")
    , .period = 24 * 7
    , .align = "center"
    , .partial = FALSE
)

google_analytics_summary_tbl %>%
    mutate(rolling_cor_pageview_organic = rolling_cor_24_7_b(pageViews, organicSearches)) %>%
    mutate(dateHour = ymd_h(dateHour)) %>%
    select(-sessions) %>%
    pivot_longer(-dateHour) %>%
    group_by(name) %>%
    plot_time_series(.date_var = dateHour, .value = value)

# * Problem with Moving Avg Forecasting ----
transactions_tbl %>%
    mutate(mavg_8wks = slidify_vec(
        .x = revenue
        , .f = ~ mean(.x, na.rm = TRUE)
        , .period = 8
        , .align = "right"
        )
    ) %>%
    bind_rows(
        future_frame(., .length_out = 8)
    ) %>%
    fill(mavg_8wks, .direction = "down") %>%
    pivot_longer(-purchased_at) %>%
    plot_time_series(
        .date_var = purchased_at
        , .value = value
        , .color_var = name
        , .smooth = FALSE
    )



# 3.0 RANGE REDUCTION ----
# - Used in visualization to overlay series
# - Used in ML for models that are affected by feature magnitude (e.g. linear regression)

# * Normalize to Range (0,1) ----
# - INFO: recipes::step_range() is actually normalization to range(0,1)
google_analytics_summary_long_tbl %>%
    mutate(value = normalize_vec(value)) %>%
    ungroup() %>%
    plot_time_series(
        .date_var = date
        , .value = value
        , .color_var = name
    ) 


# * Standardize to Mean = 0 (Center), SD = 1 (Scaling) -----
# - INFO: recipes::step_normalize() is actually standardization to mean = 0, sd = 1
google_analytics_summary_long_tbl %>%
    mutate(value = standardize_vec(value)) %>%
    ungroup() %>%
    plot_time_series(
        .date_var = date
        , .value = value
        , .color_var = name
    )



# 4.0 IMPUTING & OUTLIER CLEANING ----
# - Imputation helps with filling gaps (if needed)
# - Outlier removal helps linear regression detect trend and reduces high leverage points
# WARNING: Make sure you check outliers against events 
# - usually there is a reason for large values

# * Imputation ----
subscribers_daily_tbl %>%
    mutate(optins_na = ifelse(optins == 0, NA_real_, optins)) %>%
    mutate(optins_imputed = ts_impute_vec(optins_na, period = 7)) %>%
    pivot_longer(-optin_time) %>%
    plot_time_series(
        .date_var = optin_time
        , .value = log1p(value)
        , .color_var = name
        , .smooth = FALSE
    )

# * Cleaning (Imputation + Outlier Removal) ----
subscribers_daily_tbl %>%
    mutate(optins_na = ifelse(optins == 0, NA_real_, optins)) %>%
    mutate(optins_clean = ts_clean_vec(optins_na, period = 7)) %>%
    pivot_longer(-optin_time) %>%
    # plot_anomaly_diagnostics(
    #     .date_var = optin_time
    #     , .value = value
    # )
    plot_time_series(
        .date_var = optin_time
        , .value = value
        , .color_var = name
        , .smooth = FALSE
    )


# Outlier Effect - Before Cleaning
subscribers_cleaned_daily_tbl <- subscribers_daily_tbl %>%
    mutate(optins_na = ifelse(optins == 0, NA_real_, optins)) %>%
    mutate(optins_clean = ts_clean_vec(optins_na, period = 7))

# We can invert the following with exp1
subscribers_cleaned_daily_tbl %>%
    plot_time_series_regression(
        .date_var = optin_time
        , .formula = log1p(optins) ~ as.numeric(optin_time) +
            wday(optin_time, label = TRUE) +
            month(optin_time, label = TRUE)
        , .show_summary = TRUE
    )

# we cannot invert this data
subscribers_cleaned_daily_tbl %>%
    plot_time_series_regression(
        .date_var = optin_time
        , .formula = optins_clean ~ as.numeric(optin_time) +
            wday(optin_time, label = TRUE) +
            month(optin_time, label = TRUE)
        , .show_summary = TRUE
    )


# Outlier Effect - After Cleaning




# 5.0 LAGS & DIFFERENCING -----
# - Used to go from growth to change
# - Makes a series "stationary" (potentially)
# - MOST IMPORTANT - Can possibly use lagged variables in a model, if lags are correlated & 

# * Lags ----
# - Often used for feature engineering
# - Autocorrelation
# - 
subscribers_daily_tbl %>%
    mutate(optins_lag_1 = lag_vec(optins, lag = 1)) %>%
    mutate(optins_lag_2 = lag_vec(optins, lag = 2))

subscribers_daily_tbl %>%
    plot_acf_diagnostics(
        .date_var = optin_time
        , .value = log1p(optins)
    )

subscribers_daily_tbl %>%
    tk_augment_lags(
        .value = optins
        , .lags = c(1,2,6,14)
    ) %>%
    drop_na() %>%
    plot_time_series_regression(
        .date_var = optin_time
        , .formula = log1p(optins) ~ as.numeric(optin_time) +
            log1p(optins_lag1) +
            log1p(optins_lag2) +
            log1p(optins_lag6) +
            log1p(optins_lag14)
        , .show_summary = TRUE
    )

# * Differencing ----
# - Makes a series "stationary"
# - Used to get change
#   - Stock price changes
#   - Cumulative Revenue to change by day
#   - Total subs to change by day

# Cumulative Sum & Differencing
subscribers_daily_tbl %>%
    mutate(total_optins = cumsum(optins)) %>%
    mutate(optins_diff_1 = diff_vec(total_optins, lag = 1)) %>% # Instantaneous Velocity
    mutate(optins_diff_2 = diff_vec(total_optins, lag = 1, difference = 2)) %>% # Acceleration
    pivot_longer(-optin_time) %>%
    group_by(name) %>%
    plot_time_series(optin_time, value, .color_var = name, .smooth = FALSE)

subscribers_daily_tbl %>%
    mutate(total_optins = cumsum(optins)) %>%
    mutate(optins_diff_1 = diff_vec(total_optins, lag = 1)) %>% # Instantaneous Velocity
    mutate(optins_diff_2 = diff_vec(total_optins, lag = 1, difference = 2)) %>% # Acceleration
    pivot_longer(-optin_time) %>%
    group_by(name) %>%
    plot_time_series(optin_time, log1p(value), .color_var = name, .smooth = FALSE)

# Comparing Differences 
google_analytics_summary_diff_tbl <-  google_analytics_summary_tbl %>%
    mutate(across(pageViews:sessions, .fns = diff_vec))

google_analytics_summary_diff_tbl %>%
    mutate(dateHour = ymd_h(dateHour)) %>%
    pivot_longer(-dateHour) %>%
    plot_time_series(dateHour, value, name, .smooth = FALSE)

# Inversion 
google_analytics_summary_diff_tbl %>%
    mutate(pageViews = diff_inv_vec(pageViews, initial_values = 79))



# 6.0 FOURIER SERIES ----
# - Useful for incorporating seasonality & autocorrelation
# - BENEFIT: Don't need a lag, just need a frequency (based on your time index)

# * Vector (Single Fourier) ----
subscribers_daily_tbl %>%
    mutate(sin14_k1 = fourier_vec(optin_time, period = 14, K = 1, type = "sin")) %>%
    mutate(cos14_k2 = fourier_vec(optin_time, period = 14, K = 1, type = "cos")) %>%
    select(-optins) %>%
    pivot_longer(matches("(cos)|(sin)")) %>%
    plot_time_series(
        optin_time
        , value
        , name
        , .smooth = FALSE
    )


# * Augmenting (Multiple Fourier Series) ----
subscribers_daily_tbl %>%
    tk_augment_fourier(optin_time, .periods = c(14,30,90,365), .K = 2) %>%
    plot_time_series_regression(
        optin_time
        , .formula = log1p(optins) ~ as.numeric(optin_time) +
            . - optin_time
        , .show_summary = TRUE
    )



# 7.0 CONFINED INTERVAL FORECASTING ----
# - Showcase: log_interval_vec()
# - Transformation used to confine forecasts to a max/min interval

# * Data ----
1:10

values_transformed_vec <- log_interval_vec(1:10, limit_lower = 0, limit_upper = 15)

values_transformed_vec %>% log_interval_inv_vec(limit_lower = 0, limit_upper = 15)

new_values_transformed_vec <- c(values_transformed_vec, c(.75,1.5,2.0, 10.0))

new_values_transformed_vec %>% plot()

new_values_transformed_vec %>% log_interval_inv_vec(0, 15) %>% plot()

limit_lower <- 0
limit_upper <- 3650.8
offset      <- 1

subscribers_daily_tbl %>%
    plot_time_series(
        optin_time
        , log_interval_vec(
            optins
            , limit_lower = limit_lower
            , limit_upper = limit_upper
            , offset = offset))

# * Apply Transformation ----

fourier_periods <- c(6,14,30,90,365)
fourier_order <- 5

data_transformed_tbl <- subscribers_daily_tbl %>%
    mutate(optins_trans = log_interval_vec(optins, limit_lower = limit_lower,
                                           limit_upper = limit_upper,
                                           offset = offset)) %>%
    tk_augment_fourier(
        .date_var  = optin_time
        , .periods = fourier_periods
        , .K = fourier_order
    ) %>%
    select(-optins)


# * Model ----
model_formula <- as.formula(optins_trans ~ as.numeric(optin_time) + 
                                wday(optin_time, label = TRUE) +
                                month(optin_time, label = TRUE) +
                                . -optin_time)

data_transformed_tbl %>%
    plot_time_series_regression(
        .date_var = optin_time
        , .formula = model_formula
        , .show_summary = TRUE
    )

model_fit_lim <- lm(formula = model_formula, data = data_transformed_tbl)
    
# * Create Future Data ----
future_tbl <- data_transformed_tbl %>%
    future_frame(
        .date_var     = optin_time
        , .length_out = "6 months"
    ) %>%
    tk_augment_fourier(
        .date_var = optin_time
        , .periods  = fourier_periods
        , .K      = fourier_order
    )
    
future_tbl 

# * Predict ----
predictions <- predict(
    object = model_fit_lim
    , newdata = future_tbl
  ) %>%
  as.vector()


# * Combine data ----
conf_interval <- 0.95
residuals <- model_fit_lim$residuals %>% as.vector()
alpha <- (1 - conf_interval)/2
abs_margin_error <- abs(qnorm(alpha) * sd(residuals))

forecast_tbl <- data_transformed_tbl %>%
    select(optin_time, optins_trans) %>%
    add_column(type = "actual") %>%
    bind_rows(
        future_tbl %>%
            select(optin_time) %>%
            mutate(
                optins_trans = predictions
                , type       = "prediction"
            ) %>%
            mutate(
                conf_lo   = optins_trans - abs_margin_error
                , conf_hi = optins_trans + abs_margin_error
            )
    )

forecast_tbl %>%
    pivot_longer(cols = c(optins_trans, conf_lo, conf_hi)) %>%
    plot_time_series(
        .date_var = optin_time
        , .value = value
        , .color_var = name
        , .smooth = FALSE
    )

# * Invert Transformation ----

forecast_tbl %>%
    pivot_longer(cols = c(optins_trans, conf_lo, conf_hi)) %>%
    plot_time_series(
        .date_var = optin_time
        , .value = log_interval_inv_vec(
            x = value
            , limit_lower = limit_lower
            , limit_upper = limit_upper
            , offset      = offset
        )
        , .color_var = name
        , .smooth    = FALSE
    )

