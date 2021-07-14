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
train_diff_tbl <- train_tbl %>%
    mutate(optins_trans = diff_vec(optins_trans, lag = 1, difference = 1)) %>%
    drop_na()

arima(train_tbl$optins_trans, order = c(1,1,0))
arima(train_diff_tbl$optins_trans, order = c(1,0,0))

lm(optins_trans ~ lag_vec(optins_trans, 1), data = train_diff_tbl)

# * ARIMA (MA) = Modeling Errors ----
arima(train_tbl$optins_trans, order = c(1,0,1))

fit_lm_ar1 <- lm(optins_trans ~ lag_vec(optins_trans, 1), data = train_tbl)

fitted_values_vec <- predict(fit_lm_ar1) %>% as.numeric()

train_error_tbl <- train_tbl %>%
    slice(-1) %>%
    mutate(error = optins_trans - fitted_values_vec)

lm(optins_trans ~ lag_vec(optins_trans, 1) + lag_vec(error, 1)
   , data = train_error_tbl)

# * Seasonal ARIMA ----
arima(
    train_tbl$optins_trans,
    order = c(1,0,0),
    seasonal = list(
        order = c(2,0,0),
        period = 7
    )
)

lm(
    optins_trans ~ lag_vec(optins_trans, 1) +
        lag_vec(optins_trans, 7)+
        lag_vec(optins_trans, 14),
    data = train_tbl
)

# * SARIMAX - Seasonal Regression w/ ARIMA Errors ----

arima(
    train_tbl$optins_trans,
    order    = c(1,0,0),
    seasonal = list(
        order  = c(2,0,0),
        period = 7
    ),
    xreg = matrix(month(train_tbl$optin_time))
)

lm(
    optins_trans ~ lag_vec(optins_trans, 1) +
        lag_vec(optins_trans, 7) +
        lag_vec(optins_trans, 14) +
        month(optin_time),
    data = train_tbl
)


# 2.0 ARIMA ----

model_fit_arima <- arima_reg(
    seasonal_period = 7,
    seasonal_ar = 1,
    seasonal_differences = 1,
    seasonal_ma = 1,
    non_seasonal_ar = 1,
    non_seasonal_differences = 1,
    non_seasonal_ma = 1
) %>%
    set_engine("arima") %>%
    fit(optins_trans ~ optin_time, training(splits))

modeltime_table(
    model_fit_arima
) %>%
    modeltime_calibrate(testing(splits)) %>%
    modeltime_forecast(
        new_data = testing(splits),
        actual_data = data_prepared_tbl
    ) %>%
    plot_modeltime_forecast()

# 3.0 AUTO ARIMA + XREGS ----

# * Model ----

model_fit_auto_arima <- arima_reg() %>%
    set_engine("auto_arima") %>%
    fit(
        optins_trans ~ .
        + fourier_vec(optin_time, period = 7)
        + fourier_vec(optin_time, period = 14)
        + fourier_vec(optin_time, period = 30)
        + fourier_vec(optin_time, period = 90)
        + month(optin_time, label = TRUE),
        data = training(splits)
    )


# * Calibrate ----

calibration_tbl <- modeltime_table(
    model_fit_arima,
    model_fit_auto_arima
) %>%
    modeltime_calibrate(testing(splits))

# * Forecast Test ----

calibration_tbl %>%
    modeltime_forecast(
        new_data = testing(splits)
        , actual_data = data_prepared_tbl
    ) %>%
    plot_modeltime_forecast()

# * Accuracy Test -----

calibration_tbl %>%
    modeltime_accuracy()

# * Refit ----
refit_tbl <- calibration_tbl %>%
    modeltime_refit(
        data = data_prepared_tbl
    )

refit_tbl %>%
    modeltime_forecast(
        new_data = artifacts_list$data$forecast_tbl,
        actual_data = data_prepared_tbl
    ) %>%
    plot_modeltime_forecast(
        .conf_interval_alpha = 0.05
    )

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
model_fit_best_arima <- calibration_tbl %>%
    slice(2) %>%
    pull(.model) %>%
    pluck(1)
write_rds(model_fit_best_arima, "00_models/model_fit_best_arima.RDS")
