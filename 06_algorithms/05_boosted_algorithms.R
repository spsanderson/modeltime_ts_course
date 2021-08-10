# BUSINESS SCIENCE UNIVERSITY
# DS4B 203-R: TIME SERIES FORECASTING FOR BUSINESS
# MODULE: BOOSTED ALGORITHMS

# GOAL: Understand Boosting Errors

# OBJECTIVES ----
# - Learn about modeling residual errors
# - Apply Prophet + XGBoost

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

# FUNCTIONS & MODELS ----
source("00_scripts/01_calibrate_and_plot.R")

model_fit_best_prophet <- read_rds("00_models/model_fit_best_prophet.rds")
model_fit_best_arima   <- read_rds("00_models/model_fit_best_arima.rds")

# TRAIN / TEST SPLITS ----

splits <- time_series_split(data_prepared_tbl, assess = "8 weeks", cumulative = TRUE)

splits %>%
    tk_time_series_cv_plan() %>%
    plot_time_series_cv_plan(optin_time, optins_trans)


# 1.0 PROPHET BOOST ---- best for daily or sub daily

# * Best Prophet Model ----
model_fit_best_prophet

model_fit_best_prophet$preproc$terms %>% formula()

# Error - Broken Model

calibrate_and_plot(
    model_fit_best_prophet
)

# Fixing a broken model
model_tbl_best_prophet <- modeltime_table(model_fit_best_prophet) %>%
    modeltime_refit(training(splits))

model_fit_best_prophet <- model_tbl_best_prophet$.model[[1]]
model_tbl_best_prophet %>% pluck(".model", 1)

calibrate_and_plot(
    model_fit_best_prophet
)

# * Boosting Prophet Models ----

# Recipes

recipe_spec_base <- artifacts_list$recipes$recipe_spec_base
recipe_spec_base_no_lag <- recipe_spec_base %>%
    step_rm(starts_with("lag_"))

recipe_spec_base_no_lag %>%
    prep() %>%
    juice() %>%
    glimpse()

# Model Spec

model_spec_prophet_boost <- prophet_boost(
    # Prophet Params
    changepoint_num    = 20,
    changepoint_range  = 0.8,
    seasonality_daily  = FALSE,
    seasonality_weekly = FALSE,
    seasonality_yearly = FALSE,
    
    # XGBoost params - You want to tune
    #mtry           = 6,
    min_n          = 20,
    tree_depth     = 3,
    learn_rate     = 0.2,
    loss_reduction = 0.15,
    trees          = 300
) %>%
    set_engine("prophet_xgboost")

# Workflow

set.seed(123)
wflw_fit_prophet_boost <- workflow() %>%
    add_model(model_spec_prophet_boost) %>%
    add_recipe(recipe_spec_base_no_lag) %>%
    fit(training(splits))

calibrate_and_plot(
    model_fit_best_prophet,
    wflw_fit_prophet_boost
)


# 2.0 ARIMA BOOST ----

model_fit_best_arima %>%
    calibrate_and_plot()

# * Best ARIMA Model ----

model_spec_arima_boost <- arima_boost(
    seasonal_period = 1,
    min_n = 20,
    tree_depth = 3,
    learn_rate = 0.25,
    loss_reduction = 0.15,
    trees = 300
) %>%
    set_engine("auto_arima_xgboost")

set.seed(123)
wflw_fit_arima_boost <- wflw_fit_prophet_boost %>%
    update_model(model_spec_arima_boost) %>%
    fit(training(splits))

calibrate_and_plot(
    wflw_fit_arima_boost,
    type = "testing"
)

# * Boosting ARIMA -----


# 3.0 MODELTIME EVALUATION ----

# * Modeltime ----

model_tbl <- modeltime_table(
    model_fit_best_prophet,
    wflw_fit_prophet_boost,
    
    model_fit_best_arima,
    wflw_fit_arima_boost
)



# * Calibration ----
calibration_tbl <- model_tbl %>%
    modeltime_calibrate(testing(splits))


# * Accuracy Test ----
calibration_tbl %>%
    modeltime_accuracy()


# * Forecast Test ----
calibration_tbl %>%
    modeltime_forecast(
        new_data = testing(splits)
        , actual_data = data_prepared_tbl
    ) %>% 
    plot_modeltime_forecast(.conf_interval_show = FALSE)


# * Refit ----

refit_tbl <- calibration_tbl %>%
    modeltime_refit(data = data_prepared_tbl)

refit_tbl %>%
    modeltime_forecast(
        new_data = artifacts_list$data$forecast_tbl
        , actual_data = data_prepared_tbl
    ) %>%
    plot_modeltime_forecast(.conf_interval_show = FALSE)

# 4.0 SAVE ARTIFACTS ----

calibration_tbl %>%
    write_rds("00_models/calibration_tbl_boosted_models.rds")
