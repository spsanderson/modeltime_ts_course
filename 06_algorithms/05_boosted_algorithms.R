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
    changepoint_num = 25,
    changepoint_range = 0.8,
    
    # XGBoost params
    min_n = l,
    tree_depth = 6,
    learn_rate = 0.3
) %>%
    set_engine("prophet_xgboost")

# Workflow

set.seed(123)
wflw_fit_prophet_boost <- workflow() %>%
    add_model(model_spec_prophet_boost) %>%
    add_recipe(recipe_spec_base_no_lag) %>%
    fit(training(splits))




# 2.0 ARIMA BOOST ----


# * Best ARIMA Model ----



# * Boosting ARIMA -----



# 3.0 MODELTIME EVALUATION ----

# * Modeltime ----




# * Calibration ----



# * Accuracy Test ----



# * Forecast Test ----



# * Refit ----


# 4.0 SAVE ARTIFACTS ----
