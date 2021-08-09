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


# 1.0 PROPHET BOOST ----

# * Best Prophet Model ----


# Error - Broken Model


# Fixing a broken model



# * Boosting Prophet Models ----

# Recipes


# Model Spec


# Workflow





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
