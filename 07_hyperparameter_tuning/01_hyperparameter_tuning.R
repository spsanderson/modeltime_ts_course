# BUSINESS SCIENCE UNIVERSITY
# DS4B 203-R: TIME SERIES FORECASTING FOR BUSINESS
# MODULE: HYPERPARAMETER TUNING 

# SPECIAL REQUIREMENTS:
# - Parallel Processing: Requires development versions of:
#   tune, recipes, workflows, modeltime, and timetk

# GOAL: Tune Models that Generalize Better

# OBJECTIVES ----
# - Sequential (e.g. ARIMA, ETS, TBATS) vs Non-Sequential Algorithms (e.g. Prophet, ML)
# - Cross-Validation Workflow - K-FOLD vs TSCV
# - Hyperparameter tuning with Prophet Boost
# - Parallel Processing with doFuture for 3X-5X Speedup


# LIBRARIES & SETUP ----

# Time Series ML
library(tidymodels)
library(modeltime)
library(rules)

# Timing & Parallel Processing
library(tictoc)
library(future)
library(doFuture)

# Core 
library(plotly)
library(tidyverse)
library(lubridate)
library(timetk)
library(plotly)


# DATA ----

artifacts_list    <- read_rds("00_models/feature_engineering_artifacts_list.rds") 
data_prepared_tbl <- artifacts_list$data$data_prepared_tbl 
data_prepared_tbl


# MODELS ----

source("00_scripts/01_calibrate_and_plot.R")

calibration_ml_tbl      <- read_rds("00_models/machine_learning_calibration_tbl.rds")
calibration_boosted_tbl <- read_rds("00_models/calibration_tbl_boosted_models.rds")
calibration_ets_tbl     <- read_rds("00_models/calibration_tbl_ets_tbats.rds")

# TRAIN / TEST SPLITS ----

splits <- time_series_split(data_prepared_tbl, assess = "8 weeks", cumulative = TRUE)

splits %>%
    tk_time_series_cv_plan() %>%
    plot_time_series_cv_plan(optin_time, optins_trans)


# 1.0 REVIEW  ----

# * Combine Tables ----



# * Review Accuracy ----



# [SEQUENTIAL MODEL] ----
# 2.0 NNETAR  ----
# - Sequential Model Definition: 
#   - Creates Lags internally
#   - [IMPORTANT DIFFERENTIATION]: Predicts next H observations
#   - All data must be sequential
#   - Cannot use K-Fold Cross Validation / Must use Time Series Cross Validation
# - Examples of Sequential Models:
#   - ARIMA
#   - Exponential Smoothing
#   - NNETAR
#   - Any algorithm from the forecast package

# * Extract Fitted Model ----



# * Cross Validation Plan (TSCV) -----
# - Time Series Cross Validation


# * Recipe ----



# * Model Spec ----



# * Grid Spec ----
# - Random Process 



# * Tune ----
# - Expensive Operation
# - Parallel Processing is essential


# Workflow - Tuning



# Run Tune Grid (Expensive Operation) 


# ** Setup Parallel Processing ----



# ** TSCV Cross Validation ----



# ** Reset Sequential Plan ----



# Show Results



# Visualize Results



# * Retrain & Assess -----



# [NON-SEQUENTIAL] ----
# 3.0 PROPHET BOOST   -----
# - Non-Sequential Model:
#   - Uses date features
#   - Lags Created * Externally * (We Provide)
#   - Spline can be modeled with random missing observations
#   - Therefore can be Tuned using K-Fold Cross Validation
#   - IMPORTANT: Experiment to see which gives better results
# - Other Examples:
#   - Machine Learning Algorithms that use Calendar Features (e.g. GLMNet, XGBoost)
#   - Prophet
# - IMPORTANT: Can use time_series_cv() or vfold_cv(). Usually better performance with vfold_cv().


# * Extract Fitted Model ----



# * Cross Validation Plans (K-Fold) ----
# - Prophet is a non-sequential model. We can randomize time observations.
# - K-Fold is OK
# - Should set.seed() because random process



# * Recipe Spec ----



# * Model Spec ----



# * Grid Spec ----



# * Tune ----

# ** Setup Parallel Processing ----



# ** K-Fold Cross Validation ----



# ** Reset Sequential Plan ----



# Show Best



# Visualize



# * Retrain & Assess ----






# 4.0 SAVE ARTIFACTS ----




