# BUSINESS SCIENCE UNIVERSITY
# DS4B 203-R: TIME SERIES FORECASTING FOR BUSINESS
# MODULE: MACHINE LEARNING

# GOAL: Understand Machine Learning Algorithms

# OBJECTIVES ----
# - Exposure to key ML algorithms
# - Inspect Key Parameters
# - Show Modeltime Workflow

# LIBRARIES & SETUP ----

# Time Series ML
library(tidymodels)
library(modeltime)
library(rules)

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

# RECIPES ----

recipe_spec_base     <- artifacts_list$recipes$recipe_spec_base
recipe_spec_base

recipe_spec_1_spline <- artifacts_list$recipes$recipe_spec_1
recipe_spec_1_spline

recipe_spec_2_lag    <- artifacts_list$recipes$recipe_spec_2
recipe_spec_2_lag

training(splits) %>% glimpse()

recipe_spec_1_spline %>% prep() %>% juice() %>% glimpse()

# 1.0 ELASTIC NET REGRESSION ----
# - Strengths: Very good for trend
# - Weaknesses: Not as good for complex patterns (i.e. seasonality)

model_spec_glmnet <- linear_reg(
    mode = "regression"
    , penalty = 0.01
    , mixture = 0.5
) %>%
    set_engine("glmnet")

# Spline

wflw_fit_glmnet_spline <- workflow() %>%
  add_model(model_spec_glmnet) %>%
  add_recipe(recipe_spec_1_spline) %>%
  fit(training(splits))


# Lag

wflw_fit_glmnet_lag <- workflow() %>%
  add_model(model_spec_glmnet) %>%
  add_recipe(recipe_spec_2_lag) %>%
  fit(training(splits))


# Calibrate & Plot
calibration_tbl <- modeltime_table(
  wflw_fit_glmnet_spline,
  wflw_fit_glmnet_lag
) %>%
  update_model_description(1, "GLMNET_Spline") %>%
  update_model_description(2, "GLMNET_Lag") %>%
  modeltime_calibrate(testing(splits))

calibration_tbl %>%
  modeltime_accuracy()

calibration_tbl %>%
  modeltime_forecast(
    new_data = testing(splits),
    actual_data = data_prepared_tbl
  ) %>%
  plot_modeltime_forecast(
    .conf_interval_show = FALSE
  )

# *** PLOTTING UTILITY *** ----
# - Calibrate & Plot
calibrate_and_plot <- function(..., type = "testing"){
  
  if(type == "testing"){
    new_data = testing(splits)
  } else {
    new_data = training(splits) %>%
      drop_na()
  }
  
  calibration_tbl <- modeltime_table(...) %>%
    modeltime_calibrate(new_data)
  
  print(calibration_tbl %>% modeltime_accuracy())
  
  calibration_tbl %>%
    modeltime_forecast(
      new_data = new_data
      , actual_data = data_prepared_tbl
    ) %>%
    plot_modeltime_forecast(
      .conf_interval_show = FALSE
    )
}

calibrate_and_plot(
  wflw_fit_glmnet_spline,
  wflw_fit_glmnet_lag,
  type = "testing"
)

# 2.0 MARS ----
# Multiple Adaptive Regression Splines
# - Strengths: Best algorithm for modeling trend
# - Weaknesses: 
#   - Not good for complex patterns (i.e. seasonality)
#   - Don't combine with splines! MARS makes splines.
# - Key Concept: Can combine with xgboost (better seasonality detection)
#   - prophet_reg: uses a technique similar to mars for modeling trend component
#   - prophet_boost: Uses prophet for trend, xgboost for features

model_spec_mars <- mars(
  mode = "regression"
  , num_terms = 10
  , prod_degree = 2
) %>%
  set_engine("earth", endspan = nrow(testing(splits)))


# Simple Numeric
wflw_fit_mars_simple <- workflow() %>%
  add_model(model_spec_mars) %>%
  add_formula(optins_trans ~ as.numeric(optin_time)) %>%
  fit(training(splits))

calibrate_and_plot(
  wflw_fit_mars_simple
  , type = "testing"
)

# Spline
wflw_fit_mars_spline <- workflow() %>%
  add_model(model_spec_mars) %>%
  add_recipe(recipe_spec_1_spline) %>%
  fit(training(splits))


# Lag
wflw_fit_mars_lag <- workflow() %>%
  add_model(model_spec_mars) %>%
  add_recipe(recipe_spec_2_lag) %>%
  fit(training(splits))

# Calibrate & Plot
calibrate_and_plot(
  wflw_fit_mars_simple,
  wflw_fit_mars_spline,
  wflw_fit_mars_lag
)

# 3.0 SVM POLY ----
# Strengths: Well-rounded algorithm
# Weaknesses: Needs tuned or can overfit



# Spline


# Lag


# Calibrate & Plot



# 4.0 SVM RADIAL BASIS ----
# Strengths: Well-rounded algorithm
# Weaknesses: Needs tuned or can overfit


# Spline


# Lag


# Calibrate & Plot



# 5.0 K-NEAREST NEIGHBORS ----
# - Strengths: Uses neighboring points to estimate 
# - Weaknesses: Cannot predict beyond the maximum/minimum target (e.g. increasing trend)
# - Solution: Model trend separately (if needed). 
#   - Can combine with ARIMA, Linear Regression, Mars, or Prophet

# Show issue when trend extends beyond maximum value ----


# Implementation ----

# Spline


# Lag


# Calibrate & Plot

# 6.0 RANDOM FOREST ----
# - Strengths: Can model seasonality very well
# - Weaknesses: 
#   - Cannot predict beyond the maximum/minimum target (e.g. increasing trend)
# - Solution: Model trend separately (if needed). 
#   - Can combine with ARIMA, Linear Regression, Mars, or Prophet

# Implementation

# Spline


# Lag


# Calibrate & Plot

# 7.0 XGBOOST ----
# - Strengths: Best for seasonality & complex patterns
# - Weaknesses: 
#   - Cannot predict beyond the maximum/minimum target (e.g. increasing trend)
# - Solution: Model trend separately (if needed). 
#   - Can combine with ARIMA, Linear Regression, Mars, or Prophet
#   - prophet_boost & arima_boost: Do this

# Implementation

# Spline


# Lag


# Calibrate & Plot


# 8.0 CUBIST ----
# - Like XGBoost, but the terminal (final) nodes are fit using linear regression
# - Does better than tree-based algorithms when time series has trend
# - Can predict beyond maximum



# Implementation 

# Spline


# Lag


# Calibrate & Plot



# 9.0 NEURAL NET ----
# - Single Layer Multi-layer Perceptron Network
# - Simple network - Like linear regression
# - Can improve learning by adding more hidden units, epochs, etc

# Spline


# Lag


# Calibrate & Plot


# 10.0 NNETAR ----
# - NNET with Lagged Features (AR)
# - Is a sequential model (comes from the forecast package)
# - Must include date feature

# Base Model


# Calibrate & Plot



# 11.0 Modeltime Forecasting Workflow -----
# - Compare model performance

# * Modeltime Table ----


# * Calibration Table ----


# * Obtain Test Forecast Accuracy ----


# * Visualize Test Forecast ----


# * Refit ----
  
