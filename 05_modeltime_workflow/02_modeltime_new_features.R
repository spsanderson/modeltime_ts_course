# BUSINESS SCIENCE UNIVERSITY
# DS4B 203-R: TIME SERIES FORECASTING FOR BUSINESS
# MODULE: NEW FEATURES - MODELTIME 0.1.0

# GOAL ----
# Showcase Modeltime's Flexibility 

# OBJECTIVES ----
# - Expedited Forecasting - Skip Calibrating / Refitting
# - Working with In-Sample & Out-of-Sample Data
# - NEW Residual Diagnositics

# LIBRARIES ----

# Time Series ML
library(tidymodels)
library(modeltime)

# Core
library(tidyverse)
library(timetk)
library(lubridate)

# DATA & ARTIFACTS ----

feature_engineering_artifacts_list <- read_rds("00_models/feature_engineering_artifacts_list.rds")

data_prepared_tbl    <- feature_engineering_artifacts_list$data$data_prepared_tbl
forecast_tbl         <- feature_engineering_artifacts_list$data$forecast_tbl
recipe_spec_1_spline <- feature_engineering_artifacts_list$recipes$recipe_spec_1
recipe_spec_2_lag    <- feature_engineering_artifacts_list$recipes$recipe_spec_2

# TRAIN / TEST ----

splits <- data_prepared_tbl %>%
    time_series_split(assess = "8 weeks", cumulative = TRUE)


# 1.0 EXPEDITED FORECASTING ----

# * Model Time Table ----
#   - Fitted on Full Dataset (No Train/Test)

model_fit_prophet <- prophet_reg(
    seasonality_yearly = TRUE
    , seasonality_weekly = TRUE
) %>%
    set_engine("prophet") %>%
    fit(optins_trans ~ optin_time, data = data_prepared_tbl)

model_spec_glmnet <- linear_reg(
    penalty = 0.1
    , mixture = 0.5
) %>%
    set_engine("glmnet")

wflw_fit_glmnet <- workflow() %>%
    add_model(model_spec_glmnet) %>%
    add_recipe(recipe = recipe_spec_2_lag) %>%
    fit(data_prepared_tbl)

model_tbl <- modeltime_table(
    model_fit_prophet
    , wflw_fit_glmnet
)
    
# * Make a Forecast ----
#   - No confidence intervals
model_tbl %>%
    modeltime_forecast(
        new_data = forecast_tbl
        , actual_data = data_prepared_tbl
    ) %>%
    plot_modeltime_forecast()

# * Visualize a Fitted Model ----
model_tbl %>%
    modeltime_forecast(
        new_data = data_prepared_tbl
        , actual_data = data_prepared_tbl
    ) %>%
    plot_modeltime_forecast()



# 2.0 CALIBRATION (In-Sample vs Out-of-Sample) ----

# * Refitting for Train/Test ----

calibration_tbl <- model_tbl %>%
    modeltime_refit(training(splits)) %>%
    modeltime_calibrate(testing(splits))

# * Accuracy ----

# Out-of-Sample 
calibration_tbl %>%
    modeltime_accuracy()


# In-Sample
calibration_tbl %>%
    modeltime_accuracy(new_data = training(splits) %>% drop_na())

# 3.0 RESIDUALS ----

residuals_out_tbl <- calibration_tbl %>%
    modeltime_residuals()

residuals_in_tbl <- calibration_tbl %>%
    modeltime_residuals(
        training(splits) %>% drop_na()
    )

calibration_tbl %>%
    modeltime_residuals()

calibration_tbl %>%
    modeltime_residuals(training(splits) %>% drop_na())

# * Time Plot ----

# Out-of-Sample 

residuals_out_tbl %>%
    plot_modeltime_residuals()

# In-Sample

residuals_in_tbl %>%
    plot_modeltime_residuals(
        .smooth = TRUE
        , .smooth_span = 1
        , .y_intercept = 0
        , .y_intercept_color = "blue"
    )


# * ACF Plot ----

# Out-of-Sample 
residuals_out_tbl %>%
    plot_modeltime_residuals(
        .type = "ACF"
    )

# In-Sample
residuals_in_tbl %>%
    plot_modeltime_residuals(
        .type = "ACF"
    )


# * Seasonality ----

# Out-of-Sample 
residuals_out_tbl %>%
    plot_modeltime_residuals(.type = "seasonality")

# In-Sample
residuals_in_tbl %>%
    plot_modeltime_residuals(.type = "seasonality")


