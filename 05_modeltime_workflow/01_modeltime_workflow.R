# BUSINESS SCIENCE UNIVERSITY
# DS4B 203-R: TIME SERIES FORECASTING FOR BUSINESS
# MODULE: MODELTIME WORKFLOW FOR FORECASTING

# GOAL ----
# Deep-dive into modeltime

# OBJECTIVES ----
# - Understand the Modeltime Workflow 
# - Understand Accuracy Measurements 
# - Understand the Forecast Horizon & Confidence Intervals
# - Why refit? 

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

recipe_spec_2_lag    <- feature_engineering_artifacts_list$recipes$recipe_spec_2

# TRAIN / TEST ----

splits <- data_prepared_tbl %>%
    time_series_split(assess = "8 weeks", 
                      # initial = "1 year 1 months"
                      cumulative = TRUE)

splits %>%
    tk_time_series_cv_plan() %>%
    plot_time_series_cv_plan(optin_time, optins_trans)



# 1.0 MAKING MODELS ----
# - Works with Parnsip & Workflows
# - Models must be fit (trained)

# * Parsnip Model (ARIMA)  ----

model_fit_arima <- arima_reg() %>%
    set_engine(engine = "auto_arima") %>%
    fit(optins_trans ~ optin_time, data = training(splits))

model_fit_arima

# * Workflow (ARIMA + Date Features) ----

model_spec_arima <- arima_reg() %>%
    set_engine("auto_arima")

recipe_spec_fourier <- recipe(optins_trans ~ optin_time, data = training(splits)) %>%
    step_fourier(optin_time, period = c(7,14, 30, 90), K = 1)

workflow_fit_arima <- workflow() %>%
    add_recipe(recipe_spec_fourier) %>%
    add_model(model_spec_arima) %>%
    fit(training(splits))

workflow_fit_arima

# * Workflow (GLMNET + XREGS) ----

model_spec_glmnet <- linear_reg(
    penalty = 0.1,
    mixture = 0.5
) %>%
    set_engine("glmnet")

workflow_fit_glmnet <- workflow() %>%
    add_recipe(recipe_spec_2_lag) %>%
    add_model(model_spec_glmnet) %>%
    fit(training(splits))

# 2.0 MODELTIME TABLE ----
# - Organize

model_tbl <- modeltime_table(
    model_fit_arima,
    workflow_fit_arima,
    workflow_fit_glmnet
) %>%
    update_model_description(
        3, "GLMNET - LAG RECIPE"
    )


# 3.0 CALIBRATION ----
# - Calculates residual model errors on test set
# - Gives us a true prediction error estimate when we model with confidence intervals

calibration_tbl <- model_tbl %>%
    modeltime_calibrate(
        new_data = testing(splits)
    )

# 4.0 TEST ACCURACY ----
# - Calculates common accuracy measures
# - MAE, MAPE, MASE, SMAPE, RMSE, R-SQUARED

calibration_tbl %>%
    modeltime_accuracy()

calibration_tbl %>% 
    modeltime_accuracy(
        metric_set = default_forecast_accuracy_metric_set()
    ) %>%
    table_modeltime_accuracy(.interactive = FALSE)


# 5.0 TEST FORECAST ----
# - Visualize the out-of-sample forecast
calibration_tbl %>%
    modeltime_forecast(
        actual_data = data_prepared_tbl
        , new_data = testing(splits)
        , conf_interval = 0.8
    ) %>%
    plot_modeltime_forecast()


# 6.0 REFITTING ----

# * Refit ----

refit_tbl <- calibration_tbl %>%
    modeltime_refit(
        data = data_prepared_tbl
    )

# * Final Forecast ----
# - 'new_data' vs 'h'
# - 'actual_data'
# - Preprocessing

refit_tbl %>%
    modeltime_forecast(
        actual_data = data_prepared_tbl,
        new_data = forecast_tbl
    ) %>%
    plot_modeltime_forecast(
        .conf_interval_fill = "light blue"
        , .interactive = FALSE
    )

