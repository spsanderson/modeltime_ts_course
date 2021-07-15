# BUSINESS SCIENCE UNIVERSITY
# DS4B 203-R: TIME SERIES FORECASTING FOR BUSINESS
# MODULE: PROPHET

# GOAL: Understand PROPHET

# OBJECTIVES ----
# - Describe PROPHET using Prophet Components
# - Inspect Key Parameters
# - Show Modeltime PROPHET

# LIBRARIES & SETUP ----

# Time Series ML
library(tidymodels)
library(modeltime)
library(prophet)

# Core 
library(tidyverse)
library(lubridate)
library(timetk)
library(plotly)

# DATA ----

artifacts_list <- read_rds("00_models/feature_engineering_artifacts_list.rds") 
data_prepared_tbl <- artifacts_list$data$data_prepared_tbl 

data_prepared_tbl

# TRAIN / TEST SPLITS ----

splits <- time_series_split(
    data_prepared_tbl
    , assess = "8 weeks"
    , cumulative = TRUE
    )

splits %>%
    tk_time_series_cv_plan() %>%
    plot_time_series_cv_plan(optin_time, optins_trans)


# 1.0 PROPHET MODEL ----

# * Modeltime Model ----

?prophet_reg

model_fit_prophet <- prophet_reg(
    changepoint_num = 25,
    changepoint_range = 0.8,
    seasonality_yearly = TRUE,
    seasonality_weekly = TRUE
) %>%
    set_engine("prophet") %>%
    fit(
        data = training(splits)
        , optins_trans ~ optin_time
    )

modeltime_table(
    model_fit_prophet
) %>%
    modeltime_calibrate(new_data = testing(splits)) %>%
    modeltime_forecast(
        new_data = testing(splits)
        , actual_data = data_prepared_tbl
    ) %>%
    plot_modeltime_forecast()

# 2.0 PROPHET CONCEPTS ----

# * Extract model ----
prophet_model <- model_fit_prophet$fit$models$model_1

prophet_fcst <- predict(prophet_model, 
                        newdata = training(splits) %>%
                            rename(ds = 1, y = 2))

# * Visualize prophet model ----

plot(prophet_model, prophet_fcst) +
    add_changepoints_to_plot(prophet_model)


# * Visualize Additive Components ----
prophet_plot_components(prophet_model, prophet_fcst)


# 3.0 XREGS ----

# * Model ----

model_fit_prophet_xregs <- prophet_reg(
    seasonality_yearly = TRUE
) %>%
    set_engine("prophet") %>%
    fit(
        optins_trans ~ optin_time + lab_event,
        data = training(splits)
    )


# * Calibration ----

calibration_tbl <- modeltime_table(
    model_fit_prophet,
    model_fit_prophet_xregs
) %>%
    modeltime_calibrate(testing(splits))


# * Forecast Test ----

calibration_tbl %>%
    modeltime_forecast(
        new_data = testing(splits),
        actual_data = data_prepared_tbl
    ) %>%
    plot_modeltime_forecast(
        .conf_interval_show = FALSE
    )


# * Accuracy Test ----

calibration_tbl %>%
    modeltime_accuracy()


# * Refit ----

calibration_tbl %>%
    modeltime_refit(
        data = data_prepared_tbl
    ) %>%
    modeltime_forecast(
        new_data = artifacts_list$data$forecast_tbl,
        actual_data = data_prepared_tbl
    ) %>%
    plot_modeltime_forecast(.conf_interval_show = FALSE)


# 4.0 Saving ----

model_fit_best_prophet <- calibration_tbl %>%
    slice(2) %>%
    pluck(".model", 1)

write_rds(model_fit_best_prophet, "00_models/model_fit_best_prophet.RDS")
