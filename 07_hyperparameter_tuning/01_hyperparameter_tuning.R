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
model_tbl <- combine_modeltime_tables(
    calibration_ml_tbl,
    calibration_ets_tbl,
    calibration_boosted_tbl
)

# * Review Accuracy ----

calibration_tbl <- model_tbl %>%
    modeltime_refit(training(splits)) %>%
    modeltime_calibrate(testing(splits))


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
wflw_fit_nnetar <- calibration_tbl %>%
    pluck_modeltime_model(19)

wflw_fit_nnetar

# * Cross Validation Plan (TSCV) -----
# - Time Series Cross Validation

resamples_tscv_lag <- time_series_cv(
    data        = training(splits) %>% drop_na(),
    date_var    = optin_time,
    cumulative  = TRUE,
    assess      = "8 weeks",
    skip        = "4 weeks",
    slice_limit = 6
)

resamples_tscv_lag %>%
    tk_time_series_cv_plan() %>%
    plot_time_series_cv_plan(optin_time, optins_trans, .facet_ncol = 2)

# * Recipe ----

recipe_spec_3_lag_date <- wflw_fit_nnetar %>% 
    extract_preprocessor() %>%
    step_naomit(starts_with("lag"))

# * Model Spec ----
wflw_fit_nnetar %>%
    extract_spec_parsnip()

model_spec_nnetar <- nnetar_reg(
    seasonal_period = 7,
    non_seasonal_ar = tune(id = "non_seasonal_ar"),
    seasonal_ar     = tune(),
    hidden_units    = tune(),
    num_networks    = 10,
    penalty         = tune(),
    epochs          = 50
) %>%
    set_engine("nnetar")

# * Grid Spec ----
# - Random Process 

parameters(model_spec_nnetar)

set.seed(123)
grid_random(parameters(model_spec_nnetar), size = 10)

# Round 1

set.seed(123)
grid_spec_nnetar_1 <- grid_latin_hypercube(
    parameters(model_spec_nnetar)
    , size = 15
)


# Round 2

set.seed(123)
grid_spec_nnetar_2 <- grid_latin_hypercube(
    non_seasonal_ar(range = c(1,3)),
    seasonal_ar(range = c(0,1)),
    hidden_units(range= c(2,5)),
    penalty(range = c(-4.8, -2.9), trans = scales::log10_trans()),
    size = 15
)

grid_spec_nnetar_2

# * Tune ----
# - Expensive Operation
# - Parallel Processing is essential

#?tune_grid

# Workflow - Tuning

wflw_tune_nnetar <- wflw_fit_nnetar %>%
    update_recipe(recipe_spec_3_lag_date) %>%
    update_model(model_spec_nnetar)



# Run Tune Grid (Expensive Operation) 

# ** Setup Parallel Processing ----

registerDoFuture()

plan(
    strategy = cluster,
    workers  = parallel::makeCluster(parallel::detectCores() - 1)
)
# ** TSCV Cross Validation ----

set.seed(123)
tic()
tune_results_nnetar_1 <- wflw_tune_nnetar %>%
    tune_grid(
        resamples = resamples_tscv_lag,
        grid      = grid_spec_nnetar_1,
        metrics   = default_forecast_accuracy_metric_set(),
        control   = control_grid(
            verbose   = TRUE,
            save_pred = TRUE
        )
    )
toc()

tune_results_nnetar_1

# Round 2
set.seed(123)
tic()
tune_results_nnetar_2 <- wflw_tune_nnetar %>%
    tune_grid(
        resamples = resamples_tscv_lag,
        grid      = grid_spec_nnetar_2,
        metrics   = default_forecast_accuracy_metric_set(),
        control   = control_grid(
            verbose   = TRUE,
            save_pred = TRUE
        )
    )
toc()

tune_results_nnetar_2

# ** Reset Sequential Plan ----

plan(strategy = sequential)

# Show Results

tune_results_nnetar_1 %>% show_best(metric = "rmse")
tune_results_nnetar_2 %>% show_best(metric = "rmse")

# Visualize Results

g1 <- tune_results_nnetar_1 %>%
    tune::autoplot() +
    geom_smooth(se = FALSE)

ggplotly(g1)

g2 <- tune_results_nnetar_2 %>%
    tune::autoplot() +
    geom_smooth(se = FALSE)

ggplotly(g2)

# * Retrain & Assess -----

set.seed(123)
wflw_fit_nnetar_tscv <- wflw_tune_nnetar %>%
    finalize_workflow(
        tune_results_nnetar_2 %>%
            show_best(metric = "rmse", n = 1)
    ) %>%
    fit(training(splits))

calibrate_and_plot(
    wflw_fit_nnetar_tscv
)


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
wflw_fit_prophet_boost <- calibration_tbl %>%
    pluck_modeltime_model(26)


# * Cross Validation Plans (K-Fold) ----
# - Prophet is a non-sequential model. We can randomize time observations.
# - K-Fold is OK
# - Should set.seed() because random process

set.seed(123)
resamples_kfold <- vfold_cv(
    data = training(splits)
    , v = 10
)

resamples_kfold %>%
    tk_time_series_cv_plan() %>%
    plot_time_series_cv_plan(optin_time, optins_trans, .facet_ncol = 2)


# * Recipe Spec ----
wflw_fit_prophet_boost %>%
    extract_preprocessor() %>%
    prep() %>%
    juice() %>%
    glimpse()


# * Model Spec ----
wflw_fit_prophet_boost %>%
    extract_spec_parsnip()

model_spec_prophet_boost <- prophet_boost(
    changepoint_num      = 20
    , changepoint_range  = 0.8
    , seasonality_yearly = FALSE
    , seasonality_weekly = FALSE
    , seasonality_daily  = FALSE
    
    , mtry           = tune()
    , trees          = 300
    , min_n          = tune()
    , tree_depth     = tune()
    , learn_rate     = tune()
    , loss_reduction = tune()
) %>%
    set_engine("prophet_xgboost")

model_spec_prophet_boost

# * Grid Spec ----
# Round 1

grid_spec_prophet_boost_1 <- grid_latin_hypercube(
    parameters(model_spec_prophet_boost) %>%
        update(
            mtry = mtry(range = c(1, 65))
        )
    , size = 15
)


# * Tune ----

# ** Setup Parallel Processing ----
registerDoFuture()

n_cores <- parallel::detectCores() - 1

plan(
    strategy = cluster,
    workers  = parallel::makeCluster(n_cores)
)


# ** K-Fold Cross Validation ----

grid <- grid_spec_prophet_boost_1

set.seed(123)
tic()
tune_results_prophet_kfold <- wflw_fit_prophet_boost %>%
    update_model(model_spec_prophet_boost) %>%
    tune_grid(
        resamples = resamples_kfold
        , grid    = grid
        , metrics = default_forecast_accuracy_metric_set()
        , control = control_grid(
            verbose = FALSE
            , save_pred = TRUE
        )
    )    
toc()

# ** Reset Sequential Plan ----

plan(strategy = sequential)

# Show Best
tune_results_prophet_kfold %>% show_best(metric = "rmse")


# Visualize
tune_results_prophet_kfold %>%
    autoplot() +
    geom_smooth(se = FALSE)


# * Retrain & Assess ----






# 4.0 SAVE ARTIFACTS ----




