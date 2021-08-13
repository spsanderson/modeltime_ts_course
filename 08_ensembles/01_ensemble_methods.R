# BUSINESS SCIENCE UNIVERSITY
# DS4B 203-R: TIME SERIES FORECASTING FOR BUSINESS
# MODULE: ENSEMBLE METHODS 

# SPECIAL REQUIREMENTS:
# - Ensembling: Requires modeltime.ensemble
# - Installation: devtools::install_github("business-science/modeltime.ensemble")

# GOAL: Combine Models (Ensemble) to Use Strength of Individual ♀Models by 
#  using their output as input into a final estimator

# OBJECTIVES ----
# - Simple Model Averaging (Got 2nd Place in 2014 Kaggle Walmart Forecast Challenge)
# - Weighted Averaging (Improves vs Averaging)
# - Stacked Ensembles
# - Multi-Level Ensembles


# LIBRARIES & SETUP ----

# Time Series ML
library(tidymodels)
library(modeltime)
library(modeltime.ensemble)
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

# * Parallel Processing ----

registerDoFuture()
n_cores <- parallel::detectCores()
plan(
    strategy = cluster,
    workers  = parallel::makeCluster(n_cores)
)


# DATA ----

artifacts_list    <- read_rds("00_models/feature_engineering_artifacts_list.rds") 
data_prepared_tbl <- artifacts_list$data$data_prepared_tbl 
data_prepared_tbl


# MODELS ----

calibration_tune_tbl    <- read_rds("00_models/calibration_tbl_hyperparameter_tuning.rds")
calibration_ml_tbl      <- read_rds("00_models/machine_learning_calibration_tbl.rds")
calibration_boosted_tbl <- read_rds("00_models/calibration_tbl_boosted_models.rds")
calibration_ets_tbl     <- read_rds("00_models/calibration_tbl_ets_tbats.rds")

# TRAIN / TEST SPLITS ----

splits <- time_series_split(data_prepared_tbl, assess = "8 weeks", cumulative = TRUE)

splits %>%
    tk_time_series_cv_plan() %>%
    plot_time_series_cv_plan(optin_time, optins_trans)


# 1.0 REVIEW  ----

# * Combine Tables & Organize ----

model_tbl <- combine_modeltime_tables(
    calibration_tune_tbl %>% 
        mutate(
            .model_desc = .model_desc %>% 
                   str_c(" - Tuned")
            ),
    calibration_ml_tbl,
    calibration_boosted_tbl,
    calibration_ets_tbl
)

calibration_tbl <- model_tbl %>%
    modeltime_calibrate(testing(splits))

# * Sub-Model Selection ----

calibration_tbl %>%
    modeltime_accuracy() %>%
    table_modeltime_accuracy(
        defaultPageSize = 40,
        bordered = TRUE,
        resizable = TRUE
    )

model_id_selection <- calibration_tbl %>%
    modeltime_accuracy() %>%
    arrange(rmse) %>%
    slice(1:10) %>%
    slice(-4, -6, -8) %>%
    pull(.model_id)

sub_models_tbl <- calibration_tbl %>%
    filter(.model_id %in% model_id_selection)

# 2.0 AVERAGE ENSEMBLES ----
# - Concept: Use sub-model predictions as inputs, take a simple average (or median)
# - Strengths: Fast to Create (No extra training)
# - Weaknesses: Susceptible to bad models (median can be helpful in these situations)

# * Making an Average Ensemble ----

?ensemble_average

ensemble_fit_mean <- sub_models_tbl %>%
    ensemble_average(type = "mean")

modeltime_table(ensemble_fit_mean) %>%
    modeltime_accuracy(testing(splits))

# * Making a Median Model ----

ensemble_fit_median <- sub_models_tbl %>%
    ensemble_average(type = "median")

modeltime_table(ensemble_fit_median, ensemble_fit_mean) %>%
    modeltime_accuracy(testing(splits))

# 3.0 WEIGHTED ENSEMBLES ----
# - Concept: Very similar to a simple average
# - Strength: 
#   - Can improve over simple average
#   - No training involved / Just as fast as Simple Average
# - Weakness: How to decide the weights?
# - Technique: Use a simple ranking (fastest)

loadings_tbl <- sub_models_tbl %>%
    modeltime_accuracy() %>%
    mutate(rank = min_rank(-rmse)) %>%
    select(.model_id, rank)

ensemble_fit_wt <- sub_models_tbl %>%
    ensemble_weighted(loadings = loadings_tbl$rank)

ensemble_fit_wt$fit$loadings_tbl

modeltime_table(
    ensemble_fit_wt
) %>%
    modeltime_accuracy(testing(splits))

# 4.0 STACKING ----
# - Concept: We use sub-model predictions as inputs; use a meta-learner to predict
# - Penalized Regression Technique: Assigns loadings (coefficients) to a
#     Elastic Net Model (glmnet). Those loadings are weights that can be used
#     as a simple weighted prediction (this is what glmnet does).
# - Strengths: This de-weights bad models, improving predictive accuracy
# - Weaknesses: More expensive operation when using Stacking with Cross Validation. 



# * 4.1 RESAMPLES -----
# - Step 1: modeltime_fit_resamples()

# *** TSCV ----

resamples_tscv <- training(splits) %>%
    drop_na() %>%
    time_series_cv(
        assess       = "8 weeks"
        , skip       = "4 weeks"
        , initial    = "12 months"
        , cumulative = TRUE
    )

resamples_tscv %>%
    tk_time_series_cv_plan() %>%
    plot_time_series_cv_plan(optin_time, optins_trans)

submodels_resample_tscv_tbl <- sub_models_tbl %>%
    modeltime_fit_resamples(
        resamples = resamples_tscv,
        control   = control_resamples(
            verbose   = TRUE,
            allow_par = FALSE
        )
    )

submodels_resample_tscv_tbl$.resample_results[[1]]$.predictions

# *** K-FOLD ----

set.seed(123)
resamples_kfold <- training(splits) %>%
    drop_na() %>%
    vfold_cv()

submodels_resamples_kfold_tbl <- sub_models_tbl %>%
    dplyr::slice(-1) %>%
    modeltime_fit_resamples(
        resamples = resamples_kfold,
        control_resamples(
            allow_par = TRUE
            , verbose = TRUE
            , pkgs = c("Cubist","rules")
        )
    )

# * 4.2 LM STACK ----

# TSCV 

ensemble_fit_lm_tscv <- submodels_resample_tscv_tbl %>%
    ensemble_model_spec(
        model_spec = linear_reg() %>%
            set_engine("lm")
        , control = control_grid(verbose = TRUE)
    )

modeltime_table(ensemble_fit_lm_tscv) %>%
    modeltime_accuracy(testing(splits))

# K-FOLD

set.seed(123)
ensemble_fit_lm_kfold <- submodels_resamples_kfold_tbl %>%
    ensemble_model_spec(
        model_spec = linear_reg() %>% set_engine("lm"),
        control = control_grid(verbose = TRUE)
    )

modeltime_table(
    ensemble_fit_lm_kfold
) %>%
    modeltime_accuracy(testing(splits))

# * 4.3 GLMNET STACK ----



# * 4.4 RANDOM FOREST STACK ----



# * 4.5 NNET STACK ----



# * 4.6 XGBOOST STACK ----



# * 4.7 CUBIST STACK ----



# * 4.8 SVM STACK ----



# 5.0 MULTI-LEVEL STACK ----




# 6.0 MODELTIME ----

# * Calibration  ----


# * Accuracy ----



# * Test Forecast ----



# * Refit Forecast ----



# * Turn off Parallelization ----
plan(sequential)

# 7.0 SAVE ----

