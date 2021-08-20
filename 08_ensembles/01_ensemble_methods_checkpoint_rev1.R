# BUSINESS SCIENCE UNIVERSITY
# DS4B 203-R: TIME SERIES FORECASTING FOR BUSINESS
# MODULE: ENSEMBLE METHODS 

# SPECIAL REQUIREMENTS:
# - Ensembling: Requires modeltime.ensemble
# - Installation: devtools::install_github("business-science/modeltime.ensemble")

# GOAL: Combine Models (Ensemble) to Use Strength of Individual Models by 
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

# plan(sequential)


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
    calibration_tune_tbl %>% mutate(.model_desc = .model_desc %>% str_c(" - Tuned")),
    calibration_ml_tbl,
    calibration_boosted_tbl,
    calibration_ets_tbl
)

calibration_tbl <- model_tbl %>%
    
    # FIX #1 ----
    # Add Refit to get models updated
    modeltime_refit(training(splits)) %>%
    # END FIX #1 ----
    
    modeltime_calibrate(testing(splits))
# * NOTES: ----
#   - Refitting updates the saved models 
#   - Cubist Models 18 & 19: Error: argument "default" is missing, with no default
#     - Solution: Update rules package: devtools::install_github("tidymodels/rules")

# * Sub-Model Selection ----

calibration_tbl %>%
    modeltime_accuracy() %>%
    table_modeltime_accuracy(
        defaultPageSize = 40,
        bordered        = TRUE,
        resizable       = TRUE
    )

model_id_selection <- calibration_tbl %>%
    modeltime_accuracy() %>%
    arrange(rmse) %>%
    slice(1:10) %>%
    slice(-1, -7) %>%
    pull(.model_id)
    
submodels_tbl <- calibration_tbl %>%
    filter(.model_id %in% model_id_selection)

submodels_tbl

# 2.0 AVERAGE ENSEMBLES ----
# - Concept: Use sub-model predictions as inputs, take a simple average (or median)
# - Strengths: Fast to Create (No extra training)
# - Weaknesses: Susceptible to bad models (median can be helpful in these situations)

?ensemble_average

# * Making an Average Ensemble ----

ensemble_fit_mean <- submodels_tbl %>%
    ensemble_average(type = "mean")

modeltime_table(
    ensemble_fit_mean
) %>%
    modeltime_accuracy(testing(splits))


# * Making a Median Model ----

ensemble_fit_median <- submodels_tbl %>%
    ensemble_average("median")

modeltime_table(
    ensemble_fit_mean,
    ensemble_fit_median
) %>%
    modeltime_accuracy(testing(splits))

# 3.0 WEIGHTED ENSEMBLES ----
# - Concept: Very similar to a simple average
# - Strength: 
#   - Can improve over simple average
#   - No training involved / Just as fast as Simple Average
# - Weakness: How to decide the weights?
# - Technique: Use a simple ranking (fastest)

?ensemble_weighted

loadings_tbl <- submodels_tbl %>%
    modeltime_accuracy() %>%
    mutate(rank = min_rank(-rmse)) %>%
    select(.model_id, rank)

ensemble_fit_wt <- submodels_tbl %>%
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

?ensemble_model_spec


# * 4.1 RESAMPLES -----
# - Step 1: modeltime_fit_resamples()

# *** TSCV ----

resamples_tscv <- training(splits) %>%
    drop_na() %>%
    time_series_cv(
        assess     = "8 weeks",
        skip       = "4 weeks",
        initial    = "12 months",
        cumulative = TRUE
    )

resamples_tscv %>%
    tk_time_series_cv_plan() %>%
    plot_time_series_cv_plan(optin_time, optins_trans)


submodels_resamples_tscv_tbl <- submodels_tbl %>%
    modeltime_fit_resamples(
        resamples = resamples_tscv,
        control   = control_resamples(
            verbose   = TRUE, 
            # allow_par = FALSE, 
            allow_par = TRUE,
            pkgs      = c("Cubist", "rules")
        )
    )

submodels_resamples_tscv_tbl$.resample_results[[1]]$.predictions

# *** K-FOLD ----

set.seed(123)
resamples_kfold <- training(splits) %>%
    drop_na() %>%
    vfold_cv(v = 10)

resamples_kfold %>%
    tk_time_series_cv_plan() %>%
    plot_time_series_cv_plan(optin_time, optins_trans, .facet_ncol = 2)

submodels_resamples_kfold_tbl <- submodels_tbl %>%
    dplyr::slice(-1) %>%
    modeltime_fit_resamples(
        resamples = resamples_kfold,
        control   = control_resamples(
            verbose    = TRUE, 
            allow_par  = TRUE,
            pkgs       = c("Cubist", "rules")
        )
    )

# * 4.2 LM STACK ----

# TSCV 
set.seed(123)
ensemble_fit_lm_tscv <- submodels_resamples_tscv_tbl %>%
    ensemble_model_spec(
        model_spec = linear_reg() %>% set_engine("lm"),
        control    = control_grid(verbose = TRUE)
    )

modeltime_table(
    ensemble_fit_lm_tscv
) %>%
    modeltime_accuracy(testing(splits))

# K-FOLD
set.seed(123)
ensemble_fit_lm_kfold <- submodels_resamples_kfold_tbl %>%
    ensemble_model_spec(
        model_spec = linear_reg() %>% set_engine("lm"),
        control    = control_grid(verbose = TRUE)
    )

modeltime_table(
    ensemble_fit_lm_kfold
) %>%
    modeltime_accuracy(testing(splits))

# * 4.3 GLMNET STACK ----
set.seed(123)
ensemble_fit_glmnet_kfold <- submodels_resamples_kfold_tbl %>%
    ensemble_model_spec(
        model_spec = linear_reg(
            penalty = tune(),
            mixture = tune()
        ) %>% 
            set_engine("glmnet"),
        kfolds  = 10,
        grid    = 10,
        control = control_grid(
            verbose   = TRUE, 
            allow_par = TRUE
        )
    )

modeltime_table(
    ensemble_fit_glmnet_kfold
) %>%
    modeltime_accuracy(testing(splits))



# * 4.4 RANDOM FOREST STACK ----

set.seed(123)
ensemble_fit_ranger_kfold <- submodels_resamples_kfold_tbl %>%
    ensemble_model_spec(
        model_spec = rand_forest(
            trees = tune(),
            min_n = tune()
        ) %>%
            set_engine("ranger"),
        kfolds  = 10, 
        grid    = 10,
        control = control_grid(verbose = TRUE, allow_par = TRUE)
    )

modeltime_table(
    ensemble_fit_ranger_kfold
) %>%
    modeltime_accuracy(testing(splits))


# * 4.5 NNET STACK ----

set.seed(123)
ensemble_fit_nnet_kfold <- submodels_resamples_kfold_tbl %>%
    ensemble_model_spec(
        model_spec = mlp(
            hidden_units = tune(),
            penalty      = tune(),
            epochs       = tune()
        ) %>% set_engine("nnet"),
        kfolds = 10, 
        grid   = 10, 
        control = control_grid(verbose = TRUE, allow_par = TRUE)
    )

modeltime_table(
    ensemble_fit_nnet_kfold
) %>%
    modeltime_accuracy(testing(splits))

# * 4.6 XGBOOST STACK ----
set.seed(123)
ensemble_fit_xgboost_kfold <- submodels_resamples_kfold_tbl %>%
    ensemble_model_spec(
        model_spec = boost_tree(
            trees          = tune(),
            tree_depth     = tune(),
            learn_rate     = tune(),
            loss_reduction = tune()
        ) %>%
            set_engine("xgboost"),
        kfolds = 10, 
        grid   = 10, 
        control = control_grid(verbose = TRUE, allow_par = TRUE)
    )


modeltime_table(
    ensemble_fit_xgboost_kfold
) %>%
    modeltime_accuracy(testing(splits))

# * 4.7 CUBIST STACK ----

set.seed(123)
ensemble_fit_cubist_kfold <- submodels_resamples_kfold_tbl %>%
    ensemble_model_spec(
        model_spec = cubist_rules(
            committees = tune(), 
            neighbors  = tune(),
            max_rules  = tune()
        ) %>%
            set_engine("Cubist"),
        kfold = 10,
        grid  = 10, 
        control = control_grid(
            verbose = TRUE, 
            allow_par = TRUE
        )
    )

modeltime_table(
    ensemble_fit_cubist_kfold
) %>%
    modeltime_accuracy(testing(splits))

# * 4.8 SVM STACK ----

set.seed(123)
ensemble_fit_svm_kfold <- submodels_resamples_kfold_tbl %>%
    ensemble_model_spec(
        model_spec = svm_rbf(
            mode      = "regression",
            cost      = tune(),
            rbf_sigma = tune(),  
            margin    = tune()
        ) %>%
            set_engine("kernlab"),
        kfold = 10, 
        grid  = 10, 
        control = control_grid(verbose = TRUE, allow_par = TRUE)
    )

modeltime_table(
    ensemble_fit_svm_kfold
) %>%
    modeltime_accuracy(testing(splits))


# 5.0 MULTI-LEVEL STACK ----

model_stack_level_2_accuracy_tbl <- modeltime_table(
    ensemble_fit_glmnet_kfold,
    ensemble_fit_ranger_kfold,
    ensemble_fit_nnet_kfold,
    ensemble_fit_xgboost_kfold, 
    ensemble_fit_cubist_kfold,
    ensemble_fit_svm_kfold
) %>%
    modeltime_accuracy(testing(splits))

model_stack_level_2_accuracy_tbl %>% table_modeltime_accuracy()


model_stack_level_3_tbl <- modeltime_table(
    ensemble_fit_ranger_kfold,
    ensemble_fit_svm_kfold,
    ensemble_fit_glmnet_kfold
) %>%
    ensemble_weighted(loadings = c(5, 3, 1)) %>%
    modeltime_table()

model_stack_level_3_tbl %>%
    modeltime_accuracy(testing(splits))

# 6.0 MODELTIME ----

# * Calibration  ----

calibration_ensemble_tbl <- model_stack_level_3_tbl %>% 
    modeltime_calibrate(testing(splits))

calibration_ensemble_tbl

# * Accuracy ----

calibration_ensemble_tbl %>% modeltime_accuracy()



# * Test Forecast ----

forecast_test_tbl <- calibration_ensemble_tbl %>%
    modeltime_forecast(
        new_data    = testing(splits),
        actual_data = data_prepared_tbl
    )

forecast_test_tbl %>%
    plot_modeltime_forecast()

# * Refit Forecast ----

# Not updating the super learner / updating submodels only 

refit_ensemble_submodel_tbl <- calibration_ensemble_tbl %>%
    modeltime_refit(data = data_prepared_tbl)

forecast_submodels_tbl <- refit_ensemble_submodel_tbl %>%
    modeltime_forecast(
        new_data    = artifacts_list$data$forecast_tbl,
        actual_data = data_prepared_tbl
    )

forecast_submodels_tbl %>%
    plot_modeltime_forecast()

# Updating the superlearner and the submodels
set.seed(123)
refit_ensemble_superlearner_tbl <- calibration_ensemble_tbl %>%
    modeltime_refit(
        data = data_prepared_tbl,
        resamples = data_prepared_tbl %>%
            drop_na() %>%
            vfold_cv(v = 10)
    )

forecast_superlearner_tbl <- refit_ensemble_superlearner_tbl %>%
    modeltime_forecast(
        new_data    = artifacts_list$data$forecast_tbl,
        actual_data = data_prepared_tbl
    )

forecast_superlearner_tbl %>%
    plot_modeltime_forecast()


# * Turn off Parallelization ----
plan(sequential)

# 7.0 SAVE ----

model_stack_level_3_tbl %>%
    write_rds("00_models/model_stack_level_3_tbl.rds")

model_stack_level_3_tbl$.model[[1]]$model_tbl$.model[[2]]$model_tbl
