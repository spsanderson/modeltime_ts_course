# BUSINESS SCIENCE UNIVERSITY ----
# DS4B 203-R: TIME SERIES FORECASTING  ----
# MODULE: DEEP LEARNING - MULTIPLE TIME SERIES ----

# GOAL: Forecast Grouped Daily Google Analytics Page Views - Next 28-days

# OBJECTIVES ----
# - Deep Learning - Using Modeltime GluonTS w/ Multiple Time Series
# - Compare       - Deep Learning vs Machine Learning
# - Ensembling    - Multiple Models
# - Pros/Cons     - External Regressors

# LIBRARIES & SETUP ----

# Time Series ML
library(tidymodels)
library(modeltime)
library(modeltime.gluonts)
library(modeltime.ensemble)

# Parallel Processing
library(doFuture)

# Core 
library(tidyverse)
library(timetk)
library(skimr)
library(fs)

reticulate::py_discover_config()

# 1.0 DATA ----

# * GA Data ----

ga_page_raw_tbl <- read_rds("00_data/google_analytics_by_page_daily.rds")


ga_page_raw_tbl %>%
    group_by(pagePath) %>%
    plot_time_series(
        date, pageViews,
        .facet_ncol = 4
    )

# * Full Data ----

FORECAST_HORIZON <- 28

full_data_tbl <- ga_page_raw_tbl %>%
    
    # Fix data issues
    select(date:pageViews) %>%
    group_by(pagePath) %>%
    pad_by_time(date, .by = "day", .pad_value = 0) %>%
    ungroup() %>%
    
    # Log Transformation Target
    mutate(pageViews = log1p(pageViews)) %>%
    
    # Groupwise Data Manipulation
    group_by(pagePath) %>%
    
    # Extending
    future_frame(
        .length_out = FORECAST_HORIZON,
        .bind_data  = TRUE
    ) %>%
    
    # Fourier
    tk_augment_fourier(
        .date_var = date,
        .periods  = c(0.5*FORECAST_HORIZON, FORECAST_HORIZON),
        .K        = 1
    ) %>%
    
    # Lag
    tk_augment_lags(
        .value  = pageViews, 
        .lags   = FORECAST_HORIZON
    ) %>%
    
    # Rolling Features
    tk_augment_slidify(
        .value   = pageViews_lag28,
        .f       = ~ mean(.x, na.rm = TRUE),
        .period  = c(7, FORECAST_HORIZON, 2*FORECAST_HORIZON),
        .partial = TRUE,
        .align   = 'center' 
    ) %>% 
    ungroup() %>%
    
    rowid_to_column(var = "rowid")

full_data_tbl %>% glimpse()

# * Data Prepared ----

data_prepared_tbl <- full_data_tbl %>%
    filter(!is.na(pageViews)) %>%
    drop_na()

data_prepared_tbl %>% skim()

# * Future Data ----

future_tbl <- full_data_tbl %>%
    filter(is.na(pageViews)) %>%
    drop_na(pageViews_lag28)

future_tbl %>% skim()

future_tbl %>%
    filter(is.na(pageViews_lag28)) %>%
    select(rowid, pagePath) 


# 2.0 TIME SPLIT ----

splits <- data_prepared_tbl %>%
    time_series_split(
        date_var   = date, 
        assess     = FORECAST_HORIZON, 
        cumulative = TRUE
    )

splits %>%
    tk_time_series_cv_plan() %>%
    plot_time_series_cv_plan(date, pageViews)

training(splits) %>%
    group_by(pagePath) %>%
    plot_time_series(
        date, pageViews, .facet_ncol = 4
    )

# 3.0 GLUONTS MODELS ----

# * GLUON Recipe Specification ----

recipe_spec_gluon <- recipe(
    pageViews ~ pagePath + date + rowid, 
    data = training(splits)
) %>%
    update_role(rowid, new_role = "indicator")

recipe_spec_gluon %>% prep() %>% juice()

recipe_spec_gluon %>% prep() %>% summary()

# * DeepAR Estimator ----

# Model 1: Default GluonTS

?deep_ar

model_spec_1 <- deep_ar(
    # Required Params
    id                = "pagePath",
    freq              = "D",
    prediction_length = FORECAST_HORIZON,
    
    # Trainer (Common Models)
    epochs            = 5,
    
    # DeepAR Specific
    cell_type         = "lstm"
) %>%
    set_engine("gluonts_deepar")

wflw_fit_deepar_1 <- workflow() %>%
    add_model(model_spec_1) %>%
    add_recipe(recipe_spec_gluon) %>%
    fit(training(splits))

wflw_fit_deepar_1

# Model 2: Increase Epochs, Adjust Num Batches per Epoch

model_spec_2_deepar <- deep_ar(
    id                    = "pagePath",
    freq                  = "D",
    prediction_length     = FORECAST_HORIZON,
    
    epoch                 = 10,
    num_batches_per_epoch = 35
) %>%
    set_engine("gluonts_deepar")

wflw_fit_deepar_2 <- workflow() %>%
    add_model(model_spec_2_deepar) %>%
    add_recipe(recipe_spec_gluon) %>%
    fit(training(splits))

# Model 3: Increase Epochs, Adjust Num Batches Per Epoch, & Add Scaling 

model_spec_deepar_3 <- deep_ar(
    id                    = "pagePath",
    freq                  = "D",
    prediction_length     = FORECAST_HORIZON,
    
    epoch                 = 10,
    num_batches_per_epoch = 35,
    
    scale                 = TRUE
) %>%
    set_engine("gluonts_deepar")

wflw_fit_deepar_3 <- workflow() %>%
    add_model(model_spec_deepar_3) %>%
    add_recipe(recipe_spec_gluon) %>%
    fit(training(splits))



# * N-BEATS Estimator ----

?nbeats()

training(splits)

# Model 4: N-BEATS Default

model_spec_nbeats_4 <- nbeats(
    id                = "pagePath",
    freq              = "D",
    prediction_length = FORECAST_HORIZON,
    
    lookback_length   = 2 * FORECAST_HORIZON 
) %>%
    set_engine("gluonts_nbeats")

wflw_fit_nbeats_4 <- workflow() %>%
    add_model(model_spec_nbeats_4) %>%
    add_recipe(recipe_spec_gluon) %>%
    fit(training(splits))

# Model 5: Loss Function MASE, Reduce Epochs 2

model_spec_nbeats_5 <- nbeats(
    id                = "pagePath",
    freq              = "D",
    prediction_length = FORECAST_HORIZON,
    
    lookback_length   = 2 * FORECAST_HORIZON,
    epochs            = 2,
    loss_function     = "MASE"
) %>%
    set_engine("gluonts_nbeats")

wflw_fit_nbeats_5 <- workflow() %>%
    add_model(model_spec_nbeats_5) %>%
    add_recipe(recipe_spec_gluon) %>%
    fit(training(splits))

# Model 6: Model 5 Start, Ensemble

model_spec_nbeats_6 <- nbeats(
    id                    = "pagePath",
    freq                  = "D",
    prediction_length     = FORECAST_HORIZON,
    
    lookback_length       = c(FORECAST_HORIZON, 2 * FORECAST_HORIZON),
    epochs                = 2,
    num_batches_per_epoch = 35,
    loss_function         = "MASE",
    
    bagging_size          = 1
) %>%
    set_engine("gluonts_nbeats_ensemble")

wflw_fit_nbeats_6 <- workflow() %>%
    add_model(model_spec_nbeats_6) %>%
    add_recipe(recipe_spec_gluon) %>%
    fit(training(splits))

# ** Modeltime Comparison ----

model_tbl_submodels <- modeltime_table(
    wflw_fit_deepar_1,
    wflw_fit_deepar_2,
    wflw_fit_deepar_3,
    
    wflw_fit_nbeats_4,
    wflw_fit_nbeats_5,
    wflw_fit_nbeats_6
)

# Forecast Accuracy

model_tbl_submodels %>%
    modeltime_accuracy(testing(splits))


# Forecast Visualization
forecast_submodels_test_tbl <- model_tbl_submodels %>%
    modeltime_forecast(
        new_data    = testing(splits),
        actual_data = data_prepared_tbl,
        keep_data   = TRUE 
    ) 

forecast_submodels_test_tbl %>%
    group_by(pagePath) %>%
    plot_modeltime_forecast(
        .facet_ncol = 4
    )

# Investigate Model Failures

forecast_submodels_test_tbl %>% skim()

forecast_submodels_test_tbl %>%
    filter(is.na(.value)) %>%
    select(.model_id, .model_desc, .index, pagePath, .value)

training(splits) %>% distinct(pagePath)

testing(splits) %>% distinct(pagePath)

# Forecast Future

submodel_inspection_tbl <- modeltime_table(
    wflw_fit_deepar_2,
    wflw_fit_deepar_3
)

deepar_submodel_refitted_tbl <- submodel_inspection_tbl %>%
    modeltime_refit(data_prepared_tbl)


deepar_submodel_refitted_tbl %>%
    modeltime_forecast(
        new_data    = future_tbl,
        actual_data = data_prepared_tbl,
        keep_data   = TRUE 
    ) %>%
    group_by(pagePath) %>%
    plot_modeltime_forecast(
        .facet_ncol = 4
    )
    



# 4.0 MACHINE LEARNING ----

# * Parallel Processing ----

registerDoFuture()
n_cores <- parallel::detectCores()
plan(
    strategy = cluster, 
    workers  = parallel::makeCluster(n_cores)
)

# * ML Recipe Specification ----

training(splits)

recipe_spec_ml <- recipe(pageViews ~ ., data = training(splits)) %>%
    update_role(rowid, date, new_role = "indicator") %>%
    step_timeseries_signature(date) %>%
    step_rm(matches("(.xts$)|(.iso$)|(hour)|(minute)|(second)|(am.pm)")) %>%
    step_normalize(contains("index.num"), ends_with("_year")) %>%
    step_other(pagePath) %>%
    step_dummy(all_nominal(), one_hot = TRUE)

recipe_spec_ml %>% prep() %>% juice() %>% glimpse()

recipe_spec_ml %>% prep() %>% summary()

# * XGBoost ----

model_spec_xgboost_tune <- boost_tree(
    mode            = "regression",
    mtry            = tune(),
    trees           = 300,
    min_n           = tune(),
    tree_depth      = tune(),
    learn_rate      = tune(),
    loss_reduction  = tune()
) %>%
    set_engine("xgboost")


wflw_spec_tune_xgboost <- workflow() %>%
    add_model(model_spec_xgboost_tune) %>%
    add_recipe(recipe_spec_ml)

set.seed(123)
resamples_kfold <- training(splits) %>% vfold_cv(v = 5)

?tune_grid()

tune_results_xgboost <- tune_grid(
    object     = wflw_spec_tune_xgboost,
    resamples  = resamples_kfold,
    param_info = parameters(wflw_spec_tune_xgboost) %>%
        update(learn_rate = learn_rate(range = c(0.15, 0.5), trans = NULL)),
    grid       = 10, 
    control    = control_grid(verbose = TRUE, allow_par = TRUE)
)

best_results <- tune_results_xgboost %>%
    show_best(metric = "rmse", n = 10)

wflw_fit_xgboost <- wflw_spec_tune_xgboost %>%
    finalize_workflow(parameters = best_results %>% slice(1)) %>%
    fit(training(splits))

# * End Parallel Processing ----

plan(sequential)


# 5.0 EVALUATION ----

# * Test Evaluations ----

testing(splits) %>% distinct(pagePath)

page_paths_train <- training(splits) %>% distinct(pagePath) %>% pull(pagePath)

submodels_tbl <- submodel_inspection_tbl %>%
    update_model_description(1, "DEEPAR - Unscaled") %>%
    update_model_description(2, "DEEPAR - Scaled") %>%
    add_modeltime_model(wflw_fit_xgboost)

submodels_tbl %>%
    modeltime_accuracy(
        testing(splits) %>% filter(pagePath %in% page_paths_train)
    )

submodels_tbl %>%
    modeltime_forecast(
        new_data    = testing(splits),
        actual_data = data_prepared_tbl,
        keep_data   = TRUE 
    ) %>%
    group_by(pagePath) %>%
    plot_modeltime_forecast(
        .facet_ncol = 4
    )

# * Future Evaluations ----

submodels_refitted_tbl <- submodels_tbl %>%
    modeltime_refit(data_prepared_tbl)

submodels_refitted_tbl %>%
    modeltime_forecast(
        new_data    = future_tbl,
        actual_data = data_prepared_tbl,
        keep_data   = TRUE 
    ) %>%
    group_by(pagePath) %>%
    plot_modeltime_forecast(
        .facet_ncol = 4
    )

# 6.0 ENSEMBLE ----

# * Make Ensemble ----

model_tbl_ensemble <- submodels_tbl %>%
    ensemble_weighted(loadings = c(3, 7, 3)) %>%
    modeltime_table() 


# * Evaluate Ensemble Test ----

model_tbl_ensemble %>%
    combine_modeltime_tables(submodels_tbl) %>%
    modeltime_accuracy(testing(splits)) %>%
    arrange(rmse)


# * Refit Ensemble & Evaluate Future ----

model_tbl_ensemble_refit <- model_tbl_ensemble %>%
    modeltime_refit(data_prepared_tbl)

model_tbl_ensemble_refit %>%
    modeltime_forecast(
        new_data    = future_tbl,
        actual_data = data_prepared_tbl,
        keep_data   = TRUE
    ) %>%
    
    mutate(.value = expm1(.value)) %>%
    
    group_by(pagePath) %>%
    plot_modeltime_forecast(
        .facet_ncol = 4
    )


# 7.0 SAVING & LOADING ----


# * Save Submodels ----

fs::dir_create("00_models/final_google_analytics_daily")

model_tbl_ensemble_refit$.model[[1]]$model_tbl$.model[[1]] %>%
    save_gluonts_model(path = "00_models/final_google_analytics_daily/deepar_unscaled")

model_tbl_ensemble_refit$.model[[1]]$model_tbl$.model[[2]] %>%
    save_gluonts_model(path = "00_models/final_google_analytics_daily/deepar_scaled")

model_tbl_ensemble_refit$.model[[1]]$model_tbl$.model[[3]] %>%
    write_rds("00_models/final_google_analytics_daily/xgboost.rds")

# * Load Submodels ----

model_deepar_unscaled <- load_gluonts_model("00_models/final_google_analytics_daily/deepar_unscaled/")

model_deepar_scaled  <- load_gluonts_model("00_models/final_google_analytics_daily/deepar_scaled/")

model_xgboost <- read_rds("00_models/final_google_analytics_daily/xgboost.rds")

# * Make ensemble ----

ensemble_reloaded_tbl <- modeltime_table(
    model_deepar_unscaled,
    model_deepar_scaled,
    model_xgboost
) %>%
    ensemble_weighted(loadings = c(3, 7, 3)) %>%
    modeltime_table()

ensemble_reloaded_tbl %>%
    modeltime_forecast(
        new_data = future_tbl,
        actual_data = data_prepared_tbl,
        keep_data = TRUE
    ) %>%
    mutate(.value = expm1(.value)) %>%
    group_by(pagePath) %>%
    plot_modeltime_forecast(
        .facet_ncol = 4
    )


# CONCLUSIONS ----

# DEEP LEARNING
# - [PROS] Create very powerful models by combining Machine Learning & Deep Learning
# - [PROS] Deep Learning is great for global modeling time series (1 model for > 1 Time Series Groups)
# COMBINING ML & DL
# - [CONS] Deep Learning does not factor in external regressors. 
#   - Solution 1: Run DL without. Run ML on the Residuals. 
#   - Solution 2: Create an Ensemble with ML & DL


