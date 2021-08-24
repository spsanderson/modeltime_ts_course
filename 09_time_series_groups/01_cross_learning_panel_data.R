# BUSINESS SCIENCE UNIVERSITY ----
# DS4B 203-R: TIME SERIES FORECASTING  ----
# MODULE: SCALABLE TIME SERIES - CROSS-SECTIONAL LEARNING ----

# GOAL: Forecast Grouped Daily Google Analytics Page Views - Next 28-days

# OBJECTIVES ----
# - Cross-Sectional Learning - Forecast Grouped Data using Cross-Sections
# - Panel Data - Become comfortable with Overlapping Time Stamps
# - Time Series Resampling - Evaluating Model Stability Over Time
# - Ensembling - Multiple Cross-Sectional Models

# IMPORTANT ----
# - These techniques must only be used with non-sequential models (e.g. machine learning)

# LIBRARIES & SETUP ----

# Time Series ML
library(tidymodels)
library(modeltime)
library(modeltime.ensemble)

# Timing & Parallel Processing
library(tictoc)
library(future)
library(doFuture)

# Core 
library(tidyquant)
library(tidyverse)
library(timetk)

# * Parallel Processing ----

registerDoFuture()
n_cores <- parallel::detectCores()
plan(
    strategy = cluster,
    workers  = parallel::makeCluster(n_cores)
)

# plan(sequential)

# 1.0 DATA ----

# * GA Data ----
ga_page_raw_tbl <- read_rds("00_data/google_analytics_by_page_daily.rds")

ga_page_raw_tbl %>%
    group_by(pagePath) %>%
    plot_time_series(
        date
        , pageViews
        , .facet_ncol = 4
        , .smooth = FALSE
        , .interactive = FALSE
    )

# * Full Data ----

full_data_tbl <- ga_page_raw_tbl %>%
    
    # Fix data issues
    select(date, pagePath, pageViews) %>%
    group_by(pagePath) %>%
    pad_by_time(date, .by = "day", .pad_value = 0) %>%
    ungroup() %>%
    
    # Global features / Transformations / Joins
    mutate(pageViews = log1p(pageViews)) %>%
    
    # Group-wise Feature Transformations
    group_by(pagePath) %>%
    future_frame(
        .date_var = date
        , .length_out = 28
        , .bind_data = TRUE
    ) %>%
    ungroup() %>%
    
    # Lags / Rolling Features / Fourier
    mutate(pagePath = as_factor(pagePath)) %>%
    group_by(pagePath) %>%
    group_split() %>%
    map(.f = function(df) {
        df %>%
            arrange(date) %>%
            tk_augment_fourier(date, .periods = c(14, 28)) %>%
            tk_augment_lags(pageViews, .lags = 28) %>%
            tk_augment_slidify(
                pageViews_lag28
                , .f = ~mean(.x, na.rm = TRUE)
                , .period = c(7, 28, 28*2)
                , .partial = TRUE
                , .align = "center"
            )
    }) %>%
    bind_rows() %>%
    rowid_to_column(var = "rowid")


# * Data Prepared ----
data_prepared_tbl <- full_data_tbl %>%
    filter(!is.na(pageViews)) %>%
    drop_na()

# * Future Data ----
future_tbl <- full_data_tbl %>%
    filter(is.na(pageViews))
    
future_tbl <- future_tbl %>%
    mutate(
        across(
            .cols = contains("_lag")
            , .fns = ~ ifelse(is.nan(.x), NA, .x)
            )
        ) %>%
    fill(contains("_lag"), .direction = "up")

future_tbl %>% filter(is.na(pageViews_lag28_roll_56))

# 2.0 TIME SPLIT ----

splits <- data_prepared_tbl %>%
    time_series_split(
        date_var = date
        , assess = 28
        , cumulative = TRUE
    )

splits %>%
    tk_time_series_cv_plan() %>%
    plot_time_series_cv_plan(
        date, pageViews
    )

# 3.0 RECIPE ----

# * Clean Training Set ----
# - With Panel Data, need to do this outside of a recipe
# - Transformation happens by group

train_cleaned <- training(splits) %>%
    group_by(pagePath) %>%
    mutate(pageViews = ts_clean_vec(pageViews, period = 7))

train_cleaned %>%
    group_by(pagePath) %>%
    plot_time_series(
        date,
        pageViews,
        .facet_ncol = 4,
        .smooth = FALSE,
        .interactive = FALSE
    )

# * Recipe Specification ----
train_cleaned

recipe_spec <- recipe(pageViews ~ ., data = train_cleaned) %>%
    update_role(rowid, new_role = "indicator") %>%
    step_timeseries_signature(date) %>%
    step_rm(matches("(.xts$)|(.iso$)|(hour)|(minute)|(second)|(am.pm)")) %>%
    step_normalize(date_index.num, date_year) %>%
    step_other(pagePath) %>%
    step_dummy(all_nominal(), one_hot = TRUE)

recipe_spec %>%
    prep() %>%
    juice() %>%
    glimpse()

# 4.0 MODELS ----
# - !!! REMINDER: Cannot use sequential models !!!

# * PROPHET ----
wflw_fit_prophet <- workflow() %>%
    add_model(
        spec = prophet_reg() %>%
            set_engine("prophet")
    ) %>%
    add_recipe(recipe_spec) %>%
    fit(train_cleaned)


# * XGBOOST ----
wflw_fit_xgboost <- workflow() %>%
    add_model(
        spec = boost_tree(mode = "regression") %>%
            set_engine("xgboost")
    ) %>%
    add_recipe(
        recipe_spec %>%
            update_role(date, new_role = "indicator")
    ) %>%
    fit(train_cleaned)


# * PROPHET BOOST ----
wflw_fit_prophet_boost <- workflow() %>%
    add_model(
        spec = prophet_boost(
            seasonality_daily = FALSE,
            seasonality_weekly = FALSE,
            seasonality_yearly = FALSE
        ) %>%
            set_engine("prophet_xgboost")
    ) %>%
    add_recipe(recipe_spec) %>%
    fit(train_cleaned)


# * SVM ----

wflw_fit_svm <- workflow() %>%
    add_model(
        spec = svm_rbf(
            mode = "regression"
        ) %>%
            set_engine("kernlab")
    ) %>%
    add_recipe(
        recipe_spec %>%
            update_role(date, new_role = "indicator")
    ) %>%
    fit(train_cleaned)


# * RANDOM FOREST ----
wflw_fit_rf <- workflow() %>%
    add_model(
        spec = rand_forest(
            mode = "regression"
        ) %>%
            set_engine("ranger")
    ) %>%
    add_recipe(recipe_spec %>% update_role(date, new_role = "indicator")) %>%
    fit(train_cleaned)


# * NNET ----
wflw_fit_nnet <- workflow() %>%
    add_model(
        spec = mlp(mode = "regression") %>%
            set_engine("nnet")
    ) %>%
    add_recipe(recipe_spec %>% update_role(date, new_role = "indicator")) %>%
    fit(train_cleaned)


# * MARS ----
wflw_fit_mars <- workflow() %>%
    add_model(
        spec = mars(
            mode = "regression"
        ) %>%
        set_engine("earth")
    ) %>%
    add_recipe(recipe_spec %>% update_role(date, new_role = "indicator")) %>%
    fit(train_cleaned)


# * ACCURACY CHECK ----
submodels_1_tbl <- modeltime_table(
    wflw_fit_prophet,
    wflw_fit_xgboost,
    wflw_fit_prophet_boost,
    wflw_fit_svm,
    wflw_fit_rf,
    wflw_fit_nnet,
    wflw_fit_mars
)

submodels_1_tbl %>%
    modeltime_accuracy(testing(splits))  %>%
    arrange(rmse)

# 5.0 HYPER PARAMETER TUNING ---- 

# * RESAMPLES - K-FOLD ----- 

set.seed(123)
resamples_kfold <- train_cleaned %>%
    vfold_cv(v = 5)

resamples_kfold %>%
    tk_time_series_cv_plan() %>%
    plot_time_series_cv_plan(date, pageViews, .facet_ncol = 2)

# * XGBOOST TUNE ----

# ** Tunable Specification
model_spec_xgboost_tune <- boost_tree(
    mode = "regression"
    , mtry           = tune()
    , trees          = tune()
    , min_n          = tune()
    , tree_depth     = tune()
    , learn_rate     = tune()
    , loss_reduction = tune()
) %>%
    set_engine("xgboost")

wflw_spec_xgboost_tune <- workflow() %>%
    add_model(model_spec_xgboost_tune) %>%
    add_recipe(recipe_spec %>% update_role(date, new_role = "indicator"))

# ** Tuning
set.seed(123)
parallel_start(4)
wflw_spec_xgboost_tune %>%
    tune_grid(
        resamples = resamples_kfold
        , grid    = 10
        , param_info = parameters(wflw_spec_xgboost_tune) %>%
            update(learn_rate = learn_rate(range = c(0.001, 0.400),
                                           trans = NULL))
        , control = control_grid(
            verbose = TRUE
        )
    )
parallel_stop()

# ** Results


# ** Finalize


# * RANGER TUNE ----

# ** Tunable Specification


# ** Tuning


# ** Results


# ** Finalize


# * EARTH TUNE ----

# ** Tunable Specification


# ** Tuning


# ** Results


# ** Finalize



# 6.0 EVALUATE PANEL FORECEASTS  -----

# * Model Table ----


# * Calibration ----


# * Accuracy ----


# * Forecast Test ----


# 7.0 RESAMPLING ----
# - Assess the stability of our models over time
# - Helps us strategize an ensemble approach

# * Time Series CV ----


# * Fitting Resamples ----


# * Resampling Accuracy Table ----


# * Resampling Accuracy Plot ----


# 8.0 ENSEMBLE PANEL MODELS -----

# * Average Ensemble ----


# * Accuracy ----


# * Forecast ----


# * Refit ----


# * Turn OFF Parallel Backend
plan(sequential)

# 9.0 RECAP ----
# - You:
#     1. Prepared 20 Time Series Groups
#     2. Modeled Panel Data
#     3. Hyper Parameter Tuned 
#     4. Resampled & Evaluated Accuracy Over Time
#     5. Ensembled the models using a strategy based on resample stability
#     6. RMSE 143, MAE 46, RSQ 0.40
# - This code can work for 10,000 Time Series. 
#     1. Only expense is Hyper Parameter Tuning
#     2. Watch Saving Ensembles & Models - Memory Size 


