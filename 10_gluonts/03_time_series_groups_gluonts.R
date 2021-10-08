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
        .date_var = date,
        .value = pageViews,
        .facet_ncol = 4
    )


# * Full Data ----
# set up fro ML and DL
FORECAST_HORIZON <- 28

full_data_tbl <- ga_page_raw_tbl %>%
    
    # Fix data issues
    select(date:pageViews) %>%
    group_by(pagePath) %>%
    pad_by_time(
        .date_var  = date,
        .by        = "day",
        .pad_value = 0
    ) %>%
    ungroup() %>%
    
    # Transform Target
    mutate(pageViews = log1p(pageViews)) %>%
    
    # Groupwise Data manipulation forecast horizon
    group_by(pagePath) %>%
    future_frame(
        .date_var   = date,
        .length_out = FORECAST_HORIZON,
        .bind_data  = TRUE
    ) %>%
    
    # Add features
    tk_augment_fourier(
        .date_var = date,
        .periods  = c(0.5 * FORECAST_HORIZON, FORECAST_HORIZON),
        .K = 1
    ) %>%
    
    # Lags and Rolling Lag features
    tk_augment_lags(
        .value = pageViews,
        .lags  = FORECAST_HORIZON
    ) %>%
    tk_augment_slidify(
        .value   = contains("pageViews_lag"),
        .f       = ~ mean(.x, na.rm = TRUE),
        .period  = c(7, FORECAST_HORIZON, 2*FORECAST_HORIZON),
        .partial = TRUE,
        .align   = "center"
    ) %>%
    ungroup() %>%
    rowid_to_column(var = "rowid")
    

# * Data Prepared ----
data_prepared_tbl <- full_data_tbl %>%
    filter(!is.na(pageViews)) %>%
    drop_na()

data_prepared_tbl %>%
    skim()

# * Future Data ----
future_tbl <- full_data_tbl %>%
    filter(is.na(pageViews)) %>%
    drop_na(pageViews_lag28)

future_tbl %>% skim()


# 2.0 TIME SPLIT ----
splits <- data_prepared_tbl %>%
    time_series_split(
        date_var  = date,
        assess    = FORECAST_HORIZON,
        cumulative = TRUE
    )

splits %>%
    tk_time_series_cv_plan() %>%
    plot_time_series_cv_plan(
        .date_var = date,
        .value = pageViews
    )

training(splits) %>%
    group_by(pagePath) %>%
    plot_time_series(
        .date_var = date,
        .value = pageViews,
        .facet_ncol = 4
    )

testing(splits) %>%
    group_by(pagePath) %>%
    plot_time_series(
        .date_var = date,
        .value = pageViews,
        .facet_ncol = 4
    )



# 3.0 GLUONTS MODELS ----

# * GLUON Recipe Specification ----
recipe_spec_gluon <- recipe(
    pageViews ~ pagePath + date + rowid,
    data = training(splits)
) %>%
    update_role(rowid, new_role = "indicator")


# * DeepAR Estimator ----

# Model 1: Default GluonTS
model_spec_1 <- deep_ar(
    id = "pagePath",
    freq = "D",
    prediction_length = FORECAST_HORIZON,
    
    # Training params
    epochs = 5,
    
    # Deep AR params
    cell_type = "lstm"
) %>%
    set_engine("gluonts_deepar")

wflw_fit_deepar_1 <- workflow() %>%
    add_model(model_spec_1) %>%
    add_recipe(recipe_spec_gluon) %>%
    fit(training(splits))

wflw_fit_deepar_1
    
# Model 2: Increase Epochs, Adjust Num Batches per Epoch
batch_num = round(sqrt(nrow(testing(splits))), 0)
model_spec_2 <- deep_ar(
    id = "pagePath",
    freq = "D",
    prediction_length = FORECAST_HORIZON,
    
    epochs = 10,
    num_batches_per_epoch = batch_num
) %>%
    set_engine("gluonts_deepar")

wflw_fit_deepar_2 <-  workflow() %>%
    add_recipe(recipe_spec_gluon) %>%
    add_model(model_spec_2) %>%
    fit(training(splits))


# Model 3: Increase Epochs, Adjust Num Batches Per Epoch, & Add Scaling 
model_spec_3 <- deep_ar(
    id = "pagePath",
    freq = "D",
    prediction_length = FORECAST_HORIZON,
    
    epochs = 10,
    num_batches_per_epoch = batch_num,
    scale = TRUE
) %>%
    set_engine("gluonts_deepar")

wflw_fit_deepar_3 <-  workflow() %>%
    add_recipe(recipe_spec_gluon) %>%
    add_model(model_spec_3) %>%
    fit(training(splits))


# * N-BEATS Estimator ----


# ** Modeltime Comparison ----

model_tbl_submodels <- modeltime_table(
    wflw_fit_deepar_1,
    wflw_fit_deepar_2,
    wflw_fit_deepar_3
)

# Forecast Accuracy
model_tbl_submodels %>%
    modeltime_accuracy(testing(splits))


# Forecast Visualization
forecast_submodels_test_tbl <- model_tbl_submodels %>%
    modeltime_forecast(
        new_data      = testing(splits)
        , actual_data = data_prepared_tbl
        , keep_data   = TRUE 
    ) 

forecast_submodels_test_tbl %>%
    group_by(pagePath) %>%
    plot_modeltime_forecast(
        .facet_ncol = 4
    )


# Investigate Model Failures
forecast_submodels_test_tbl %>%
    skim()

forecast_submodels_test_tbl %>%
    filter(is.na(.value)) %>%
    select(.model_id, .model_desc, .index, pagePath) %>%
    distinct(pagePath)


# Forecast Future








# 4.0 MACHINE LEARNING ----

# * Parallel Processing ----


# * ML Recipe Specification ----


# * XGBoost ----



# * End Parallel Processing ----




# 5.0 EVALUATION ----

# * Test Evaluations ----


# * Future Evaluations ----



# 6.0 ENSEMBLE ----

# * Make Ensemble ----


# * Evaluate Ensemble Test ----


# * Refit Ensemble & Evaluate Future ----


# 7.0 SAVING & LOADING ----


# * Save Submodels ----


# * Load Submodels ----


# * Make ensemble ----



# CONCLUSIONS ----

# DEEP LEARNING
# - [PROS] Create very powerful models by combining Machine Learning & Deep Learning
# - [PROS] Deep Learning is great for global modeling time series (1 model for > 1 Time Series Groups)
# COMBINING ML & DL
# - [CONS] Deep Learning does not factor in external regressors. 
#   - Solution 1: Run DL without. Run ML on the Residuals. 
#   - Solution 2: Create an Ensemble with ML & DL


