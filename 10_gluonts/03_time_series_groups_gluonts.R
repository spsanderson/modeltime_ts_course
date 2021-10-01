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




# 3.0 GLUONTS MODELS ----

# * GLUON Recipe Specification ----



# * DeepAR Estimator ----

# Model 1: Default GluonTS



# Model 2: Increase Epochs, Adjust Num Batches per Epoch



# Model 3: Increase Epochs, Adjust Num Batches Per Epoch, & Add Scaling 


# * N-BEATS Estimator ----


# ** Modeltime Comparison ----

# Forecast Accuracy



# Forecast Visualization


# Investigate Model Failures


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


