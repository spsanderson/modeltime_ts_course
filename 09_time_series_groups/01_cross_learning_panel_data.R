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

ga_page_raw_tbl %>%
    
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
    })


# * Data Prepared ----


# * Future Data ----


# 2.0 TIME SPLIT ----



# 3.0 RECIPE ----

# * Clean Training Set ----
# - With Panel Data, need to do this outside of a recipe
# - Transformation happens by group


# * Recipe Specification ----



# 4.0 MODELS ----
# - !!! REMINDER: Cannot use sequential models !!!

# * PROPHET ----


# * XGBOOST ----



# * PROPHET BOOST ----



# * SVM ----



# * RANDOM FOREST ----



# * NNET ----



# * MARS ----



# * ACCURACY CHECK ----




# 5.0 HYPER PARAMETER TUNING ---- 

# * RESAMPLES - K-FOLD ----- 


# * XGBOOST TUNE ----

# ** Tunable Specification


# ** Tuning


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


