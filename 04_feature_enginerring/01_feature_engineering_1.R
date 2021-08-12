# BUSINESS SCIENCE UNIVERSITY
# DS4B 103-R: TIME SERIES FORECASTING FOR BUSINESS
# MODULE: FEATURE ENGINEERING

# GOAL ----
# - FIND ENGINEERED FEATURES BEFORE MODELING

# OBJECTIVES:
# - Time-Based Features - Trend-Based & Seasonal Features
# - Interactions
# - Fourier Series
# - Autocorrelated Lags
# - Special Events
# - External Regressor Lags

# LIBRARIES & DATA ----

# Core
library(tidyverse)
library(lubridate)
library(timetk)
library(plotly)

# Data
google_analytics_summary_tbl <- read_rds("00_data/google_analytics_summary_hourly.rds")

learning_labs_tbl <- read_rds("00_data/learning_labs.rds") 

subscribers_tbl   <- read_rds("00_data/mailchimp_users.rds")


# DATA PREPARATION ----
# - Apply Preprocessing to Target
data_prepared_tbl <- subscribers_tbl %>%
    summarize_by_time(
        optin_time
        , .by = "day"
        , optins = n()
    ) %>%
    pad_by_time(
        .date_var = optin_time
        , .pad_value = 0
    ) %>%
    
    # Preprocess
    mutate(optins_trans = log_interval_vec(optins, limit_lower = 0, offset = 1)) %>%
    mutate(optins_trans = standardize_vec(optins_trans)) %>%
    # fix missing values at beginning of series
    filter_by_time(.start_date = "2018-07-03") %>%
    
    # Cleaning
    mutate(optins_trans_cleaned = ts_clean_vec(optins_trans, period = 7)) %>%
    mutate(optins_trans = ifelse(optin_time %>% between_time("2018-11-18","2018-11-20"),
                                 optins_trans_cleaned,
                                 optins_trans)) %>%
    select(-optins, -optins_trans_cleaned)

data_prepared_tbl %>%
    pivot_longer(-optin_time) %>%
    plot_time_series(
        .date_var    = optin_time
        , .value     = value
        , .color_var = name
    )

# FEATURE INVESTIGATION ----

# 1.0 TIME-BASED FEATURES ----
# - tk_augment_timeseries_signature()

# * Time Series Signature ----

data_prep_signature_tbl <- data_prepared_tbl %>%
    tk_augment_timeseries_signature() %>%
    select(-diff, -contains(".iso"), -contains(".xts"), 
           -matches("(hour)|(minute)|(second)|(am.pm)"))

data_prep_signature_tbl %>%
    glimpse()

# * Trend-Based Features ----

# ** Linear Trend

data_prep_signature_tbl %>%
    plot_time_series_regression(
        .date_var = optin_time
        , .formula = optins_trans ~ index.num
    )

# ** Nonlinear Trend - Basis Splines

data_prep_signature_tbl %>%
    plot_time_series_regression(
        optin_time
        , .formula = optins_trans ~ splines::bs(index.num, degree = 3)
    )

data_prep_signature_tbl %>%
    plot_time_series_regression(
        optin_time
        , .formula = optins_trans ~ splines::ns(index.num, df = 3)
    )

data_prep_signature_tbl %>%
    plot_time_series_regression(
        optin_time
        , .formula = optins_trans ~ splines::ns(index.num
                                                 , knots = quantile(
                                                     index.num, probs = c(0.33)
        ))
    )

# * Seasonal Features ----

# ** Weekly Seasonality

data_prep_signature_tbl %>%
    plot_time_series_regression(
        optin_time
        , .formula = optins_trans ~ wday.lbl
    )


# ** Monthly Seasonality

data_prep_signature_tbl %>%
    plot_time_series_regression(
        optin_time
        , .formula = optins_trans ~ month.lbl
    )

# ** Together with Trend

model_formula_seasonality <- as.formula(
    optins_trans ~ splines::ns(index.num,
                               knots = quantile(
                                   index.num
                                   , prob = c(.025,0.5)
                               ))
    + wday.lbl
    + month.lbl
    + .
)

data_prep_signature_tbl %>%
    plot_time_series_regression(
        optin_time
        , .formula = model_formula_seasonality
        , .show_summary = TRUE
    )

# 2.0 INTERACTIONS ----

model_formula_interactions <- as.formula(
    optins_trans ~ splines::ns(index.num,
                               knots = quantile(
                                   index.num
                                   , prob = c(.025,0.5)
                               ))
    + .
    + (as.factor(week2) * wday.lbl)
)

data_prep_signature_tbl %>%
    plot_time_series_regression(
        optin_time
        , .formula = model_formula_interactions
        , .show_summary = TRUE
    )

# 3.0 FOURIER SERIES ----
# - tk_augment_fourier

# Data Prep
data_prep_signature_tbl %>% 
    plot_acf_diagnostics(
        optin_time
        , optins_trans
    )

data_prep_fourier_tbl <- data_prep_signature_tbl %>%
    tk_augment_fourier(
        optin_time
        , .periods = c(7,14,30,90,365)
        , .K = 2
    )

# Model

model_formula_fourier <- as.formula(
    optins_trans ~ splines::ns(index.num,
                               knots = quantile(
                                   index.num
                                   , prob = c(.025,0.5)
                               ))
    + .
    + (as.factor(week2) * wday.lbl)
)


# Visualize
data_prep_fourier_tbl %>%
    plot_time_series_regression(
        optin_time
        , .formula = model_formula_fourier
        , .show_summary = TRUE
    )


# 4.0 LAGS ----
# - tk_augment_lags()

# Data Prep
data_prep_fourier_tbl %>%
    plot_acf_diagnostics(.date_var = optin_time, .value = optins_trans,
                         .lags = (8*7 + 1):600)

data_prep_lags_tbl <- data_prep_fourier_tbl %>%
    tk_augment_lags(
        .value = optins_trans
        , .lags = c(57, 63, 70)
    ) %>%
    drop_na()

# Model
model_formula <- as.formula(
    optins_trans ~ splines::ns(index.num,
                               knots = quantile(
                                   index.num
                                   , prob = c(.025, .50)
                               ))
    + .
    + (as.factor(week2) * wday.lbl)
)


# Visualize
data_prep_lags_tbl %>%
    plot_time_series_regression(
        .date_var = optin_time
        , .formula = model_formula
        , .show_summary = TRUE
    )


# 5.0 SPECIAL EVENTS ----

# Data Prep
learning_labs_daily_tbl <- learning_labs_tbl %>%
    mutate(event_date = ymd_hms(event_date)) %>%
    summarise_by_time(.date_var = event_date, .by = "day", event = n())

data_prep_events_tbl <- data_prep_fourier_tbl %>%
    left_join(learning_labs_daily_tbl, by = c("optin_time" = "event_date")) %>%
    mutate(event = ifelse(is.na(event), 0, event))

g <- data_prep_events_tbl %>%
    plot_time_series(optin_time, optins_trans, .interactive = FALSE) +
    geom_point(color = "red", data = . %>% filter(event == 1))

ggplotly(g)

# Model
model_formula <- as.formula(
    optins_trans ~ splines::ns(index.num,
                               knots = quantile(
                                   index.num
                                   , prob = c(.25, .50)
                               ))
    + .
    + (as.factor(week2) * wday.lbl)
)


# Visualize

data_prep_events_tbl %>%
    plot_time_series_regression(
        optin_time
        , .formula = model_formula
        , .show_summary = TRUE
    )

# 6.0 EXTERNAL LAGGED REGRESSORS ----
# - xregs

# Data Prep
google_analytics_prep_tbl <- google_analytics_summary_tbl %>%
    mutate(date = ymd_h(dateHour)) %>%
    summarise_by_time(
        .date_var = date
        , .by = "day"
        , across(pageViews:sessions, .fns = sum)
    ) %>%
    mutate(across(pageViews:sessions, .fns = log1p)) %>%
    mutate(across(pageViews:sessions, .fns = standardize_vec))

data_prep_google_tbl <- data_prep_events_tbl %>%
    left_join(google_analytics_prep_tbl, by = c("optin_time"="date"))

data_prep_google_tbl <- data_prep_google_tbl %>%
    drop_na() # not good dropped half the data

data_prep_google_tbl %>%
    plot_acf_diagnostics(
        optin_time, optins_trans,
        .ccf_vars = pageViews:sessions,
        .show_ccf_vars_only = TRUE
    )

# Model
model_formula <- as.formula(
    optins_trans ~ splines::ns(index.num,
                               knots = quantile(
                                   index.num
                                   , prob = c(.25, .50)
                               ))
    + .
    + (as.factor(week2) * wday.lbl)
)


# Visualize
data_prep_google_tbl %>%
    plot_time_series_regression(
        .date_var = optin_time
        , .formula = model_formula
        , .show_summary = TRUE
    )

data_prep_google_tbl %>%
    select(optin_time, optins_trans, pageViews) %>%
    pivot_longer(-optin_time) %>%
    plot_time_series(
        .date_var = optin_time
        , .value = value
        , .color_var = name
        , .smooth = FALSE
    )

# 7.0 RECOMMENDATION ----
# - Best model: 
# - Best Model Formula:
# Model
model_formula <- as.formula(
    optins_trans ~ splines::ns(index.num,
                               knots = quantile(
                                   index.num
                                   , prob = c(.25, .50)
                               ))
    + .
    + (as.factor(week2) * wday.lbl)
)


# Visualize

data_prep_events_tbl %>%
    plot_time_series_regression(
        optin_time
        , .formula = model_formula
        , .show_summary = TRUE
    )

# Linear Regression Model

model_fit_best_lm <- lm(model_formula, data = data_prep_events_tbl)

model_fit_best_lm$terms %>% formula()

write_rds(model_fit_best_lm, file = "00_models/model_fit_best_lm.rds")
read_rds("00_models/model_fit_best_lm.rds") %>% summary()
