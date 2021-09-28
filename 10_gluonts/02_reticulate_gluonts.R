# BUSINESS SCIENCE UNIVERSITY ----
# DS4B 203-R: TIME SERIES FORECASTING  ----
# MODULE: DEEP LEARNING - RETICULATE & GLUONTS ----

# GOAL: Forecast Weekly Revenue (Simple Case: Single Time Series)

# OBJECTIVES ----
# - Work with GluonTS via Reticulate
# - Understand the GluonTS List Dataset
# - Make a GluonTS Probabilistic Forecast via Reticulated Python
# - Showcase Modeltime GluonTS in R


# 1.0 LIBRARIES & SETUP ----

# Time Series ML
library(tidymodels)
library(modeltime)
library(modeltime.gluonts)

# Python 
library(reticulate)

# Plotting 
library(plotly)

# Core 
library(tidyverse)
library(timetk)


# 2.0 PYTHON & RETICULATE ----

# Check Environment - Should be 'r-gluonts'

py_discover_config()


# * Import Key Packages ----

gluonts <- import("gluonts", convert = FALSE)
py <- import_builtins(convert = FALSE)


# * Using Python & R Together ----
py$abs(-5) %>% class()
py$abs(-5) %>% py_to_r() %>% class()
r_to_py(5) %>% class()

my_dict <- py$dict("key1" = "value1", "key2" = "value2")
my_dict %>% class()
my_dict$key1
my_dict["key1"] %>% py_to_r() %>% class()

# 3.0 DATA ----

# * Transactions Data ----
transactions_tbl <- read_rds("00_data/transactions_weekly.rds")
transactions_tbl %>%
    plot_time_series(
        purchased_at,
        revenue
    )


# 4.0 DATA PROCESSING ----

full_data_tbl <- transactions_tbl %>%
    mutate(id = "total_revenue") %>%
    group_by(id) %>%
    future_frame(
        .length_out = 12,
        .bind_data = TRUE
    ) %>%
    ungroup()

data_prepared_tbl <- full_data_tbl %>%
    filter(!is.na(revenue))

future_tbl <- full_data_tbl %>%
    filter(is.na(revenue))


# 5.0 GLUONTS LIST DATASET ----

# * Creating a gluonts ListDataset ----
?to_gluon_list_dataset()
data_prepared_list_dataset <- data_prepared_tbl %>%
    to_gluon_list_dataset(
        date_var  = purchased_at,
        value_var = revenue,
        id_var    = id
    )


# * Examining a ListDataset ----
data_prepared_list_dataset %>% class()

data_prepared_list_dataset$list_data

data_prepared_list_dataset$list_data %>% py_to_r()

data_prepared_list_dataset$list_data %>% class()

data_prepared_list_dataset$list_data[0]


# * Converting to Pandas ----
to_pandas <- gluonts$dataset$util$to_pandas

data_prepared_list_dataset$list_data[0] %>% to_pandas


# 6.0 DeepAR Estimator ----
# - Documentation: https://ts.gluon.ai/api/gluonts/gluonts.model.deepar.html

# * Connect to Model & Trainer


# * Model Specification 


# * Fitting the GluonTS Model



# 7.0 PREDICTION ----

# * Prediction Object ----



# * Probabilistic Forecasting  -----



# 8.0 MATPLOTLIB PROBABILISTIC VISUALIZATION ----



# 9.0 GGPLOT & PLOTLY PROBABILISTIC FORECAST VISUALIZATION ----



# 10.0 MODELTIME ----



# 11.0 SAVING / LOADING MODELS ----

# * GluonTS ----




# * Modeltime ----





# 12.0 BONUS - Deep Factor Estimator ----

# * Model Specification 


# * Fitting the GluonTS DeepFactor Model ----


# * Visualize  ----




# CONCLUSIONS ----

# PROS & CONS:
# - GLUONTS 
#   - [PRO] Wide range of algorithms
#   - [PRO] One round of training and probabilities are incorporated (for some models)
#   - [CON] More complex to work with ListDataset Structure
# - MODELTIME 
#   - [PRO] Simplifies creating ListDataset() objects
#   - [PRO] Can compare w/ other forecast algorithms (shown next)
#   - [PRO] Good for scaling up predictions (shown next)
#   - [CON] Requires 2 rounds of training to get confidence intervals
#   - [CON] Not all GluonTS Algorithms incorporated (more coming)