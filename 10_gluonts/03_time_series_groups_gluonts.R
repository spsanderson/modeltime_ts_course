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


# 1.0 DATA ----

# * GA Data ----


# * Full Data ----



# * Data Prepared ----



# * Future Data ----



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


