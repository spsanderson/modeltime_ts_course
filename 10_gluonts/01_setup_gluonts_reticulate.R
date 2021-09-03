# BUSINESS SCIENCE UNIVERSITY ----
# DS4B 203-R: TIME SERIES FORECASTING  ----
# MODULE: DEEP LEARNING - GLUONTS & RETICULATE SETUP ----

# OBJECTIVES ----
# - Install GluonTS
# - Work with Python Environments via Reticulate

# REQUIREMENTS ----
# - Install latest Modeltime / Timetk Suite: 

# devtools::install_github("business-science/modeltime")
# devtools::install_github("business-science/modeltime.gluonts")
# devtools::install_github("business-science/modeltime.ensemble")
# devtools::install_github("business-science/modeltime.resample")
# devtools::install_github("business-science/timetk")



# 1.0 SETUP ----
# - Get the default 'r-gluonts' conda environment set up

# * Step 1: Load the Library ----
library(modeltime.gluonts)

# * Step 2: Install the Python Environment ----


# * Step 3: Restart R Session ----



# * Troubleshooting ----

# 1. No conda?



# 2. Windows - Need Visual Studio C++ for Python
"https://github.com/business-science/modeltime.gluonts/issues/4"



# 2.0 TEST ----
# - Load modeltime.gluonts
# - Activates the 'r-gluonts' (default) environment

# Test a GluonTS DeepAR Model


# Data



# 3.0 RETICULATE NAVIGATION ----



# * Which Environments are Available? ----



# * Which Environment am I Using? ----



# * What's in My Environment? ----



# 4.0 SETTING UP A CUSTOM PYTHON ENVIRONMENT ----

# Py Install

# Find the Environment Python


# 5.0 ACTIVATING A CUSTOM PYTHON ENV ----

# * Step 1. Requires a Session Restart ----



# * Step 2. Get the Path to your Python Environment ----


# * Step 3. Set an R environment variable ----


# Step 4. Load Modeltime - It will override the default using the new python environment


# Step 5. Common Gotchas:
# - If you've already activated an environment or run library(modeltime), you need to restart R
#   and set the Sys.setenv(GLUONTS_PYTHON) with the path to your python and run this first


# 6.0 RE-ACTIVATING THE DEFAULT ENV ----
# - Requires session restart




