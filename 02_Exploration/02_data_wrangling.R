# BUSINESS SCIENCE UNIVERSITY
# DS4B 203-R: TIME SERIES FORECASTING FOR BUSINESS
# MODULE: TIME SERIES DATA WRANGLING ----


# GOAL ----
# - Gain exposure to timetk data wrangling functionality

# OBJECTIVES ----
# - Summarize/Pad - Manipulate Data to different periodicities (scales, intervals)
# - Filter - Zoom in & Slice Time Series
# - Mutate - Apply mutations by time groups
# - Joining Time Series
# - Index Operations
# - Future Frame - Forecasting Exposure



# LIBRARIES ----

library(tidyverse)
library(timetk)
library(lubridate)
library(DataExplorer)

# DATA ----

google_analytics_summary_tbl <- read_rds("00_data/google_analytics_summary_hourly.rds")
google_analytics_summary_tbl 

mailchimp_users_tbl  <- read_rds("00_data/mailchimp_users.rds")
mailchimp_users_tbl 

transactions_tbl  <- read_rds("00_data/transactions_weekly.rds")
transactions_tbl 


# 1.0 SUMMARIZE BY TIME  ----
# - APPLY COMMON AGGREGATIONS
# - HIGH TO LOW FREQ

# * To Daily - Subscribers ----
subscribers_daily_tbl <- mailchimp_users_tbl %>%
    summarise_by_time(
        .date_var = optin_time
        , .by = "day"
        , optins = n()
    )

# Grouped
mailchimp_users_tbl %>%
    group_by(member_rating) %>%
    summarise_by_time(
        .date_var = optin_time
        , .by = "day"
        , optins = n()
    ) %>%
    plot_time_series(
        .date_var = optin_time
        , .value = optins
    )


# * To Daily - GA Summary ----
google_analytcs_summary_daily_tbl <- google_analytics_summary_tbl %>%
    mutate(dateHour = ymd_h(dateHour)) %>%
    summarise_by_time(
        .date_var = dateHour
        , .by = "day"
        , across(pageViews:sessions, .fns = sum)
    )

google_analytcs_summary_daily_tbl %>%
    pivot_longer(
        pageViews:sessions
    ) %>%
    plot_time_series(
        .date_var = dateHour
        , .value = value
        , .facet_vars = name
    )

# * To Weekly - Subscribers ----
subscribers_weekly_tbl <- mailchimp_users_tbl %>%
    summarize_by_time(
        .date_var = optin_time
        , .by = "week"
        , optins = n()
    )


# * To Monthly - Transactions ----
transactions_monthly_tbl <- transactions_tbl %>%
    summarise_by_time(
        .date_var = purchased_at
        , .by = "month"
        , revenue = sum(revenue)
    )

# Floor, ceiling, round
transactions_tbl %>%
    summarise_by_time(
        .date_var = purchased_at
        , .by = "month"
        , revenue = sum(revenue)
        , .type = "floor"
    )

transactions_tbl %>%
    summarise_by_time(
        .date_var = purchased_at
        , .by = "month"
        , revenue = sum(revenue)
        , .type = "celieng"
    ) %>%
    mutate(purchased_at = purchased_at %-time% "1 day")

transactions_tbl %>%
    summarise_by_time(
        .date_var = purchased_at
        , .by = "month"
        , revenue = sum(revenue)
        , .type = "round"
    )    

# 2.0 PAD BY TIME ----
# - Filling in Gaps
# - Going from Low to High Frequency (un-aggregating)

# * Fill Daily Gaps ----




# * Weekly to Daily ----





# 3.0 FILTER BY TIME ----
# - Pare data down before modeling

# * Slicing - Everything after the BIG anomaly ----


# * Zooming In - Just December 2018 ----


# * Offsetting - Using plus-time and minus-time offsets to get things just right ----


# 4.0 MUTATING BY TIME -----
# - Get change from beginning/end of period

# * First, Last, Mean, Median by Period ----





# 5.0 JOINING BY TIME ----
# - Investigating Relationships
# - Identify External Regressors

# * Subscribers + GA Summary Web Traffic ----


# * Inspect Join -----


# * Visualization Techniques (Relationships) ----




# 6.0 WORKING WITH THE INDEX ----
# - Index Manipulations

# * Making an index ----


# * Holiday Sequence ----


# * Offsetting time ----


# * Extending an index ----




# 7.0 FUTURE FRAME ----
# - Forecasting helper



# * Future Frame ----



# * Modeling ----



# * Visualizing ----



