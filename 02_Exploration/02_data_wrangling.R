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
subscribers_daily_tbl %>%
    pad_by_time(
        .date_var = optin_time
        , .by = "day"
        , .pad_value = 0
    )



# * Weekly to Daily ----
transactions_tbl %>%
    pad_by_time(
        .date_var = purchased_at
        , .by = "day"
        , .start_date = "2018-06"
        , .pad_value = 
    ) %>%
    mutate_by_time(.by = "week", revenue = sum(revenue / 7, na.rm = TRUE))




# 3.0 FILTER BY TIME ----
# - Pare data down before modeling

# * Slicing - Everything after the BIG anomaly ----
subscribers_daily_tbl %>%
    filter_by_time(
        .date_var = optin_time
        , .start_date = "2018-11-20"
    ) %>%
    plot_time_series(
        .date_var = optin_time
        , .value = optins
    )

# * Zooming In - Just December 2018 ----
subscribers_daily_tbl %>%
    filter_by_time(
        .date_var = optin_time
        , .start_date = "2019-12"
        , .end_date = "2019-12"
    ) %>%
    plot_time_series(
        .date_var = optin_time
        , .value = optins
    )


# * Offsetting - Using plus-time and minus-time offsets to get things just right ----
subscribers_daily_tbl %>%
    filter_by_time(
        .date_var = optin_time
        , .start_date = "2019-12"
        , .end_date = "2019-12-01" %+time% "8 weeks"
    ) %>%
    plot_time_series(
        .date_var = optin_time
        , .value = optins
    )

# 4.0 MUTATING BY TIME -----
# - Get change from beginning/end of period

# * First, Last, Mean, Median by Period ----
transactions_tbl %>%
    mutate_by_time(
        .date_var = purchased_at
        , .by = "3 month"
        , revenue_mean = round(mean(revenue, na.rm = TRUE))
        , revenue_median = median(revenue)
        , revenue_max = max(revenue)
        , revenue_min = min(revenue)
    ) %>%
    pivot_longer(cols = contains("revenue")) %>%
    plot_time_series(
        .date_var = purchased_at
        , .value = value
        , .color_var = name
        , .smooth = FALSE
    )




# 5.0 JOINING BY TIME ----
# - Investigating Relationships
# - Identify External Regressors

# * Subscribers + GA Summary Web Traffic ----
subscribers_daily_padded_tbl <- subscribers_daily_tbl %>%
    pad_by_time(.date_var = optin_time, .pad_value = 0, .start_date = "2018-06")

subscribers_google_joined_daily_tbl <- subscribers_daily_padded_tbl %>%
    left_join(google_analytcs_summary_daily_tbl,
              by = c("optin_time" = "dateHour"))

# * Inspect Join -----
subscribers_google_joined_daily_tbl %>% plot_missing()

google_analytcs_summary_daily_tbl %>% tk_summary_diagnostics()

subscribers_google_joined_daily_tbl %>% tk_summary_diagnostics()

subscribers_google_joined_daily_tbl %>%
    pivot_longer(-optin_time) %>%
    plot_time_series(.date_var = optin_time, .value = value, .color_var = name)

# * Visualization Techniques (Relationships) ----
log_standardized_subscribers_google_joined_tbl <- subscribers_google_joined_daily_tbl %>%
    drop_na() %>%
    mutate(across(.cols = optins:sessions, .fns = log1p)) %>%
    mutate(across(.cols = optins:sessions, .fns = standardize_vec))
    
log_standardized_subscribers_google_joined_tbl %>%
    pivot_longer(-optin_time) %>%
    plot_time_series(
        .date_var = optin_time
        , .value = value
        , .color_var = name
        , .smooth = FALSE
        )

log_standardized_subscribers_google_joined_tbl %>%
    plot_acf_diagnostics(
        .date_var = optin_time
        , .value = optins
        , .ccf_vars = pageViews:sessions
        , .show_ccf_vars_only = TRUE
    )

# 6.0 WORKING WITH THE INDEX ----
# - Index Manipulations
subscribers_daily_padded_tbl %>%
    tk_index()

# * Making an index ----
tibble(
    date = tk_make_timeseries("2011",length_out = 100, by = "month")
    ,values = 1:100
)

# * Holiday Sequence ----
tk_make_holiday_sequence(
    start_date = "2011"
    , end_date = "2021"
    , calendar = "NYSE"
) %>%
    tk_get_holiday_signature() %>%
    glimpse()

tk_make_timeseries("2011") %>%
    tk_get_holiday_signature()

# * Offsetting time ----
"2011-01-01" %+time% "1 day"
"2011-01-01" %-time% "1 day"
"2011-01-01" %+time% "1 month"
"2011-01-01" %+time% "1 year"

tk_make_timeseries("2011") %+time% "1 year"

# * Extending an index ----
tk_make_timeseries("2011-01") %>%
    tk_make_future_timeseries(length_out = "1 month")

tk_make_timeseries("2011", by = "quarter") %>%
    tk_make_future_timeseries(length_out = 4)



# 7.0 FUTURE FRAME ----
# - Forecasting helper



# * Future Frame ----



# * Modeling ----



# * Visualizing ----



