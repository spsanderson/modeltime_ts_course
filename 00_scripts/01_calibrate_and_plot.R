calibrate_and_plot <-
function(..., type = "testing") {
    
    if (type == "testing") {
        new_data <- testing(splits)
    } else {
        new_data <- training(splits) %>% drop_na()
    }
    
    calibration_tbl <- modeltime_table(...) %>%
        modeltime_calibrate(new_data)
    
    print(calibration_tbl %>% modeltime_accuracy())
    
    calibration_tbl %>%
        modeltime_forecast(
            new_data = new_data,
            actual_data = data_prepared_tbl
        ) %>%
        plot_modeltime_forecast(.conf_interval_show = FALSE)
    
}
