expand_irrigation_calendar <- function(
                                       rotation_calendar,
                                       df,
                                       irrig_q_var,
                                       irrig_start_var,
                                       irrig_stop_var
                                       ) {
    
    ## Extract irrigation months safely (allow NA)
    df$irrig_start_month <- ifelse(
        is.na(df[[irrig_start_var]]),
        NA,
        as.numeric(substr(df[[irrig_start_var]], 4, 5))
    )
    
    df$irrig_stop_month <- ifelse(
        is.na(df[[irrig_stop_var]]),
        NA,
        as.numeric(substr(df[[irrig_stop_var]], 4, 5))
    )
    
    ## Join irrigation info to calendar by crop.id
    cal <- rotation_calendar %>%
        left_join(
            df %>%
            select(crop.id,
                   irrig_q = all_of(irrig_q_var),
                   irrig_start_month,
                   irrig_stop_month),
            by = "crop.id"
        )
    
    ## Create irrigation column
    cal$irrigation <- 0
    
    month_id <- function(y, m) y * 12 + m
    
    for (i in seq_len(nrow(cal))) {
        
        if (is.na(cal$irrig_q[i]) || cal$active[i] == 0) next
        
        start_m <- cal$irrig_start_month[i]
        stop_m  <- cal$irrig_stop_month[i]
        m       <- cal$month[i]
        
        if (is.na(start_m) | is.na(stop_m)) next
        
        ## Handle year crossing irrigation (e.g. Oct → Feb)
        if (stop_m >= start_m) {
            in_window <- m >= start_m & m <= stop_m
        } else {
            in_window <- m >= start_m | m <= stop_m
        }
        
        if (in_window) {
            cal$irrigation[i] <- cal$irrig_q[i]
        }
    }
    
    ## Remove helper columns
    cal %>%
        select(-irrig_q, -irrig_start_month, -irrig_stop_month)
}
