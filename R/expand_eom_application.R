expand_eom_calendar_simple <- function(rotation_calendar,
                                       df,
                                       eom_id_var,
                                       eom_date_var,
                                       eom_q_var) {
    
    ## Extract eomation months safely (allow NA)
    df$eom_month <- ifelse(
        is.na(df[[eom_date_var]]),
        NA,
        as.numeric(substr(df[[eom_date_var]], 4, 5))
    )
 
    
    ## Join eomation info to calendar by crop.id
    cal <- rotation_calendar %>%
        left_join(
            df %>%
            select(
                   eom_q = all_of(eom_q_var),
                   eom_month),
            by = "crop.id"
        )
    
    ## Create eomation column
    cal$eomation <- 0
    
    month_id <- function(y, m) y * 12 + m
    
    for (i in seq_len(nrow(cal))) {
        
        if (is.na(cal$eom_q[i]) || cal$active[i] == 0) next
        
        start_m <- cal$eom_month[i]
        stop_m  <- cal$eom_stop_month[i]
        m       <- cal$month[i]
        
        if (is.na(start_m) | is.na(stop_m)) next
        
        ## Handle year crossing eomation (e.g. Oct → Feb)
        if (stop_m >= start_m) {
            in_window <- m >= start_m & m <= stop_m
        } else {
            in_window <- m >= start_m | m <= stop_m
        }
        
        if (in_window) {
            cal$eomation[i] <- cal$eom_q[i]
        }
    }
    
    ## Remove helper columns
    cal %>%
        select(-eom_q, -eom_month, -eom_stop_month)
}
