expand_eom_calendar <- function(rotation_calendar,
                                df,
                                eom_id_var = "eom.id",
                                eom_date_var = "eom.date",
                                eom_q_var = "eom.q") {
    ##:ess-bp-start::browser@nil:##
browser(expr=is.null(.ESSBP.[["@3@"]]));##:ess-bp-end:##
    
    ## Extract eom_app_q months safely (allow NA)
    df$eom_month <- ifelse(
        is.na(df[[eom_date_var]]),
        NA,
        as.numeric(substr(df[[eom_date_var]], 4, 5))
    )  
    
    ## Join eom_app_q info to calendar by crop.id
    ## cal <- rotation_calendar %>%
    ##     left_join(
    ##         df %>%
    ##         select(crop.id,
    ##                eom_id = all_of(eom_id_var),
    ##                eom_q = all_of(eom_q_var),
    ##                eom_month),
    ##         by = "crop.id"
    ##     )
    cal <- rotation_calendar
    cal$eom_app_id <- NA    
    cal$eom_app_q <- 0

    stop("Work in progress")
    
    for (crop_index in 1:nrow(df)) {
        
        cal[
        (
            (cal$crop.id == df$crop.id[crop_index] &
             cal$harvest == FALSE) |
            (cal$nextcrop.id == df$crop.id[crop_index] &
             cal$harvest == TRUE)
        ) &
        cal$month == df$eom_month[crop_index]
       ,
        c("eom_id","eom_q","eom_month")
        ]  <- 
           }

    cal[1:36,c("month","crop.id","eom_month","eom_q")]

    
    ## Create eom_app_q column

    
    ## month_id <- function(y, m) y * 12 + m
    
    for (i in seq_len(nrow(cal))) {
        
        if (is.na(cal$eom_q[i])) next
        
        start_m <- cal$eom_month[i]
        m       <- cal$month[i]
        
        if (is.na(start_m)) next
        

        if (start_m == m) {
            cal$eom_app_id[i] <- cal$eom_id[i]
            cal$eom_app_q[i] <- cal$eom_q[i]
        }
    }
    
    ## Remove helper columns
    cal %>%
        select(-eom_q, -eom_month)
}
