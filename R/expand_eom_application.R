expand_eom_calendar <- function(rotation_calendar,
                                df,
                                eom_id_var = "eom.id",
                                eom_date_var = "eom.date",
                                eom_q_var = "eom.q") {
    
    ## Extract eom_app_q months safely (allow NA)
    df$eom_month <- ifelse(
        is.na(df[[eom_date_var]]),
        NA,
        as.numeric(substr(df[[eom_date_var]], 4, 5))
    )  
    
    cal <- rotation_calendar
    cal$eom_app_id <- as.integer(NA)
    cal$eom_app_q <- 0

    ## stop("Work in progress")
    
    for (crop_index in 1:nrow(df)) {

        ## lignes à mettre à jour
        targets <- cal %>%
            mutate(is_target =
                       ((crop.id == df$crop.id[crop_index] & !harvest) |
                        (nextcrop.id == df$crop.id[crop_index] & harvest)) &
                       month == df$eom_month[crop_index]
                   ) %>%
            group_by(rotation.cycle) %>%
            filter(is_target) %>%
            slice(1) %>%     # <-- première occurrence par groupe
            ungroup() %>%
            mutate(
                eom_app_id  = df[[eom_id_var]][crop_index],
                eom_app_q   = df[[eom_q_var]][crop_index] #,
                ## eom_month   = df$eom_month[crop_index]
            ) %>%
            dplyr::select(-is_target)
        

        ## mise à jour dans cal
        cal <- rows_update(cal, targets, by = c("year","month"))
    }
    return(cal)
}
