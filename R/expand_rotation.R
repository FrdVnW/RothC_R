expand_rotation_calendar <- function(
                                     df,
                                     sowing.var = "sowing.date",
                                     harvest.var = "harvest.date",
                                     start_year = 2020,
                                     n_years = 30) {
    
    df <- df %>%
        mutate(
            sow_month  = as.numeric(substr(df[[sowing.var]], 4, 5)),
            harv_month = as.numeric(substr(df[[harvest.var]], 4, 5))
        )

    ## df$sow_month <- ifelse(
    ##   is.na(df[[sowing.var]]),
    ##   NA,
    ##   as.numeric(substr(df[[sowing.var]], 4, 5))
    ## )
    
    ## df$harv_month <- ifelse(
    ##   is.na(df[[harvest.var]]),
    ##   NA,
    ##   as.numeric(substr(df[[harvest.var]], 4, 5))
    ## )
    
    ## # Remove rows where crop does not exist
    ## df <- df[!(is.na(df$sow_month) | is.na(df$harv_month)), ]
    
    
    
    calendar <- expand.grid(
        year  = start_year:(start_year + n_years - 1),
        month = 1:12
    ) %>%
        arrange(year, month) %>%
        mutate(
            crop.id = NA,
            active  = 0,
            sowing  = FALSE,
            harvest = FALSE
        )
    
    month_id <- function(y, m) y * 12 + m
    
    current_year  <- start_year
    current_month <- df$sow_month[1]
    crop_index    <- 1
    end_year      <- start_year + n_years - 1

    if (dim(df)[1] > 0){
        
        while (current_year <= end_year) {
            
            sow_m  <- df$sow_month[crop_index]
            harv_m <- df$harv_month[crop_index]
            cropid <- df$crop.id[crop_index]
            
            ## --- find sowing year (>= current month allowed) ---
            if (current_month <= sow_m) {
                sow_year <- current_year
            } else {
                sow_year <- current_year + 1
            }
            
            if (sow_year > end_year) break
            
            ## harvest year
            if (harv_m >= sow_m) {
                harv_year <- sow_year
            } else {
                harv_year <- sow_year + 1
            }
            
            ## activate months INCLUDING harvest month
            idx <- which(
                month_id(calendar$year, calendar$month) >= month_id(sow_year, sow_m) &
                month_id(calendar$year, calendar$month) <= month_id(harv_year, harv_m)
            )
            
            calendar$crop.id[idx] <- cropid
            calendar$active[idx]  <- 1
            
            ## mark sowing & harvest
            calendar$sowing[calendar$year == sow_year &
                            calendar$month == sow_m] <- TRUE
            
            calendar$harvest[calendar$year == harv_year &
                             calendar$month == harv_m] <- TRUE
            
            ## move pointer:
            ## next crop can start sowing in SAME harvest month
            current_year  <- harv_year
            current_month <- harv_m   ## ← not +1 anymore
            
            crop_index <- crop_index + 1
            if (crop_index > nrow(df)) crop_index <- 1
        }
    }
    
    calendar
}
