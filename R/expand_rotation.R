expand_rotation_calendar <- function(
                                     df,
                                     sowing.var = "sowing.date",
                                     harvest.var = "harvest.date",
                                     start_year = 2020,
                                     n_years = 30
                                     ) {

    false_start_year <- start_year - dim(df)[1]
    
    df <- df %>%
        mutate(
            sow_month  = as.numeric(substr(df[[sowing.var]], 4, 5)),
            harv_month = as.numeric(substr(df[[harvest.var]], 4, 5))
        )

    calendar <- expand.grid(
        year  = false_start_year:(start_year + n_years - 1),
        month = 1:12
    ) %>%
        arrange(year, month) %>%
        mutate(
            rotation.cycle = NA,
            prevcrop.id = NA,
            crop.id = NA,
            nextcrop.id = NA,
            active  = 0,
            sowing  = FALSE,
            harvest = FALSE
        )
    
    month_id <- function(y, m) y * 12 + m
    
    current_year   <- false_start_year
    current_month  <- df$sow_month[1]
    crop_index     <- 1
    rotation.cycle <- 0
    prevcrop_index <- nrow(df) 
    nextcrop_index <- min(crop_index+1,nrow(df))
    end_year       <- start_year + n_years - 1

    if (dim(df)[1] > 0){
        
        while (current_year <= end_year) {
            
            sow_m  <- df$sow_month[crop_index]
            harv_m <- df$harv_month[crop_index]
            cropid <- df$crop.id[crop_index]
            prevcropid <- df$crop.id[prevcrop_index]
            nextcropid <- df$crop.id[nextcrop_index]
            
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
                is.na(calendar$crop.id) &
                month_id(calendar$year, calendar$month) >= month_id(sow_year, sow_m) &
                month_id(calendar$year, calendar$month) <= month_id(harv_year, harv_m)
            )

            calendar$rotation.cycle[last(idx):nrow(calendar)] <- rotation.cycle
            calendar$crop.id[idx] <- cropid
            calendar$prevcrop.id[last(idx):nrow(calendar)] <- cropid
            calendar$nextcrop.id[first(idx):nrow(calendar)] <- nextcropid
            calendar$prevcrop.id[idx] <- prevcropid
            calendar$active[idx]  <- 1
            
            ## mark sowing & harvest
            calendar$sowing[calendar$year == sow_year &
                            calendar$month == sow_m] <- TRUE
            
            calendar$harvest[calendar$year == harv_year &
                             calendar$month == harv_m] <- TRUE

            ## After harvest, assign next crop.id immediately

            next_index <- crop_index + 1
            if (next_index > nrow(df)) next_index <- 1

            next_cropid <- df$crop.id[next_index]

            ## assign crop.id from month AFTER harvest until next sowing
            start_fill_id <- month_id(harv_year, harv_m) + 1

            ## find next sowing
            next_sow_m <- df$sow_month[next_index]

            if (harv_m <= next_sow_m) {
                next_sow_year <- harv_year
            } else {
                next_sow_year <- harv_year + 1
            }

            end_fill_id <- month_id(next_sow_year, next_sow_m) - 1

            fill_idx <- which(
                month_id(calendar$year, calendar$month) >= start_fill_id &
                month_id(calendar$year, calendar$month) <= end_fill_id
            )

            calendar$crop.id[fill_idx] <- next_cropid

            ## move pointer:
            ## next crop can start sowing in SAME harvest month
            current_year  <- harv_year
            current_month <- harv_m   ## ← not +1 anymore
            
            crop_index <- crop_index + 1
            if (crop_index > nrow(df)) {
                crop_index <- 1
                rotation.cycle <- rotation.cycle+1
            }
            prevcrop_index <- prevcrop_index + 1
            if (prevcrop_index > nrow(df)) prevcrop_index <- 1
            nextcrop_index <- nextcrop_index + 1
            if (nextcrop_index > nrow(df)) nextcrop_index <- 1
        }
    }
    
    ## calendar[-(1:(12*dim(df)[1])),]
    return(calendar)
}
