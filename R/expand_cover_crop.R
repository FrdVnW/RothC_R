expand_cover_crop <- function(rotation_calendar,
                              df,
                              cover_id_var = "covercrop.id",
                              cover_start_var = "covercrop.sowing.date",
                              cover_stop_var = "covercrop.destruction.date") {
  
  # Safe month extraction
  get_month <- function(x){
    if(length(x) == 0) return(rep(NA_integer_, length(x)))
    ifelse(is.na(x), NA_integer_, as.numeric(substr(x, 4, 5)))
  }
  
  df$cover_start_month <- get_month(df[[cover_start_var]])
  df$cover_stop_month  <- get_month(df[[cover_stop_var]])
  
  # Initialize cover crop columns in calendar
  cal <- rotation_calendar %>%
    dplyr::mutate(
      cover.id = NA,
      cover.active = 0,
      covercrop.sowing = FALSE,
      covercrop.harvest = FALSE
    ) %>%
    dplyr::left_join(
      df %>%
        dplyr::select(
          crop.id,
          cover_id = all_of(cover_id_var),
          cover_start_month,
          cover_stop_month
        ),
      by = "crop.id"
    )
  
  month_id <- function(y, m) y * 12 + m
  
  current_year  <- min(cal$year)
  
  # iterate over rotation calendar row by row
  for(i in seq_len(nrow(cal))){
    
    # skip if no cover crop defined
    if(is.na(cal$cover_id[i]) || 
       is.na(cal$cover_start_month[i]) || 
       is.na(cal$cover_stop_month[i])) next
    
    # main crop harvest month
    main_harvest <- ifelse(cal$harvest[i], cal$month[i], NA)
    if(is.na(main_harvest)) next
    
    sow_m  <- cal$cover_start_month[i]
    harv_m <- cal$cover_stop_month[i]
    
    # Determine cover crop sow year
    sow_year <- cal$year[i]
    if(sow_m < main_harvest) sow_year <- sow_year + 1
    
    # Determine harvest year
    harv_year <- sow_year
    if(harv_m < sow_m) harv_year <- sow_year + 1
    
    # select calendar rows that match this crop
    idx <- which(
      month_id(cal$year, cal$month) >= month_id(sow_year, sow_m) &
      month_id(cal$year, cal$month) <= month_id(harv_year, harv_m)
    )
    
    # fill cover crop info
    cal$cover.id[idx]         <- cal$cover_id[i]
    cal$cover.active[idx]     <- 1
    
    # mark sowing and harvest
    sow_idx <- which(cal$year == sow_year & cal$month == sow_m)
    harv_idx <- which(cal$year == harv_year & cal$month == harv_m)
    cal$covercrop.sowing[sow_idx]  <- TRUE
    cal$covercrop.harvest[harv_idx] <- TRUE
  }
  
  # remove helper columns
  cal %>% dplyr::select(-cover_id, -cover_start_month, -cover_stop_month)
}
