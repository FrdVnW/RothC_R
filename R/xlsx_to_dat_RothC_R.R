init.container <- function(name){
    dir.create(paste0("./containers/",name),
               recursive = TRUE
               )
    dir.create(paste0("./containers/",name,"/data-output/"),
               recursive = TRUE
               )
}

f.rothc.xlsx.2.dat <- function(
                                input.xlsx.file = NULL,
                                output.dat.file = "./data-output/RothC_input.dat",
                                vers = 0
                                ){
    if (!is.null(input.xlsx.file)){
        if (vers == 0) {
            fixed.values <- readxl::read_xlsx(
                                        input.xlsx.file,
                                        sheet="Fixed"
                                    )
            simul.values <- readxl::read_xlsx(
                                        input.xlsx.file,
                                        sheet="Simul"
                                    )
        }
    } else {
    }
    
    cat(
        "## example input data to run RothC for a number of years\n",
        "## clay and depth are only needed once, other data are monthly jan-dec\n",
        "(%)\t(cm)\t(t C/ha)\t(-) \n",
        file = output.dat.file,
        sep=""
    )
    suppressWarnings(
        write.table(
            fixed.values,
            file = output.dat.file,
            append = TRUE, quote = F, sep = "\t", row.names=F, col.names=T
        )
    )
    cat(
        "(-)\t(-)\t(%)\t(C)\t(mm)\t(mm)  (t C/ha) (t C/ha)\t(-)\t(-)\n",
        file = output.dat.file,
        sep="",
        append = TRUE
    )
    suppressWarnings(
        write.table(
            simul.values,
            file = output.dat.file,
            append = TRUE, quote = F, sep = "\t", row.names=F, col.names=T
        )
    )
}





small.df.crop <- df.crop[1:4]




expand_rotation_calendar <- function(df, start_year = 2020, n_years = 30) {
  
  df <- df %>%
    mutate(
      sow_month  = as.numeric(substr(sowing.date, 4, 5)),
      harv_month = as.numeric(substr(harvest.date, 4, 5))
    )
  
  # Full calendar
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
  
  # timeline position helper
  month_id <- function(y, m) y * 12 + m
  
  current_year  <- start_year
  current_month <- df$sow_month[1]
  crop_index    <- 1
  end_year      <- start_year + n_years - 1
  
  while (current_year <= end_year) {
    
    sow_m  <- df$sow_month[crop_index]
    harv_m <- df$harv_month[crop_index]
    cropid <- df$crop.id[crop_index]
    
    # --- find first sowing AFTER previous harvest ---
    
    # candidate sowing year
    if (current_month <= sow_m) {
      sow_year <- current_year
    } else {
      sow_year <- current_year + 1
    }
    
    if (sow_year > end_year) break
    
    # determine harvest year
    if (harv_m >= sow_m) {
      harv_year <- sow_year
    } else {
      harv_year <- sow_year + 1
    }
    
    # stop if harvest beyond simulation
    if (harv_year > end_year + 1) break
    
    # activate months
    idx <- which(
      month_id(calendar$year, calendar$month) >= month_id(sow_year, sow_m) &
      month_id(calendar$year, calendar$month) <= month_id(harv_year, harv_m)
    )
    
    calendar$crop.id[idx] <- cropid
    calendar$active[idx]  <- 1
    
    # mark sow & harvest
    calendar$sowing[calendar$year == sow_year &
                    calendar$month == sow_m] <- TRUE
    
    calendar$harvest[calendar$year == harv_year &
                     calendar$month == harv_m] <- TRUE
    
    # move pointer to month AFTER harvest
    if (harv_m == 12) {
      current_year  <- harv_year + 1
      current_month <- 1
    } else {
      current_year  <- harv_year
      current_month <- harv_m + 1
    }
    
    # next crop in rotation
    crop_index <- crop_index + 1
    if (crop_index > nrow(df)) crop_index <- 1
  }
  
  calendar
}



expand_rotation_calendar(small.df.crop)







## f.rothc.make.a.rotation <- function(
##                               crop.id.SEQ, ## ID de la culture (voir liste)
##                               sowing.date.SEQ, ## Date de semis ["DD-MM"]
##                               harvest.date.SEQ, ## Date de récolte ["DD-MM"]
##                               covercrop.id.SEQ = NULL, ## ID des cultures de couverture (voir liste) ("cover crop")
##                               covercrop.sowing.date.SEQ = NULL, ## Date de semis des couvertures ("DD-MM")
##                               covercrop.harvest.date.SEQ = NULL, ## Date de récolte des couvertures ("DD-MM")
##                               irrig.q.SEQ = NULL, ## Irrigation : quantité [mm]
##                               irrig.date.start.SEQ = NULL, ##
##                               irrig.date.stop.SEQ = NULL, ##
##                               eom.id.SEQ = NULL, ## ID de la m.o. exogène (voir liste)
##                               eom.date.SEQ = NULL, ## Date de l'épandage [["DD-MM"]]
##                               eom.q.SEQ = NULL, ## Apport de m.o. exogène [t/ha/yr]
##                               starting.year = 2000,
##                               n.year = 30 ##, Nombre d'années pour la simulation (20 max)
##                               ){

    
crop.id.SEQ <- c(15,8,7)
sowing.date.SEQ <- c("01-04","01-11","01-10")
harvest.date.SEQ <- c("01-11","01-08","01-07")
covercrop.id.SEQ = NA
covercrop.sowing.date.SEQ = NA
covercrop.harvest.date.SEQ = NA
irrig.q.SEQ = NA
irrig.date.start.SEQ = NA
irrig.date.stop.SEQ = NA
eom.id.SEQ = NA
eom.date.SEQ = NA
eom.q.SEQ = NA
starting.year = 2000
n.year = 30

    


    df.crop <- data.frame(
        num.crop = 1:length(crop.id.SEQ),
        crop.id = crop.id.SEQ,
        sowing.date = sowing.date.SEQ,
        harvest.date = harvest.date.SEQ,
        covercrop.id = covercrop.id.SEQ, 
        covercrop.sowing.date = covercrop.sowing.date.SEQ,
        covercrop.harvest.date = covercrop.harvest.date.SEQ,
        irrig.q = irrig.q.SEQ,
        irrig.date.start = irrig.date.start.SEQ,
        irrig.date.stop = irrig.date.stop.SEQ,
        eom.id = eom.id.SEQ,
        eom.date = eom.date.SEQ,
        eom.q = eom.q.SEQ
    )



df.crop[1:4]



        
        crop.iP = CROP.SEQ,
        Till = Till.SEQ,
        SOWING_DATE = SOWING_DATE.SEQ,
        HARVEST_DATE = HARVEST_DATE.SEQ,
        CI_CC = CI_CC.SEQ,
        CC_NAME = CC_NAME.SEQ,
        CC_SOWING_DATE = CC_SOWING_DATE.SEQ,
        CC_HARVEST_DATE = CC_HARVEST_DATE.SEQ,
        CC_CINPUT = CC_CINPUT.SEQ,
        Irrig = Irrig.SEQ,
        Yield = Yield.SEQ,
        EOM_C = EOM_C.SEQ,
        EOM_TYPE = EOM_TYPE.SEQ,
        BIOCHAR_C = BIOCHAR_C.SEQ,
        prop_residue_removal = prop_residue_removal.SEQ,
        C_WOODY_ELEMENTS = C_WOODY_ELEMENTS.SEQ
    )

    df.crop.2 <- df.crop %>%
        slice(rep(1:n(), first(ceiling(n.year.max/length(CROP.SEQ))))) %>%
        mutate(NUM = 1:n()) %>%
        relocate(NUM, .before=1) %>%
        dplyr::filter(NUM <= n.year.max)
    
    return(df.crop.2)
}
