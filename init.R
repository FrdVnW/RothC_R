rm(list=ls())

library(dplyr)
library(ggplot2)
library(readxl)
library(lubridate)

## Main Roth-C functions
source("./RothC_R_script.R")

## Personal functions
source("./R/base.R")

source("./R/plotting_RothC_res.R")
source("./R/xlsx_to_dat_RothC_R.R")

source("./R/expand_rotation.R")
source("./R/expand_cover_crop.R")
source("./R/expand_eom_application.R")
source("./R/expand_irrigation.R")


## ==========
## Run Roth-C
## ==========

## Case 1a : you have your input.dat file
## --------------------------------------
if (FALSE){
    ## if needed
    ## setwd("B:/Github_RothC_development/RothC_R/")

    ## a - run
    res <- ROTH_C("./RothC_input.dat")

    ## b - print results

    res[["output_years"]]

    res[["output_months"]]

    ## c - export
    EXPORT_ROTHC_RES(
        res
    )

    ## Case 1b : same but with container

    my.container <- "1b_from_dat_file"
    init.container(my.container) ## at least once

    ## a - run from a dat file in the container
    res <- ROTH_C("./containers/1b_from_dat_file/data-raw/RothC_input.dat")

    EXPORT_ROTHC_RES(
        res,
        container = my.container
    )
}

## Case 2 : build the dat file and run a simulation

## 2a : the dat file is in a 2-sheets spreadsheet file
## ---------------------------------------------------
if (FALSE) {
    init.container("2a_full_calendar_xlsx")

    f.rothc.xlsx.2.dat(
        "./containers/2a_full_calendar_xlsx/data-raw/RothC_input.xlsx",
        "./containers/2a_full_calendar_xlsx/data-raw/RothC_input.dat",
        vers = 0
    )

    res <- ROTH_C(
        filename = "./containers/2a_full_calendar_xlsx/data-raw/RothC_input.dat"
    )

    EXPORT_ROTHC_RES(
        res,
        "2a_full_calendar_xlsx"
    )

    ## Plotting direct
    PLOT_ROTHC_RES(res = res)
    PLOT_ROTHC_RES(res = res, step = "months")

    ## Save in objects and files
    g.yearly.res <- PLOT_ROTHC_RES(res = res)
    ## g.yearly.res ## show
    ggsave("./containers/2a_full_calendar_xlsx/fig/yearly_res.png")

    g.monthly.res <- PLOT_ROTHC_RES(res = res, step = "months")
    ## g.monthly.res ## show
    ggsave("./containers/2a_full_calendar_xlsx/fig/monthly_res.png")
}
## 2b Build the dat file from scratch (expand from a crop rotation)
## ----------------------------------------------------------------

{
    init.container("2b_from_rotation_code")

    ## ---------------
    ## Belgian C input
    ## ---------------

    crop.cin <- readxl::read_xlsx("~/Code/Projects/momatorsow/data-raw/crop_residues.xlsx")
    eom.cin <- readxl::read_xlsx("~/Code/Projects/momatorsow/data-raw/eom.xlsx")

    ## == a - Crop management input data ==================================

    ## example data 3Y rotation Sugar beet - Winter Wheat - Winter Barley
    crop.id.SEQ <- c(      15,               8,              7     )
    sowing.date.SEQ <- c( "03-04",          "15-10",        "01-10")
    harvest.date.SEQ <- c("30-10",          "01-08",        "01-07")
    covercrop.id.SEQ = c(  NA,               NA,             50    )
    covercrop.sowing.date.SEQ = c(NA,        NA,            "15-08")
    covercrop.destruction.date.SEQ = c(NA,   NA,            "01-03")
    irrig.q.SEQ = c(         30,             0,             0      )
    irrig.date.start.SEQ = c("01-06",        NA,            NA     )
    irrig.date.stop.SEQ = c("31-08",         NA,            NA     )
    eom.id.SEQ = c(         6,               NA,            NA     )
    eom.date.SEQ = c(       "15-07",         NA,            NA     )
    eom.q.SEQ = c(          30,              NA,            NA     )

    df.crop <- data.frame(
        num.crop = 1:length(crop.id.SEQ),
        crop.id = crop.id.SEQ,
        sowing.date = sowing.date.SEQ,
        harvest.date = harvest.date.SEQ,
        covercrop.id = covercrop.id.SEQ, 
        covercrop.sowing.date = covercrop.sowing.date.SEQ,
        covercrop.destruction.date = covercrop.destruction.date.SEQ,
        irrig.q = irrig.q.SEQ,
        irrig.date.start = irrig.date.start.SEQ,
        irrig.date.stop = irrig.date.stop.SEQ,
        eom.id = eom.id.SEQ,
        eom.date = eom.date.SEQ,
        eom.q = eom.q.SEQ
    )

    ## == b - Initiate calender with crop rotation =========================
    rothc.calendar <- expand_rotation_calendar(
        df.crop,
        start_year = 2020,
        n_years = 30
    )

    ## == c - Add cover crops information ==================================
    rothc.calendar2 <- expand_cover_crop(
        rotation_calendar = rothc.calendar,
        df = df.crop
    )

    ## == c - Add irrigation application ===================================
    rothc.calendar3 <- expand_irrigation_calendar(
        rotation_calendar = rothc.calendar2,
        df = df.crop
    )
    ## View(rothc.calendar3)

    ## == d - Add external organic matter application =======================
    rothc.calendar4 <- expand_eom_calendar(
        rotation_calendar = rothc.calendar3,
        df = df.crop
    )
    ## View(rothc.calendar4)


    ## == e - export intermediate file
    write.csv(
        rothc.calendar4,
        file = "./containers/2b_from_rotation_code/data-output/raw_calendar.csv"
    )

    ## == f - compute C input from crops, cover crops and eom ================


    ## --> C input from crops --

    rothc.calendar5 <- rothc.calendar4 |>
        dplyr::left_join(select(crop.cin,
                                `id`,
                                `Culture`,
                                `Apport C (t CO/ha)`,
                                `Coefficient d'humification (hc)`,
                                `Apport C humifié (t C/ha)`,
                                ), by = join_by(crop.id == id)) |>
        dplyr::mutate(
                   C_inp_crop = case_when(
                       harvest ~ `Apport C (t CO/ha)`,
                       TRUE ~ 0
                   ),
                   C_hum_crop = case_when(
                       harvest ~ `Apport C humifié (t C/ha)`,
                       TRUE ~ 0
                   )
               ) %>%
        dplyr::select(-`Apport C (t CO/ha)`,
                      -`Apport C humifié (t C/ha)`
                      )

    ## --> C input from cover crops --
    rothc.calendar6 <- rothc.calendar5 |>
        dplyr::left_join(
                   crop.cin |>
                   filter(`Catégorie` == "Engrais verts") |>
                   select(
                       `id`,
                       `Culture`,
                       `Apport C (t CO/ha)`,
                       `Coefficient d'humification (hc)`,
                       `Apport C humifié (t C/ha)`,
                       ),
                   by = join_by(cover.id == id)
               ) |>
        dplyr::mutate(
                   C_inp_covercrop = case_when(
                       covercrop.harvest ~ `Apport C (t CO/ha)`,
                       TRUE ~ 0
                   ),
                   C_hum_covercrop = case_when(
                       covercrop.harvest ~ `Apport C humifié (t C/ha)`,
                       TRUE ~ 0
                   )
               ) %>%
        dplyr::select(-`Apport C (t CO/ha)`,
                      -`Apport C humifié (t C/ha)`
                      )

    h(rothc.calendar6,n=30)

    ## --> C input from both crops and cover crops & plant cover
    rothc.calendar7 <- rothc.calendar6 |>
        dplyr::mutate(
                   C_inp = C_inp_covercrop + C_inp_crop,
                   C_hum_inp = C_hum_covercrop + C_hum_crop,
                   PC = case_when(
                       `active` + `cover.active` > 0 ~ 1,
                       TRUE ~ 0
                   )
               )


    ## --> C input from eom
    rothc.calendar8 <- rothc.calendar7 |>
        dplyr::left_join(
                   select(
                       eom.cin,
                       `id`,
                       `Catégorie`,
                       `Matière organique exogène`,
                       `Apport de carbone organique (t CO / 10t MF)`,
                       `Coefficient d'humification (hc)`,
                       `Apport C humifié (t C/ 10t MF)`,
                       ),
                   by = join_by(eom_app_id == id)
               ) %>%
        dplyr::mutate(
                   FYM = case_when(
                       !is.na(`Apport de carbone organique (t CO / 10t MF)`) ~
                           (0.1 * eom_app_q * `Apport de carbone organique (t CO / 10t MF)`),
                       TRUE ~ 0
                   )
               )

    ## == g - get meteo data =================================================
    source("~/Code/R/fredtoolbox/10_meteo.R")

    df.agromet <- read.agromet.datafile(
        "~/Code/Projects/momatorsow/data-raw/Sombreffe_tsa_plu_etp",
        ## "~/Code/Projects/momatorsow/data-raw/27_010985_171020_jour.txt"
        type = "rds"
    )

    df.meteo <- compute.agromet.stat(df.agromet) %>%
        dplyr::rename(Tmp = temp,
                      Evap = ep,
                      Rain.meteo = precip)

    ## --> join meteo data and add irrigation to rain
    rothc.calendar9 <- rothc.calendar8 |>
        dplyr::left_join(
                   df.meteo,
                   by = join_by(`month`)
               ) |>
        dplyr::mutate(
                   Rain = Rain.meteo + irrigation
               )

    ## == h - radio carbon & DPM_RPM ration ==========================

    ## --> join with radio carbon data (???)
    df.modern <- read.csv("./data-raw/radio_carbon.csv")

    rothc.calendar10 <- rothc.calendar9 |>
        dplyr::left_join(df.modern,
                         by = join_by(year)
                         ) |>
        dplyr::mutate(
                   modern = case_when(
                       is.na(modern) ~ 100,
                       TRUE ~ modern
                   ),
                   DPM_RPM = 1.44
               )

    h(rothc.calendar10, 50)

    ## == i - Environmental input data ===============================

    clay <- read.csv2("/home/fred/Data/BdD/Texture_LT.csv") %>%
        dplyr::select(Argile) %>%
        dplyr::pull() %>%
        mean() %>%
        round(1)
    depth <- 25 
    iom <- 3.0041
    nsteps <- dim(rothc.calendar10)[1]

    ## == j - 2 main data frames and write the dat file ==============

    df.fixed.values <- data.frame(
        clay = clay,
        depth = depth,
        iom = iom,
        nsteps = nsteps
    )

    df.simul.values <- rothc.calendar10 |>
        dplyr::select(
                   year,
                   month,
                   modern,
                   Tmp,
                   Rain,
                   Evap,
                   C_inp,
                   FYM,
                   PC,
                   DPM_RPM
               )

    f.rothc.dataframe.2.dat(
        df.fixed.values,
        df.simul.values,
        output.dat.file = "./containers/2b_from_rotation_code/data-raw/RothC_input.dat"
    )


    write.csv(df.simul.values,
              "./containers/2b_from_rotation_code/data-output/simul_values.csv"
              )

    write.csv(rothc.calendar10,
              "./containers/2b_from_rotation_code/data-output/simul_values_full_input.csv"
              )




    ## == k - run and export results

    res <- ROTH_C(
        filename = "./containers/2b_from_rotation_code/data-raw/RothC_input.dat"
    )

    EXPORT_ROTHC_RES(
        res,
        "2b_from_rotation_code"
    )

    ## Plotting direct
    PLOT_ROTHC_RES(res = res)
    PLOT_ROTHC_RES(res = res, step = "months")

    ## Save in objects and files
    g.yearly.res <- PLOT_ROTHC_RES(res = res)
    ## g.yearly.res ## show
    ggsave("./containers/2b_from_rotation_code/fig/yearly_res.png")

    g.monthly.res <- PLOT_ROTHC_RES(res = res, step = "months")
    ## g.monthly.res ## show
    ggsave("./containers/2b_from_rotation_code/fig/monthly_res.png")


}
## ENDOF 2B




## == write the dat file


##################
##
## Scratch zone ##
##
##################

## View(crop.cin)
## View((crop.cin[c("ID","Culture")]))
## crop.id.SEQ <- c(15,8,7)













## ##########
## Trash zone
## ##########

##         Till = Till.SEQ,
##         SOWING_DATE = SOWING_DATE.SEQ,
##         HARVEST_DATE = HARVEST_DATE.SEQ,
##         CI_CC = CI_CC.SEQ,
##         CC_NAME = CC_NAME.SEQ,
##         CC_SOWING_DATE = CC_SOWING_DATE.SEQ,
##         CC_HARVEST_DATE = CC_HARVEST_DATE.SEQ,
##         CC_CINPUT = CC_CINPUT.SEQ,
##         Irrig = Irrig.SEQ,
##         Yield = Yield.SEQ,
##         EOM_C = EOM_C.SEQ,
##         EOM_TYPE = EOM_TYPE.SEQ,
##         BIOCHAR_C = BIOCHAR_C.SEQ,
##         prop_residue_removal = prop_residue_removal.SEQ,
##         C_WOODY_ELEMENTS = C_WOODY_ELEMENTS.SEQ
##     )

##     df.crop.2 <- df.crop %>%
##         slice(rep(1:n(), first(ceiling(n.year.max/length(CROP.SEQ))))) %>%
##         mutate(NUM = 1:n()) %>%
##         relocate(NUM, .before=1) %>%
##         dplyr::filter(NUM <= n.year.max)
    
##     return(df.crop.2)
## }
