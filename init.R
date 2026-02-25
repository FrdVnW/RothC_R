rm(list=ls())

library(dplyr)
library(ggplot2)
library(readxl)
library(lubridate)


## Main Roth-C functions
source("./RothC_R_script.R")

## ==========
## Run Roth-C
## ==========

## Case 1a : you have your input.dat file
## --------------------------------------

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
source("./R/base.R")

my.container <- "1b_from_dat_file"
init.container(my.container) ## at least once

## a - run from a dat file in the container
res <- ROTH_C("./containers/1b_from_dat_file/data-raw/.")

EXPORT_ROTHC_RES(
    res,
    container = my.container
)

## Case 2 : build the dat file and run a simulation
source("./R/plotting_RothC_res.R")
source("./R/xlsx_to_dat_RothC_R.R")

## 2a : the dat file is in a 2-sheets spreadsheet file
## ---------------------------------------------------
init.container("my.run.test0")

f.rothc.xlsx.2.dat(
    "./containers/my.run.test0/data-raw/RothC_input.xlsx",
    "./containers/my.run.test0/data-raw/RothC_input.dat",
    vers = 0
)

res <- ROTH_C(
    filename = "./containers/my.run.test0/data-raw/RothC_input.dat"
    )

EXPORT_ROTHC_RES(
    res,
    "my.run.test0"
)

## Plotting direct
PLOT_ROTHC_RES(res = res)
PLOT_ROTHC_RES(res = res, step = "months")

## Save in objects and files
g.yearly.res <- PLOT_ROTHC_RES(res = res)
## g.yearly.res ## show
ggsave("./containers/my.run.test0/fig/yearly_res.png")

g.monthly.res <- PLOT_ROTHC_RES(res = res, step = "months")
## g.monthly.res ## show
ggsave("./containers/my.run.test0/fig/monthly_res.png")



## 2b Build the dat file from scratch (expand from a crop rotation)
## ----------------------------------------------------------------

init.container("my.run.test1")

source("./R/expand_rotation.R")
source("./R/expand_cover_crop.R")
source("./R/expand_eom_application.R")
source("./R/expand_irrigation.R")


## ---------------
## Belgian C input
## ---------------

crop.cin <- readxl::read_xlsx("~/Code/Projects/momatorsow/data-raw/crop_residues.xlsx")
eom.cin <- readxl::read_xlsx("~/Code/Projects/momatorsow/data-raw/eom.xlsx")

## == a - Crop management input data ==================================

## example data 3Y rotation Sugar beet - Winter Wheat - Winter Barley
crop.id.SEQ <- c(15,8,7)
sowing.date.SEQ <- c("03-04"      , "06-10"       , "10-09")
harvest.date.SEQ <- c("05-10"      , "06-08"       , "02-07")
covercrop.id.SEQ = c(NA,1,1)
covercrop.sowing.date.SEQ = c(NA,"01-08","01-08")
covercrop.harvest.date.SEQ = c(NA,"01-10","01-02")
irrig.q.SEQ = c(30,0,0)
irrig.date.start.SEQ = c("01-06",NA,NA)
irrig.date.stop.SEQ = c("31-08",NA,NA)
eom.id.SEQ = c(6,NA,NA)
eom.date.SEQ = c("15-09",NA,NA)
eom.q.SEQ = c(30,NA,NA)
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

## == b - Initiate calender with crop rotation =========================
rothc.calendar <- expand_rotation_calendar(df.crop)

## == c - Add cover crops information ==================================
rothc.calendar2 <- expand_cover_crop(
    rotation_calendar = rothc.calendar,
    df = df.crop,
    "covercrop.id",
    "covercrop.sowing.date",
    "covercrop.harvest.date"
)

## == c - Add irrigation application ===================================
rothc.calendar3 <- expand_irrigation_calendar(
  rotation_calendar = rothc.calendar2,
  df = df.crop,
  irrig_q_var = "irrig.q",
  irrig_start_var = "irrig.date.start",
  irrig_stop_var = "irrig.date.stop"
)
## View(rothc.calendar3)

## == d - Add external organic matter application =======================
rothc.calendar4 <- expand_eom_calendar(
    rotation_calendar = rothc.calendar3,
    df = df.crop,
    eom_id_var = "eom.id",
    eom_date_var = "eom.date",
    eom_q_var = "eom.q"
)
## View(rothc.calendar4)


## == e - export intermediate file
write.csv(
    rothc.calendar4,
    file = "./containers/my.run.test1/data-output/raw_calendar.csv"
)

## == f - compute C input from crops, cover crops and eom ================

h(rothc.calendar4,15)

h(crop.cin)




rothc.calendar5 <- rothc.calendar4 |>
    dplyr::left_join(crop.cin, by = join_by(crop.id == id))
    
View(rothc.calendar5)






## == g - get meteo data =================================================
source("~/Code/R/fredtoolbox/10_meteo.R")

df.agromet <- read.agromet.datafile(
    "~/Code/Projects/momatorsow/data-raw/Sombreffe_tsa_plu_etp",
    ## "~/Code/Projects/momatorsow/data-raw/27_010985_171020_jour.txt"
    type = "rds"
)

df.meteo <- compute.agromet.stat(df.agromet)








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
