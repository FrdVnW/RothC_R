rm(list=ls())

library(readxl)
library(dplyr)
library(lubridate)

source("./RothC_R_function.R")

source("./R/base.R")
source("./R/expand_rotation.R")
source("./R/expand_cover_crop.R")
source("./R/expand_eom_application.R")
source("./R/expand_irrigation.R")

source("./R/xlsx_to_dat_RothC_R.R")

## RothC_model(filename = "RothC_input.dat")

## year_results <- read.csv("year_results.csv")

## month_results <- read.csv("month_results.csv")

f.rothc.xlsx.2.dat(
    "./data-raw/RothC_input.xlsx",
    "./data-raw/RothC_input.dat",
    vers = 0
)

init.container("my.run.test0")

res <- RothC_model(
    filename = "./data-raw/RothC_input.dat",
    container = "my.run.test0",
    op.print = FALSE
    )

res$year

res$month



## ---------------
## Belgian C input
## ---------------

crop.cin <- readxl::read_xlsx("~/Code/Projects/momatorsow/data-raw/crop_residues.xlsx")
eom.cin <- readxl::read_xlsx("~/Code/Projects/momatorsow/data-raw/eom.xlsx")


View((crop.cin[c("ID","Culture")]))

crop.id.SEQ <- c(15,8,7)




## View(crop.cin)


##################
##
## Scratch zone ##
##
##################

## example data
## ------------


crop.id.SEQ <- c(15,8,7)
sowing.date.SEQ <- c("01-04","01-11","01-10")
harvest.date.SEQ <- c("01-11","01-08","01-07")
covercrop.id.SEQ = c(NA,1,1)
covercrop.sowing.date.SEQ = c(NA,"01-08","01-08")
covercrop.harvest.date.SEQ = c(NA,"01-10","01-02")
irrig.q.SEQ = c(30,0,0)
irrig.date.start.SEQ = c("01-06",NA,NA)
irrig.date.stop.SEQ = c("31-08",NA,NA)
eom.id.SEQ = c(100,NA,NA)
eom.date.SEQ = c("15-01",NA,NA)
eom.q.SEQ = c(150,NA,NA)
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

rothc.calendar <- expand_rotation_calendar(df.crop)

rothc.calendar2 <- expand_cover_crop(
    rotation_calendar = rothc.calendar,
    df = df.crop,
    "covercrop.id",
    "covercrop.sowing.date",
    "covercrop.harvest.date"
)

rothc.calendar3 <- expand_irrigation_calendar(
  rotation_calendar = rothc.calendar2,
  df = df.crop,
  irrig_q_var = "irrig.q",
  irrig_start_var = "irrig.date.start",
  irrig_stop_var = "irrig.date.stop"
)
## View(rothc.calendar3)

rotation_calendar <- rothc.calendar3
df = df.crop
eom_id_var = "eom.id"
eom_date_var = "eom.date"
eom_q_var = "eom.q"









rothc.calendar4 <- expand_eom_calendar_simple(
    rotation_calendar = rothc.calendar,
    df = df.crop.eom,
    eom_id_var = "eom.id",
    eom_date_var = "eom.date",
    eom_q_var = "eom.q"
)

View(rothc.calendar4)


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
