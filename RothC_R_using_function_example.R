
library(readxl)
library(dplyr)
library(lubridate)



source("./RothC_R_function.R")
source("/home/fred/Code/R/RothC_R/RothC_R_function.R")


source("./xlsx_to_dat_RothC_R.R")

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





