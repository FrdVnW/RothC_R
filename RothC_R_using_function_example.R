source("RothC_R_function.R")

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


