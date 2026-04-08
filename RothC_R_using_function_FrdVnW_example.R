source("RothC_R_functions_frdvnw.R")

res <- ROTH_C(filename = "RothC_input.dat")

## year_results
res$output_years

## month_results
res$output_months

## export in csv files

EXPORT_ROTHC_RES(res,output.path = "./")

