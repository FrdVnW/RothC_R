library(readxl)

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





