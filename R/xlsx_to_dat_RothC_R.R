f.rothc.xlsx.2.dat <- function(
                                input.xlsx.file = NULL,
                                output.dat.file = "./data-output/RothC_input.dat",
                                vers = 0,
                               n.year.max = 20,
                               ids = "all"
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
        
        df.crop <- readxl::read_xlsx(
                               input.xlsx.file,
                               "Rotation"
                           ) %>%
            dplyr::select(-c(3,4)) %>%
            dplyr::rename(var="...2",
                          ID = names(.)[1]) %>%
            dplyr::filter(!is.na(ID))

        if (ids[1] != "all"){
            df.crop <- df.crop %>%
                dplyr::filter(ID %in% ids)
        }
        
        l.my.rotation <- list()

        for (my.id in df.crop$ID) {
            df.crop.id <- df.crop %>%
                dplyr::filter(ID == my.id)

            df.crop.id <- df.crop.id[
                unlist(lapply(df.crop.id,function(x) {!all(is.na(x))}))
            ]

            my.crop.seq.id <- names(
                unlist(
                    lapply(df.crop.id,
                           function(x) {!all(is.na(x))}))
            )[-c(1:2)]


        write.csv(
            df.my.rotation,
            file = paste0("./containers/",
                          socmo.run,"/input_data/crop.csv"
                          ),
            quote=FALSE,
            row.names = FALSE,
            na = ""
        )
        
        cat("Input file  \n",
            paste0("./containers/",
                   socmo.run,"/input_data/crop.csv  \n"
                   ),
            "written  \n\n"
            )
        
    }
    
    f.rothc.dataframe.2.dat(
        fixed.values,
        simul.values,
        output.dat.file
    )
    
    }
    


f.rothc.dataframe.2.dat <- function(
                                    df.fixed.values,
                                    df.simul.values,
                                    output.dat.file = "./data-output/RothC_input.dat"
                                    ){    
    cat(
        "## example input data to run RothC for a number of years\n",
        "## clay and depth are only needed once, other data are monthly jan-dec\n",
        "(%)\t(cm)\t(t C/ha)\t(-) \n",
        file = output.dat.file,
        sep=""
    )
    suppressWarnings(
        write.table(
            df.fixed.values,
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
            df.simul.values,
            file = output.dat.file,
            append = TRUE, quote = F, sep = "\t", row.names=F, col.names=T
        )
    )
}
