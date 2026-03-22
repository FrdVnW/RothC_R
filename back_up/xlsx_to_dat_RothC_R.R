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

f.rothc.xlsx.2.dat <- function(
                               input.xlsx.file = NULL,
                               output.dat.file = "./data-output/RothC_input.dat",
                               vers = 0,
                               n.year.max = 20,
                               ids = "all"
                               ){
    if (vers == 0) {
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
        
        f.rothc.dataframe.2.dat(
            fixed.values,
            simul.values,
            output.dat.file
        )
        
    }
    if (vers == "1") {
        df.crop <- readxl::read_xlsx(
                               path.to.xlsx,
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

            l.inputs.id <- list()
            for (my.var in df.crop.id$var) {
                l.inputs.id[[my.var]] <- df.crop.id %>%
                    dplyr::filter(var == my.var) %>%
                    dplyr::select(all_of(my.crop.seq.id)) %>%
                    t() %>% as.data.frame() %>%
                    dplyr::select(V1) %>%
                    dplyr::pull()
            }
            
            l.my.rotation[[my.id]] <- do.call(
                f.make.a.rotation,
                c(l.inputs.id,
                  list(n.year.max = n.year.max)) ## 30 années de simulation ici !!!
            ) %>%
                dplyr::mutate(ID = my.id) %>%
                dplyr::relocate(ID,
                                .before=1)
        }
        
        df.my.rotation <- bind_rows(l.my.rotation) %>%
            mutate_all(stringr::str_replace_all, "NULL", "")

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

        df.soil <- readxl::read_xlsx(
                               path.to.xlsx,
                               "Sol"
                           )[-c(2,3)]

        df.soil.2 <- df.soil[
            unlist(lapply(df.soil[-1,],function(x) {!all(is.na(x))}))
        ] %>%
            t() %>%
            as.data.frame() %>%
            tibble::rownames_to_column("ID")
        names(df.soil.2) <- gsub(".SEQ","",df.soil.2[1,])
        df.soil.3 <- df.soil.2[-1,]
        
        if (ids[1] != "all"){
            df.soil.3 <- df.soil.3 %>%
                dplyr::filter(ID %in% ids)
        }
        
        na_rows <- is.na(df.soil.3$OC_THA)

        if (any(na_rows)) {
            df.soil.3$OC_THA[na_rows] <- mapply(
                soc2stock,
                as.numeric(df.soil.3$OC_GKG[na_rows]),
                as.numeric(df.soil.3$BD[na_rows]),
                res = "value"
            )
        }
        
        write.csv(df.soil.3 ,
                  file = paste0(
                      "./containers/",
                      socmo.run,"/input_data/soil.csv"
                  ),
                  quote=FALSE,
                  row.names = FALSE
                  )
        
        cat("Input file \n",
            paste0("./containers/",
                   socmo.run,"/input_data/soil.csv \n"
                   ),
            "written \n\n"
            )

        df.meteo <- readxl::read_xlsx(
                                path.to.xlsx,
                                "Météo",
                                skip = 2
                            )
        df.meteo.2 <- f.meteo.long2wide(select(df.meteo,-YEAR))

        write.csv(df.meteo.2,
                  file = paste0("./containers/",socmo.run,"/input_data/meteo.csv"),
                  quote=FALSE,
                  row.names = FALSE
                  )
        cat("Input file \n",
            paste0("./containers/",
                   socmo.run,"/input_data/meteo.csv \n"
                   ),
            "written \n\n"
            )      
        
        df.info <- readxl::read_xlsx(
                               path.to.xlsx,
                               "InfosSupp"
                           )
        if (ids[1] != "all"){
            df.info <- df.info %>%
                dplyr::filter(ID %in% ids)
        }
        write.csv(df.info,
                  file = paste0("./containers/",socmo.run,"/input_data/info.csv"),
                  quote=FALSE,
                  row.names = FALSE
                  )
        cat("Input file \n",
            paste0("./containers/",
                   socmo.run,"/input_data/info.csv \n"
                   ),
            "written \n\n"
            )
    }
}




