f.rothc.xlsx.2.dat <- function(
                                input.xlsx.file = NULL,
                                output.dat.file = "./data-output/RothC_input.dat",
                                vers = 0,
                               n.year.max = 20
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
    }
    
    f.rothc.dataframe.2.dat(
        fixed.values,
        simul.values,
        output.dat.file
    )
    
}


f.rothc.xlsx.2.dataframe <- function(
                                     input.xlsx.file = NULL,
                                     ids = "all"
                                     ){
    ## Crop
    ## ----
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

    for (my.id in as.character(unique(df.crop$ID))) {

        df.crop.id <- df.crop %>%
            dplyr::filter(ID == as.numeric(my.id)) %>%
            dplyr::select(-ID)

        df.crop.id <- as.data.frame(t(
            df.crop.id[
                unlist(lapply(df.crop.id,function(x) {!all(is.na(x))}))
            ] %>%
            tibble::column_to_rownames("var")
        )) %>%
            dplyr::select(
                       crop.id,
                       crop.sowing.date,
                       crop.harvest.date,
                       covercrop.id,
                       covercrop.sowing.date,
                       covercrop.destruction.date,
                       irrig.q,
                       irrig.date.start,
                       irrig.date.stop,
                       eom.id,
                       eom.date,
                       eom.q,
                       ) %>%
            dplyr::mutate(
                       crop.id = as.numeric(crop.id),
                       covercrop.id = as.numeric(covercrop.id),
                       eom.id = as.numeric(eom.id),
                       eom.q = as.numeric(eom.q),
                       irrig.q = as.numeric(irrig.q)
                       )

        l.my.rotation[[my.id]] <- df.crop.id
    }

    if (length(l.my.rotation) == 1){
        res.crop <- l.my.rotation[[my.id]]
    } else {
        res.crop <- l.my.rotation
    }
    ## Climate
    ## -------
    df.climate <- readxl::read_xlsx(
                              input.xlsx.file,
                              "Météo",
                              skip = 2
                          ) %>%
        dplyr::filter(!is.na(ID))

    if (ids[1] != "all"){
        df.climate <- df.climate %>%
            dplyr::filter(ID %in% ids)
    }
    
    l.my.climate <- list()
    
    for (my.id in as.character(unique(df.climate$ID))) {

        df.climate.id <- df.climate %>%
            dplyr::filter(ID == my.id) %>%
            dplyr::select(-ID)

        df.climate.id <- as.data.frame(
            df.climate.id[
                unlist(lapply(df.climate.id,function(x) {!all(is.na(x))}))
            ]
        ) %>%
            tibble::column_to_rownames("month")
        
        df.climate.id$month <- month(parse_date_time(rownames(df.climate.id), "b"))
        
        l.my.climate[[my.id]] <- df.climate.id
    }   
    
    if (length(l.my.climate) == 1){
        res.climate <- l.my.climate[[my.id]]
    } else {
        res.climate <- l.my.climate
    }
    ## Soil
    ## ----
    df.soil <- readxl::read_xlsx(
                           input.xlsx.file,
                           "Sol"
                       ) %>%
        dplyr::filter(!is.na(ID))

    if (ids[1] != "all"){
        df.soil <- df.soil %>%
            dplyr::filter(ID %in% ids)
    }
    
    l.my.soil <- list()

    for (my.id in as.character(unique(df.soil$ID))) {

        df.soil.id <- df.soil %>%
            dplyr::filter(ID == my.id) %>%
            dplyr::select(-ID)

        df.soil.id <- as.data.frame(
            df.soil.id[
                unlist(lapply(df.soil.id,function(x) {!all(is.na(x))}))
            ]
        ) %>%
            tibble::column_to_rownames("var")

        if (is.na(df.soil.id["soc.tha","value"])){
            df.soil.id["soc.tha","value"] <- soc2stock(
                soc.gkg = as.numeric(df.soil.id["soc.gkg","value"]),
                soil.depth = as.numeric(df.soil.id["soil.depth","value"]),
                soil.bulk.density = as.numeric(df.soil.id["bulk.density","value"]),
                soil.rock.frag.mass = as.numeric(df.soil.id["coarse.fragments","value"])/100,
                res = "value"  
            )
        }
        
        l.my.soil[[my.id]] <- df.soil.id %>%
            dplyr::select(value) %>%
            t()
    }
    
    if (length(l.my.soil) == 1){
        res.soil <- l.my.soil[[my.id]]
    } else {
        res.soil <- l.my.soil
    }
    
    ## Info
    ## ----
    df.info <- readxl::read_xlsx(
                           input.xlsx.file,
                           "Info"
                       ) %>%
        dplyr::filter(!is.na(ID))

    if (ids[1] != "all"){
        df.info <- df.info %>%
            dplyr::filter(ID %in% ids)
    }
    
    l.my.info <- list()

    for (my.id in as.character(unique(df.info$ID))) {

        df.info.id <- df.info %>%
            dplyr::filter(ID == my.id) %>%
            dplyr::select(-ID)

        df.info.id <- as.data.frame(
            df.info.id[
                unlist(lapply(df.info.id,function(x) {!all(is.na(x))}))
            ]
        )

        l.my.info[[my.id]] <- df.info.id
    }
    
    if (length(l.my.info) == 1){
        res.info <- l.my.info[[my.id]]
    } else {
        res.info <- l.my.info
    }

    ## End of the function
    ## -------------------
    
    return(
        list(
            crop = res.crop,
            climate = res.climate,
            soil = res.soil,
            info = res.info
        )
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
