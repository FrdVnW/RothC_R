PLOT_ROTHC_RES <- function(
                           res.file = NULL,
                           res = NULL, ## ignored of res.file
                           step = "years", ## month, ignored of res.file
                           my.run = "my run",
                           my.caption = "my caption"
                           ) {
    if (is.null(res.file)) {
        if (is.null(res)) {
            stop("no input!")
        } else {
            ## res way
            if (step %in% c("years")){
                df.res <- res[["output_years"]][-1,]
            }
            if (step %in% c("months")){
                df.res <- res[["output_months"]][-1,]
            }
        }
    } else {
        ## file way
        df.res <- read.csv(res.file)       
    }
        
    df.res <- df.res %>%
        mutate(Date = stringr::str_c(Year, Month,"01",sep="-")) %>%
        mutate(Date = as.Date(Date, format = "%Y-%m-%d"))

    g.res <- df.res %>%
        ggplot(aes(x=Date,
                   y=SOC_t_C_ha
                   )
               )
    g.res <- (
        g.res +
        geom_line(lwd=1) +
        lims(y=c(0,NA)) + 
        theme_bw() +
        labs(title="Roth-C Simulation",
             subtitle = paste0("Run : ", my.run),
             x = "Year [-]",
             y = "Soil Organic Carbon [tC/ha]",
             caption = my.caption
             ) +
        scale_color_viridis_d(name = "Run",
                              option = "H",
                              label = unique(df.res$info),
                              guide = guide_legend()) + 
        theme(
            legend.position="bottom",
            legend.direction="horizontal"
        )
    )
    return(g.res)
}


   
