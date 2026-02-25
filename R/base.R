init.container <- function(name){
    dir.create(paste0("./containers/",name),
               recursive = TRUE,
               showWarnings = FALSE
               )

    paths <- file.path("./containers", name,
                       c("data-output",
                         "data-raw",
                         "fig"))

    lapply(paths, dir.create,
           recursive = TRUE,
           showWarnings = FALSE)
    return("ok")
}
