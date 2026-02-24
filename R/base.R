init.container <- function(name){
    dir.create(paste0("./containers/",name),
               recursive = TRUE,
               showWarnings = FALSE
               )
    dir.create(paste0("./containers/",name,"/data-output/"),
               recursive = TRUE,
               showWarnings = FALSE
               )
}
