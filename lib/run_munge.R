#' run.munge
#' 
#' @description call munge methods in ./munge to load and preprocess new data
#'
#' @param null
#' @return null
#'
#' @examples
run.munge <- function(){
  wd <- getwd()
  setwd("munge")
  files <- list.files()
  setwd(wd)
  sapply(files, function(x) {
    t <- proc.time()
    source(paste0("munge/",x))
    print(paste(x, "- done in ", (proc.time()-t)[3], "sec"))
  })
}
