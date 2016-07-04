#' clean.slate
#'
#' @description function to detach all packages, and clear environment
#' @param takes none
#' @return returns nothing
#' @export

clean.slate <- function(){
  pkgs <- names(sessionInfo()$otherPkgs) 
  if(!is.null(pkgs)){
    pkgs <- paste('package:', pkgs, sep = "")
    lapply(pkgs, detach, character.only = TRUE, unload = TRUE, force = TRUE)
  }
  rm(list = ls())
}