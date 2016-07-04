#' unregister
#'
#' @description function to tidy up after using doparallel
#' @param null
#' @return null
#' @export null
#'
#' @examples
unregister <- function() {
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
}