#' bin.steps
#'
#' @param x 
#' @param lower 
#' @param upper 
#' @param by 
#' @param sep 
#' @param above.char 
#'
#' @return
#' @export
#'
#' @examples
bin.steps <- function(x, lower = 0, upper = 20, by = 2.5,
                      sep = "-", above.char = "+") {
  
  labs <- c(paste(seq(lower, upper - by, by = by),
                  seq(lower + by - 1, upper - 1, by = by),
                  sep = sep),
            paste(upper, above.char, sep = ""))
  
  cut(floor(x), breaks = c(seq(lower, upper, by = by), Inf),
      right = FALSE, labels = labs)
}