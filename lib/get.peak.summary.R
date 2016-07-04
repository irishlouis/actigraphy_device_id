#' get.peak.summary
#'
#' @param v 
#' @param k 
#' @param freq 
#'
#' @return
#' @export
#'
#' @examples
get.peak.summary <- function(v, k, freq) {
  require(zoo)
  v.smooth <- rollapply(v, k, mean)
  switch.dir <- sapply(seq_along(v.smooth), 
                       function(x) ifelse(x<length(v.smooth),
                                          v.smooth[x]<v.smooth[x+1],
                                          F))
  peaks.per.sec <- sum(sapply(seq_along(v.smooth), 
                              function(x) ifelse(x<length(v.smooth), 
                                                 switch.dir[x] != switch.dir[x+1], 
                                                 F))) / ((length(v.smooth)+k-1)/freq)
  if(peaks.per.sec == 0) return(list(peaks.per.sec = 0, 
                                     avg.period = 0, 
                                     sd.period = 0,
                                     avg.amp = 0,
                                     sd.amp = 0))
  
  period <- rollapply(which(sapply(seq_along(v.smooth), 
                                   function(x) ifelse(x<length(v.smooth), 
                                                      switch.dir[x] != switch.dir[x+1], 
                                                      F)) == T),
                      2, function(x) (x[2] - x[1])/freq)
  
  ampl <- rollapply(which(sapply(seq_along(v.smooth), 
                                 function(x) ifelse(x<length(v.smooth), 
                                                    switch.dir[x] != switch.dir[x+1], 
                                                    F)) == T),
                    2, function(x) abs(v.smooth[x[1]] - v.smooth[x[2]]))
  
  avg.period <- mean(period)
  sd.period <- sd(period)
  avg.amp <- mean(ampl)
  sd.amp <- sd(ampl)
  
  return(list(peaks.per.sec = peaks.per.sec, 
              avg.period = avg.period, 
              sd.period = sd.period,
              avg.amp = avg.amp,
              sd.amp = sd.amp))
}