#' summarise.data
#'
#' @param data 
#'
#' @return
#' @export
#'
#' @examples
summarise.data <- function(data, k = 25, freq = 100){
  # create summary data set 

  # get summary data for vector magnitude
  ## exclude epochs with low step data
  setkey(data, device_id, epoch_id, steps_bin)
  data <- data[complete.cases(data)]
  vector.summary <- data[, 
                         .(avg.vec = mean(vec.mag),
                           sd.vec = sd(vec.mag),
                           steps = steps,
                           steps_bin = steps_bin), 
                         .(device_id, epoch_id) ] %>% setkey(device_id, epoch_id)
  
  # get summary data for peak summary
  peak.summary <- data[, 
                       get.peak.summary(vec.mag, k = k, freq = freq), 
                       .(device_id, epoch_id) ] %>% setkey(device_id, epoch_id)
  
  # join summary data
  summary <- vector.summary[peak.summary]
  
  return(unique(summary))
}