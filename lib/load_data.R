#' load.data
#'
#' @param datafolder, name of subfolder within /data to read from 
#'
#' @return data
#' @export
#'
#' @examples
load.data <- function(datafolder = NULL){
  ifelse (!is.null(datafolder),
    csv.files <- paste0("data/", datafolder,"/", list.files(paste0("data/", datafolder))),
    csv.files <- paste0("data/", list.files(paste0("data"))))
  
  # get the summary files
  epoch <- lapply(csv.files[str_detect(csv.files, "5sec") * str_detect(csv.files, "csv")], fread, header = TRUE, stringsAsFactors = FALSE) %>%
    rbindlist
  epoch <- epoch[,':='(
    timestamp = ymd_hms(timestamp))][,
                                     .(timestamp, serialnumber, steps),][,
                                                                         steps_bin :=bin_steps(steps, 0, 20, 3),]
  setkey(epoch, serialnumber, timestamp)
  
  # empty list to hold raw data
  data <- list()
  # allow for milliseconds
  options(digits.secs=3)
  
  for (i in csv.files[str_detect(csv.files, "RAW.csv")]){
    dt <- fread(i, header = T, stringsAsFactors = F)
    
    device_id <- strsplit(strsplit(i, " ")[[1]][1], "/")[[1]][ifelse(is.null(datafolder), 
                                                                     0, 
                                                                     length(strsplit(datafolder, "/")))+2]
    dt[,':='(
      device_id = device_id,
      datetime = dmy_hms(Timestamp),
      Timestamp = NULL,
      vec.mag = sqrt(`Accelerometer X`^2 + `Accelerometer Y`^2 + `Accelerometer Z`^2),
      `Accelerometer X` = NULL,
      `Accelerometer Y` = NULL,
      `Accelerometer Z` = NULL
    ),][,':='(
      time_minute = floor_date(datetime, "minute"),
      second = 5 * floor(second(datetime)/5)
    ),][,':='(
      epoch_id = time_minute + seconds(second),
      time_minute = NULL,
      second = NULL
    ),]
    
    data[[which(csv.files[str_detect(csv.files, "RAW.csv")] == i)]] <- dt
  }
  rm(dt)
  # collapse to single file
  data <- rbindlist(data)
  setkey(data, device_id, epoch_id)
  
  # join epoch steps to raw
  data <- data[epoch] 
  
  return(data)
}