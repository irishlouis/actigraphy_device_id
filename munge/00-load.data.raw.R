# load full raw data, filter to epoch of interest and rewrite to disk

# define epoch period definiton
epoch.duration <- 10

files <- list.files()[str_detect("processed", list.files())]
# set for milliseconds
options(digits.secs=5)
for (i in files){
  # read
  dt <- fread(i, stringsAsFactors = FALSE)
  # format date
  dt[,':='(
    datetime = dmy_hms(datetime)
  ),]
  # define epoch_id
  dt[,':='(
    epoch_id = floor_date(datetime, "minute"),
    second = epoch.duration * floor(second(datetime)/epoch.duration)
  ),][,':='(
    epoch_id = epoch_id + second,
    second = NULL
  ),]
  # set key
  setkey(dt, device_id, epoch_id)
  # filter to identified epochs from epoch.filtered
  dt <- dt[.(epoch.filtered[device_id == dt$device_id[1],.(device_id, epoch_id)])][!is.na(datetime)]
  # write filtered file back to disk - overwrite full data
  fwrite(dt, i)
} 