# partition by day

summary.dt[, epoch_id := as.POSIXct(epoch_id, origin = "1970-01-01") ][
  ,date := as.character(floor_date(epoch_id, "day")),]

summary.dt[,date,] %>% unique

summary.dt[,.N, .(date, device_id)][order(date)]

# first six days for training
training2 <- summary.dt[!date %in% c("2016-05-03", "2016-05-02", "2016-04-26", "2016-04-25")]
training2.epoch <- training2[,.(device_id, epoch_id)]
training2[,':='(epoch_id = NULL, date = NULL, n = NULL, steps =NULL)]

## don't want to overweight for any device_id 
min.rows <- training2[, .N, device_id][,N] %>% min
## create flag
training2.subset <- data.table()
set.seed(12323)

for (i in training2[,unique(device_id)]){
  training2.subset <- rbindlist(list(training2.subset, training2[device_id == i][sample(.N, min.rows)]))
}
## check all equal
training2.subset[, .N, device_id]


testing2 <- summary.dt[date == "2016-05-02"]
testing2.epoch <- testing2[,.(device_id, epoch_id)]
testing2[,':='(epoch_id = NULL, date = NULL, n = NULL, steps =NULL)]

# balance
testing2[, .N, device_id]

cache("training2")
cache("testing2")
