# filter raw data to identified epochs

dt <- dt[epoch.filtered[,.(device_id, epoch_id, steps),]][!is.na(vec.mag)]

# bin step counts
dt[, steps_bin := bin.steps(steps, by=3,  upper = 20), ]

# summarise filtered raw data
summary.dt <- summarise.data(dt, k = 25, freq = 100)

summary.dt[, steps_bin := as.factor(as.character(steps_bin)),]

# epochs per device
summary.dt[,.N, device_id]

# add a row count for each device_id
summary.dt[,n:=1,][,n:=cumsum(n) ,device_id ]

# cache summary file
cache("summary.dt")