# filter raw data to identified epochs

dt <- dt[epoch[,.(device_id, epoch_id, steps),]][!is.na(vec.mag)]

# bin step counts
dt[, steps.bin := bin_steps(steps, by=2,  upper = 20), ]

# summarise filtered raw data
summary.dt <- summarise.data(dt)

# epochs per device
summary.dt[,.N, device_id]

# add a row count for each device_id
summary.dt[,n:=1,][,n:=cumsum(n) ,device_id ]