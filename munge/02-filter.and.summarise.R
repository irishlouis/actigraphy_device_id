# filter raw data to identified epochs

dt <- rbindlist(list(
        TAS1E31150000..2016.04.29..processed, TAS1E31150000..2016.05.03..processed, TAS1E31150005..2016.04.29..processed, 
        TAS1E31150005..2016.05.04..processed, TAS1E35150309..2016.04.29..processed, TAS1E35150309..2016.05.03..processed,
        TAS1E31150026..2016.04.29..processed, TAS1E31150026..2016.05.03..processed, TAS1E31150028..2016.04.29..processed,
        TAS1E31150028..2016.05.03..processed, TAS1E31150030..2016.05.04..processed, TAS1E31150059..2016.04.29..processed,
        TAS1E31150059..2016.05.03..processed, TAS1E35150241..2016.04.29..processed, TAS1E35150241..2016.05.10..processed,
        TAS1E35150250..2016.05.03..processed, TAS1E35150289..2016.05.03..processed,  
        TAS1E35150309..2016.05.03..processed))

rm(TAS1E31150000..2016.04.29..processed, TAS1E31150000..2016.05.03..processed, TAS1E31150005..2016.04.29..processed, 
   TAS1E31150005..2016.05.04..processed, TAS1E35150309..2016.04.29..processed, TAS1E35150309..2016.05.03..processed,
   TAS1E31150026..2016.04.29..processed, TAS1E31150026..2016.05.03..processed, TAS1E31150028..2016.04.29..processed,
   TAS1E31150028..2016.05.03..processed, TAS1E31150030..2016.05.04..processed, TAS1E31150059..2016.04.29..processed,
   TAS1E31150059..2016.05.03..processed, TAS1E35150241..2016.04.29..processed, TAS1E35150241..2016.05.10..processed,
   TAS1E35150250..2016.05.03..processed, TAS1E35150289..2016.05.03..processed, TAS1E35150309..2016.05.03..processed)

setkey(dt, device_id, epoch_id)
dt[, .N, device_id]
epoch.filtered[, .N, device_id]
dt1 <- dt[epoch.filtered[,.(device_id, epoch_id, steps),]][!is.na(vec.mag)]
dt1[, .N, device_id]

dt[device_id == "TAS1E35150289"][1:3]
dt[device_id == "TAS1E31150030"][1:3]

head(dt)

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

# copy summary.dt back to dropbox for syncing
system('cp ~/actigraphy_device_id/cache/summary.dt.RData ~/Dropbox/UCD_MSc_uncompressed/')
system('cp ~/actigraphy_device_id/cache/epoch.filtered.RData ~/Dropbox/UCD_MSc_uncompressed/')
