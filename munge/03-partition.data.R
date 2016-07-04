# partition data in testing / training
## need to take care of unequal distribution of no. of rows per device

# get smallest no. of rows
rows <- summary.dt[,.N,device_id][,N,] %>% min

set.seed(789465)
s <- sample(1:rows, round(0.6*rows))
setkey(summary.dt, n)

summary.dt[n %in% s][, .N, device_id]
training <- summary.dt[n %in% s][,':='(n=NULL,
                                       steps=NULL,
                                       epoch_id=NULL),]
testing <- summary.dt[!n %in% s][,':='(n=NULL,
                                       steps=NULL,
                                       epoch_id=NULL),]

# write to file 
fwrite(training, "training.csv")
fwrite(testing, "testing.csv")