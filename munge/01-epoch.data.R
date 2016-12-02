# some change to comment to check git branch

# process epoch data to find epochs of interest
## only want epochs > min number of steps where epoch's neightours are also > min number of steps

epoch <- as.data.table(epoch.summary.10sec)
rm(epoch.summary.10sec)

# drop unneeded cols
epoch[,':='(
  filename = NULL,
  epochlengthinseconds = NULL,
  axis1 = NULL,
  axis2 = NULL,
  axis3 = NULL
)]

# define min number of steps 
min.steps <- 6

# drops rows and neighbouring rows to below min steps
## note this doesn't account for change in device id
drop.rows <- sapply(seq_along(epoch$steps),
                    function(x){
                      if(epoch$steps[x] < min.steps) return(FALSE)
                      if(x==1) {
                        if(epoch$steps[x+1] < min.steps) return(FALSE)
                      }
                      if(x==length(epoch$steps)) {
                        if(epoch$steps[x-1] < min.steps) return(FALSE)
                      }
                      if(epoch$steps[x+1] < min.steps | epoch$steps[x+1] < min.steps) return(FALSE)
                      return(TRUE)
                    })
# drop rows
epoch.filtered <- epoch[drop.rows]

# housekeeping
rm(drop.rows)

# format dates
epoch.filtered[,timestamp := ymd_hms(timestamp),][,date := floor_date(timestamp, "day"),]

# set keys
setkey(epoch.filtered, serialnumber, date, timestamp)

# filter to week of interest
epoch.filtered <- epoch.filtered[timestamp <= ymd_hms("20160503 000000") ]

# some renaming to match with raw data
epoch.filtered[,epoch_id := as.numeric(as.POSIXct(timestamp)),]
epoch.filtered[,device_id := serialnumber,]
setkey(epoch.filtered, device_id, epoch_id)

cache("epoch.filtered")
