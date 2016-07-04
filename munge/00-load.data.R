# working directory, both local

setwd("Dropbox/UCD MSc uncompressed")
setwd("C:/Users/smithlou/Desktop/raw data")

files <- list.files()

# load epoch file
epoch <- fread(files[grep("epoch", files)])

# load processed raw data files
dt <- rbindlist(lapply(files[grep("processed", files)], fread, stringsAsFactors = FALSE))
setkey(dt, device_id, epoch_id)