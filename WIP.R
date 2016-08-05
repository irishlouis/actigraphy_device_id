
## sync data file from dropbox for EC2 instance
library(RStudioAMI)
excludeSyncDropbox("*")
# sync with dropbox folder
includeSyncDropbox("UCD_MSc_uncompressed")
# check status
dropboxStatus()

# confirm all files there
system('ls ~/Dropbox/UCD_MSc_uncompressed')
# check what's in proj /data - should be empty
system('ls ~/actigraphy_device_id/data')
# copy files across
system('cp ~/Dropbox/UCD_MSc_uncompressed/* ~/actigraphy_device_id/data')
# check files copied
system('ls ~/actigraphy_device_id/data')

###########################################################################
## clean slate

pkgs <- names(sessionInfo()$otherPkgs) 
if(!is.null(pkgs)){
  pkgs <- paste('package:', pkgs, sep = "")
  lapply(pkgs, detach, character.only = TRUE, unload = TRUE, force = TRUE)
}
rm(list = ls()[ls() != "run.munge"])

###########################################################################

require(ProjectTemplate)
load.project()

load.cache()

###########################################################################

run.munge()

###########################################################################

publish.rmd()
# publish.rmd("pdf_document")  ## no pdf creator installed
publish.rmd("word_document")

###########################################################################