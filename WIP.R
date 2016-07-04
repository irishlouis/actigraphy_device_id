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