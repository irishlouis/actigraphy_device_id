# publish rmd  
## take in argument html_document, pdf_document or word_document
### defaults to html_document
## save to reports folder, named with today's date

#' publish.rmd
#' 
#' @description publish RMD document
#'
#' @param file.name - name of RMD document to publish, if not entered will seach for .RMD docs in WD 
#' @param format - defaults to html
#'
#' @return nothin
#' @export saves document in ./retorts with file name and sys.date
#' 
publish.rmd <- function(file.name = NULL, format = "html_document") {
  library("rmarkdown")
  require(stringr)
  # get current date for naming
  date <- Sys.Date()
  # get name of RMD file if required
  if(is.null(file.name)) file.name <- list.files()[grep(pattern = ".rmd", x = tolower(list.files()))]
  if(length(file.name) == 0) {
    message("Enter filename (and path if not in wd)")
    return()
  }
  name <- str_replace(str_replace(tolower(file.name), ".rmd", ""), " ", ".")
  if(format == "html_document") {
    dot.format = ".html"} else {dot.format= ifelse(format == "pdf_document", ".pdf", ".docx")}
  
  # publish
  render(input= file.name, 
         output_format = format,
         output_file= paste0("reports/",name,".", date, dot.format))
}
