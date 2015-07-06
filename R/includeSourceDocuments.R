#' Include source files such as Rmd in an Rmd->HTML document
#' 
#' This function, when invoked in an Rmd document, will produce links to embedded
#' base64 encodings of source documents.  This allows you to include source files
#' (such as Rmd, csv, etc.) inside the HTML file, so that only the HTML file need 
#' be uploaded to sites such as RPubs.com or a course management system. By default, 
#' the .Rmd document which is being compiled will be included.  You can also list files 
#' by name.
#' 
#' @details Use this function in an R chunk in an Rmd file.  The chunk should 
#' use \code{results="asis"}.
#' Do not embed the HTML file in itself --- just the Rmd and whatever other
#' auxiliary files you want to package up with the HTML.
#' 
#' @return A character string of HTML markup containing the set of links and the
#' base64 encoded embedded files.  The string may be quite long.
#'  
#' @param \code{...} Character strings with path and file name of a file to embed.
#' The path should be relative to where the compilation of the Rmd file occurs, 
#' which typically will be in the same directory as the Rmd file.additional character strings naming files.
#' 
#' @examples
#' \dontrun{
#' # in an R chunk in the Rmd file
#' includeSourceDocuments()
#' includeSourceDocuments("sourceFile.Rmd","myData.csv","mySpreadsheet.xlsx")
#' }
#' @export


includeSourceDocuments <- function(...) { 
  userName <- Sys.getenv("USERNAME")
  userHome <- Sys.getenv("HOME")
  toolTip <- paste('User',userName,'at',userHome)
  # Helper function
  attachFile <- function(file){
    encodedFile <- base64enc::dataURI(file = file, mime = mime::guess_type(file)) 
    # mime = 'text/rmd')
    
    linkHTML <- paste0("<a href='%s' target='_blank' title='",
                      toolTip,"' download='",
                      basename(file),"'> &#8658; ",file,"</a>  ")
    sprintf(linkHTML, encodedFile)
  }
  # Do the work.
  files <- unlist(list(...)) # change arguments to a vector of strings
  # If no arguments, get the source document itself from knitr::current_input()
  if (length(files) == 0 ) files <- knitr::current_input()
  for(F in files) { 
    if (!file.exists(F)) stop( 
      paste("File", F, 
            "doesn't exist.",
            "Are you sure you used the right name?")
    )
    cat(attachFile(F))
  }
}