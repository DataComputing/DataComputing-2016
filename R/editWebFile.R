#' Display a file from the web in the editor
#' 
#' Reads a text file from the web and opens the contents in an editor.
#' This is useful, for instance, for sharing assignment templates with students. 
#' You MUST "save as" the editor buffer in your own folder 
#' if you want to keep it.
#' 
#' @return The temporary file name, invisibly.
#' @param url URL locating the file on the web
#' @examples \dontrun{
#'    editWebFile("www.mosaic-web.org/go/datasets/angle.R")
#' } 
#' @export
editWebFile <- function(url){
  contents <- RCurl::getURL(url)
  fname <- basename(url)
  partsOfName <- stringr::str_match_all("our_file_today.html",
                                        "(^.+)(\\.(Rmd|r|R|md|tex|.{1,4})$)")
  filename <- unlist(partsOfName)[2]
  extension <- unlist(partsOfName)[3]
  tmpname <- tempfile(pattern=paste0(filename,"---"), fileext=extension)
  cat(contents,file=tmpname) # write contents to temporary file
  file.edit(tmpname)
  message("Remember to save as ",
          paste0(filename,extension),
          " in your own folder. Until you do, it's just temporary.")
  invisible(tmpname)
} 
  
foo <- "https://dl.dropboxusercontent.com/u/5098197/MuhlenbergWorkshop/example1.Rmd"
