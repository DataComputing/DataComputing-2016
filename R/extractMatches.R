#' Extract substrings that match a regex
#' 
#' Pulls out the matches for marked regions of regular expressions.
#' 
#' Wrap regexes in parentheses to signal that the matching content
#' is to be extracted as a string.
#' 
#' @details Use parentheses to mark out parts of regular expressions
#' that identify substrings to be extracted if there is a match.
#' For instance, the pattern \code{"(.*)[aeiouy]$"} matches all strings ending in a vowel.
#' The \code{"(.*)"} indicates that the substring to be extracted is all the characters in 
#' the string up to that final vowel. NA is returned for strings that have no match.
#' 
#' @param data a data frame. Or, use a pipe to specify the data frame.
#' @param pattern the regex to be used for matching
#' @param var the name of the variable from which to extract the substrings
#' 
#' @return a data frame that copies the original but adds a new variable 
#' giving the substring identified in the first parentheses.  If there 
#' are more than one set of extraction parentheses, a new variable is added for each.
#' 
#' @examples 
#' # grab the root of names ending in vowels. 
#' mosaicData::KidsFeet %>% extractMatches(pattern="(.*)[aeiouy]$", name)
#' # Two matches:
#' # grab the first letter (if capitalized) in strings ending in a vowel
#' # also grab any letter between the third character and the final vowel.
#' mosaicData::KidsFeet %>% extractMatches(pattern="^([A-Z])..(.*)[aeiouy]$", name)
#' @export
extractMatches <- function( data, pattern, var, ... ) {
  var <- substitute( var )
  resNames <- list( ... )
  dataStrings <- as.character( 
    eval( var, envir=data, enclos=parent.frame() ) 
  )
  results <- data.frame(stringr::str_match( dataStrings, pattern )[,-1])
  # default names: match1, match2, ...
  newNames <- paste0( "match", 1:ncol(results) )
  # pull out any names given in ...
  if (length(resNames) > 0){
    if (! all(unlist(resNames) %in% 1:ncol(results)) )
      stop( "New names should be assigned to values in 1:npatterns")
    newNames[unlist(resNames)] <- names(resNames)
  }
  names(results) <- newNames
  return( cbind(data, results ) )
}