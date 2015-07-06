#' Select n widely representative random cases
#' 
#' Sometimes you want a small sample of a data frame --- as with head() ---
#' but you want as many different levels of the variables as possible to
#' be included.  This function takes a sample from a dataframe, arranging things so that
#' as many distinct levels as possible of each value for each variable 
#' are kept.  The incoming random seed will be restored when the function exits.
#'
#' @rdname representativeCases
#' @param .df data frame from which to select rows
#' @param n number of rows to return 
#' @param seed a random seed to use for the random parts of the algorithm.
#' If you want another result, change the seed.
#' 
#' @examples 
#' representativeCases(BabyNames, n=6)
#' @export
representativeCases <- function( .df, n=5L, seed=15 ) {
  if ( !inherits( .df, c("data.frame", "tbl","tbl_df")) )
       stop("Did not provide a data frame.")
  if (n <= 0) stop("n was negative or zero.")
  
  # Prepare the random seed and restore on exit
  originalRandomState <- .Random.seed
  set.seed( seed )
  on.exit( .Random.seed <<- originalRandomState )
  
  # Strategy: Winnow out the cases by removing duplicates from each variable.
  # Start with the variables with the largest number of discrete levels and 
  # move on the the variables with few levels.
  vTypes <- lapply( .df, function(x) length( unique(x) ) )
  classNumber <- function( classes ) {
    primary <- c("factor", "integer","character")
    secondary <- c("numeric")
    res <- rep( 3, length(classes) )
    res[classes %in% primary] <- 1
    res[classes %in% secondary] <- 2
    return(res)
  }
  colPriorities <- classNumber( vTypes )
  # Order the columns by the number of different values, from most to least
  nUniqueVals <- unlist( lapply( .df, function(x) length(unique) ) )
  colOrder <- order( -nUniqueVals )
  Surviving <- .df
  # Loop over the surviving columns
  for( col in colOrder ) {
    var <- Surviving[,col]
    keepInds <- !duplicated( var )
    NextRound <- Surviving[keepInds,]
    if (nrow( NextRound ) <= n ) {
      return( sample_n(Surviving, n))
    } else {
      Surviving <- NextRound
    }
  }
  
  return( sample_n( Surviving, size=n ) )
}