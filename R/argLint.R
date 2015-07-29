argsFromString <- function( S ) {
  # Separate argument string with commas between args
  # The commas will always be outside of parens.
  # Commas inside parens are for function calls
  # in arguments, e.g. b=fun(6+x, y, z). Don't split on 
  # those
  
  # Break string into character vector
  contents <- unlist( strsplit( S, ""))
  # <vec> will be non-zero for any contents within parens
  openParenInds <- which( contents=="(")
  closeParenInds <- which( contents==")")
  vec <- rep(0, length(contents))
  vec[openParenInds]  <-  1
  vec[closeParenInds] <- -1
  vec <- cumsum(vec)
  # Mark breaking points between arguments (with @)
  contents[vec==0 & contents==","] <- "@"
  args <- strsplit( paste0(contents,collapse=""), "@ +")[[1]]
  # Check for mismatched parens.  
  # Happens when the last element of vec is non-zero or if any element is negative
  mismatched <- vec[length(vec)] != 0  | any( vec<0 ) 
  L <- data.frame( args=args,
             mismatched=mismatched,  # mismatched parens
             stringsAsFactors=FALSE
             )
  return( L )
}
# ========================
# Does the argument involve assignment, e.g. a=x+7
# Returns a vector of LHS, RHS (where the RHS is the whole
# expression if there is no assignment.)
splitAssignment <- function( argStrings ) {
  argStrings <- as.character( argStrings )
  if (!require(stringr ) ) 
    stop("Must install stringr package.")
  tmp <- stringr::str_match_all( argStrings, "(.*[^=])=([^=].*)" )
  # match has true or false for each statement: is it an alignment
  match <- sapply(tmp, function(s){!identical(s, character(0))})
  before <- sapply( tmp, 
                    function(L) {if(length(L)>1) L[[2]] else NA })
  after <- sapply( tmp, 
                   function(L) {if(length(L)>2) L[[3]] else NA})
  # include the arguments that are not in assignment form.
  after[!match] <- argStrings[!match]
  
  return(list(match=match, before=before, 
              after=after) )
}
# ========================
# Get the variable names from a set of expressions 
# in string form
getVarNames <- function( S ) {
  varNameHelper <- function( str ) {
    E <- try( parse( text=str ), silent=TRUE ) 
    if( !inherits( E, "try-error") ) all.vars(E) else character(0)
  }
  unname( unlist(sapply(S, varNameHelper)) )
}
# ========================
# get the names of any mismatches of the names of X 
# to a list of names
# return character(0) if there are no mismatches
# Use as mismatchedNames( formals(FUN), names)
# or mismatchedNames( DF, names )
mismatchedNames <- function( X, names ) {
  names[ !names %in% names(X) ]
}
# ========================
# Do the complete checking on a character string 
# containing one or more comma-separated arguments to 
# dplyr functions, 
# using the name of the function, names of vars from the 
# data frame, and a flag to signal whether the function 
# permits assignment to non-formal args. 
# e.g. mutate() does permit this, filter() doesn't )
checkDplyrArgs <- function( S, Fname, DF, mustBe, assignOK=FALSE ) {
  theArgs <- argsFromString( S )

  badParens <- theArgs$mismatched
  tmp <- splitAssignment( theArgs$args )
  
  # check that the argument specified in <mustBe> has the right type  
  message = ""
  if (length( theArgs$args) < length(mustBe)) 
    message <- paste("At least", length(mustBe), "arguments required.\n")
  else{
    for (k in seq_along(mustBe) ){
      Thing <- try(
        eval(parse(text=paste(theArgs$args[k]) ) )
      )
      if (inherits(Thing, "try-error"))
        message <- paste( theArgs$args[k], "not defined in workspace.\n")
      else if ( ! inherits(Thing, mustBe[k]) ) {
        message <- paste( message, "Argument", k, "must be a ", mustBe[k],"\n")
      }
    }
  }
  LHSassignVars <- getVarNames( tmp$before )
  after <- tmp$after
  if (length(mustBe) > 0 ) after <- after[-seq_along(mustBe)]
  RHSVars <- getVarNames( after )
  if (Fname %in% c("NONE") ) {
    invalidAssignments <- c()
  } else {   
    invalidAssignments <- mismatchedNames( 
      formals(Fname), LHSassignVars)
  }
  invalidDataReferences <- mismatchedNames(
    DF, RHSVars )
  return( list( args=theArgs$args, badParens=badParens,
                LHSproblems=if ( assignOK ) character(0) else invalidAssignments,
                RHSproblems=invalidDataReferences,
                mustBeMismatches=message,
                Fname=Fname, validInData=names(DF))
          )
}

# =================================
# Find the most similar string from <valid> to <candidate>
# e.g. mostSimilar( names(DF), "mispelled var")
mostSimilar <- function( valid, asTheyAre ) {
  if (!require("stringdist")) 
    stop("Must install 'stringdist' package.")
  similarHelper <- function(asItIs) {
    ind <- which.min( 
      stringdist::stringdist(tolower(asItIs),tolower(valid)))
    valid[ind]
  }
  sapply( asTheyAre, similarHelper )
}


## IMPORTANT CHECK
# Make sure that, when there is no assignment, the <after> contains 
# the entire statement.  right now, it does not. 

# =================================
# turn the results of checkDplyrArgs() into human readable form
formatDplyrCheck <- function( L ) {
  sofar <- L$mustBeMismatches
  if( any( L$badParens) ) 
    sofar <- paste(sofar,"Mismatched parentheses!\n")
  if( length( L$LHSproblems) > 0 )
    sofar <- paste(sofar,"Function ", L$Fname, "() does not permit ",
                   "assignment to ",
                   paste(L$LHSproblems, collapse=", "),
                   "\n.")
  if( length(L$RHSproblems) ){
    sofar <- paste( sofar, paste(L$RHSproblems, collapse=", "),
                    "not names of variables in the data frame.\n")
    sofar <- paste( sofar, 
                    paste(paste( 
                      mostSimilar(L$validInData,
                                  L$RHSproblems), collapse=", "),"are close\n."))
  }
  # Need to work in <na.rm> for statements involving mean(), sum(), etc.
  return(sofar)
}