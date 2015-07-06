
load(system.file("LocalData","worldshapes.rda",package="DCF") )
# world <- read.csv(system.file("LocalData","world.csv",package="DCF"))
# avg <- read.csv(system.file("LocalData/avg.csv",package="DCF"))


#' Return a dataset based on the CIA World Factbook
#' 
#' This function can be used in two different ways. Without an argument, it returns a reference
#' table that includes information about all the CIA World Factbook tables that are available
#' through this function. Note the  \code{Code} column that indicates a unique code for each
#' available dataset. If this code is passed as an argument to the function, the function 
#' will return the corresponding dataset.
#' 
#' @param code An optional parameter specifying the code of the desired dataset
#' @export
CIAdata <- function (code=NULL) {
  CIA = read.csv(system.file("LocalData","CIA.csv",package='DCF'), stringsAsFactors=FALSE)
  if (is.null(code)) {
    return(CIA)
  } else {
    if (code %in% CIA$Code) {
      sub <- subset(CIA, Code==code)
      url <- (paste0("https://www.cia.gov/library/publications/the-world-factbook/rankorder/rawdata_", code, ".txt"))
      table <- read.delim(textConnection(getURL(url, ssl.verifypeer=FALSE )), 
                          header=FALSE, stringsAsFactors=FALSE)
      table[,1]<-NULL
      names(table) <- c("country", sub[["Name"]])
      table[[2]] = as.numeric(gsub("[,|[A:Za:z]|\\$| |]", "", table[[2]]))
      return(table)
    }
  }
}



meltCountries <- function() {
  data(countrySynonyms)
  syn <- countrySynonyms[-1]
  mSyn <- reshape2::melt(syn, id.vars="ISO3", measure.vars=names(syn), na.rm=TRUE)
  mSyn <- mSyn[-2]
  mSyn <- subset(mSyn, value != "")
  colnames(mSyn) <- c("Name", "Alternative")
  return(mSyn)
}


#' Turn country names into ISO 3166-1 alpha-3 codes.
#' 
#' ISO 3166-1 alpha 3 codes are three-letter codes for countries, as specified 
#' by the International Standards Organization.
#' 
#' @param names a vector of country names
#' 
#' @return a corresponding vector of ISO 3166 codes.  \code{NA} when no match
#' can be found for an element of \code{names}.
#' 
#' @details This function will typically be used with \code{mutate} or \code{transform}
#' to create a column that is standard across datasets.
#' 
#' TO DO: Add in options for ISO2, num, etc. 
#' 
#' @examples
#' toISO3( c("Russia","China", "Luxembourg", "Nowhere Land"))
#' @export
toISO3 <- function(names) {
  df <- data.frame(Alternative=toupper(names),nameOrder=1:length(names))
  ISO <- meltCountries()
  ISO2 <- unique(
    transform(ISO, ISO3=toupper(Name),
           Alternative=toupper(Alternative)
           ) 
    )
  tmp <- merge( df, ISO2, by='Alternative',all.x=TRUE)
  return(with(tmp, ISO3[order(nameOrder)])) 
}


standNames <- function (data, syn, varname.x, varname.y, nonMatch = c(FALSE, TRUE)) {
  data[[varname.x]] <- toupper(data[[varname.x]])
  syn[[varname.y]] <- toupper(syn[[varname.y]])
  df <- merge(data, syn, by.x = varname.x, by.y = varname.y, all.x=TRUE)
  unmatched <- c()
  for (i in 1:nrow(df)) { 
    if (is.na(df[i, ncol(data)+1])) {
      unmatched <- append (unmatched, df[i, varname.x])
      df[i, ncol(data)+1] <- df[i, varname.x]
    }
  } 
  df <- df[- which(names(df) %in% varname.x)]
  names(df)[ncol(data)] <- varname.x
  if (nonMatch == TRUE) {
    message(paste("There were", length(unmatched), "unmatched regions."))
    return(list(Data=df, Unmatched=unmatched))
  } else {
    message(paste("There were", length(unmatched), "unmatched regions."))
    return(list(Data=df))
  }
}


#' Standardize country names in a dataset
#'
#' This function is built-in the \code{MakeWorldMap} function, so you shouldn't
#' need to use directly.
#'
#' @param data A dataset with countries as cases
#' @param varname.x The variable name for the column that holds the countries names
#' @param nonMatch A boolean specifying whether or not a list of the countries without
#'  a match should be returned by the function
#' @return The updated dataset with standardized names (\code{$Data}) and the list of
#' countries without a match (\code{$Unmatched}) if the argument \code{nonMatch} is
#' set to \code{TRUE}
#' @export
standCountry <- function (data, varname.x, nonMatch = c(FALSE, TRUE)) {
  standNames (data=data, syn=meltCountries(), 
              varname.x=varname.x, varname.y="Alternative", nonMatch=nonMatch)
}

makeMap <- function (datafile, shapefile, varname.x, varname.y, nonMatch = c(FALSE, TRUE)) {
  sc <- standCountry (data = datafile, varname.x = varname.x, nonMatch = nonMatch)
  sc$Data[[varname.x]] <- toupper(sc$Data[[varname.x]])
  shapefile[[varname.y]] <- toupper(shapefile[[varname.y]])
  df <- merge(sc$Data, shapefile, by.x = varname.x, by.y = varname.y)
  map <- ggplot(df, aes(x = long, y = lat, group = group, order = order)) 
  return (list(Data=df, Unmatched=sc$Unmatched, Map=map))
}


#' Create a ggplot2 frame with a world map.
#'
#' This function takes in a dataset with countries as cases. 
#' Provide the country name as a three-letter iso_a3 or any common name
#' for the country (e.g. Canada, Fiji).
#' 
#' @details
#' It standardizes the
#' country names and merges it another dataset which includes geographic coordinates
#' for each country. It returns a ggplot map with no layers. It prints a message
#' indicating how many countries will not be plotted because no latitude /
#' longitude data was found.
#' 
#' 
#'
#' @param data A dataset with countries as cases
#' @param varname.x The variable name for the column that holds the countries names
#' @return A ggplot map object with no layers. 
#' @examples \dontrun{ 
#' somedat <- CIAdata(2001) # 2001 codes for GDP
#' p <- makeWorldMap(somedat,'country')
#' p + geom_polygon(col="blue",aes(fill=GDP))
#' }
#' @export 
makeWorldMap <- function (data, varname.x) {
  map <- makeMap (datafile = data, shapefile = world.shapes,
                  varname.x = varname.x, varname.y = "iso_a3", nonMatch = FALSE)
  return (map$Map)
} 


#' Return a list of countries that are not plotted in corresponding map
#' 
#' @param data A dataset with countries as cases
#' @param varname.x The variable name for the column that holds the countries names
#' @export
getUnmatched <- function (data, varname.x) {
  map <- makeMap (datafile = data, shapefile = world.shapes,
                  varname.x = varname.x, varname.y = "iso_a3", nonMatch = TRUE)
  return (map$Unmatched)
} 


#' Return the complete dataset used to plot the corresponding map
#' 
#' @param data A dataset with countries as cases
#' @param varname.x The variable name for the column that holds the countries names
#' @export
getJoinData <- function (data, varname.x) {
  map <- makeMap (datafile = data, shapefile = world.shapes,
                  varname.x = varname.x, varname.y = "iso_a3", nonMatch = FALSE)
  return (map$Data)
} 
