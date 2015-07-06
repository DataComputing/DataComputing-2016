#' Helper functions for drawing networks
#'
#' These functions translate an edge list into a positioned 
#' node list, and augment the edge list with the positions 
#' in the node list. Suppose you have only an edges file Edge with columns 
#' from, to, strength, age.  Make the corresponding Vertex file:
#' \code{V <- edgesToVertices( Edge, from=from,to=to )}
#' V will be a vertices file with x and y assigned
#' Or, if you already have a V with x and y, skip the edgesToVertices
#' step.
#' Now augment the Edge dataframe with the positions
#' x, y, xend, yend 
#' \code{E <- edgesForPlotting( V, ID=ID, x=x, y=y, Edges, from=from, to=to)}
#' Now you can plot E and V
#' \code{ggplot() + }
#' \code{geom_segment(E,aes(x=x,y=y,yend=yend,xend=xend)) + }
#' \code{geom_point( V, aes(x=x,y=y) ) + }
#' \code{geom_text( V, aes(x=x,y=y,label=ID))}
#' 
#' 
#' @rdname DrawingNetworks
#' @aliases edgesToVertices edgesForPlotting
#' @param \code{Edges} Data frame giving, at a minimum, 
#' the names of the nodes arranged as an edgelist: 
#' <to> and <from> columns where each case is one edge.
#' @param \code{from} Name of the <from> variable in the edgelist
#' @param \code{to} Name of the <from> variable in the edgelist
#' @param \code{Vertices} a data frame containing vertex IDs
#' and x,y coordinates for each vertex.
#' @param \code{ID} variable containing the ID of 
#' the vertices
#' @param \code{x} variable holding x-position of vertex
#' @param \code{y} variable holding y-position of vertex
#' @param \code{Edges} dataframe containing the from and to 
#' connection for each edge.  from and to should be drawn 
#' from the same set as ID in Vertices.
#' @return A data frame containing all the vertex IDs,
#' with x and y positions for each one.
#' @export
edgesToVertices <- function( Edges, from, to ) {
  vars <- structure(as.list(match.call()[-1]), class = "uneval")
  if (missing(from) || missing(to))
    stop("Must provide 'from' and 'to' variable names.")
  From <- eval( vars$from, envir=Edges )
  To   <- eval(   vars$to, envir=Edges )
  Bare <- data.frame( from=From, to=To)
  # Extract the set of vertex names from the Edges data
  Vertices <- data.frame( ID=unique(
    c(as.character(From),as.character(To)) 
    )
  )
  # get the positions automatically
  Net <- graph.data.frame( Bare )
  where <- layout.fruchterman.reingold( Net )
  # where <- layout.circle( Net )
  Vertices <- data.frame( ID=vertex.attributes(Net)$name,
                     x = where[,1], y = where[,2] )
  return( Vertices )
}


#' @export
edgesForPlotting <- function( Vertices, ID, x, y,
                              Edges, from, to) {
  
  vars <- structure(as.list(match.call()[-1]), class = "uneval")
  if (missing(from) || missing(to))
    stop("Must provide 'from' and 'to' variable names.")
  From <- eval( vars$from, envir=Edges )
  To   <- eval(   vars$to, envir=Edges )
  ID   <- eval(   vars$ID, envir=Vertices)
  X    <- eval(    vars$x, envir=Vertices )
  Y    <- eval(    vars$y, envir=Vertices )
  # Create a data frame holding the vertex ID/position info
  VPos  <- data.frame( ID=ID, X=X, Y=Y )
  # check that the vertex IDs are compatible
  if ( !all( 
        unique( c( as.character(From), 
                   as.character(To))) %in%
        Vertices$ID ) )
    stop("Vertex set doesn't match Edge set completely.")
 
  # join the x,y vertex positions to the corresponding
  # <from> and <to> IDs as <x>,<y>, <xend>, <yend>
  names(VPos) <- c(as.character(vars$from),"x","y")
  Edges <- merge( Edges, VPos, all.x=TRUE,
                       by=as.character(vars$from)) # the from locations
  names(VPos) <- c(as.character(vars$to),"xend","yend")
  Edges <- merge( Edges, VPos, all.x=TRUE,
                  by=as.character(vars$to))
  return( Edges )
}    

