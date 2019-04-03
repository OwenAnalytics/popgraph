#' Convience function to plot edges from \code{popgraph}
#' 
#' This is a quick convienence function for plotting nodes and edges 
#'   on a normal R plot by iterating through edges and connecting
#'   nodes.
#' @param graph An object of type \code{popgraph}
#' @return NULL
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @export
overlay_popgraph <- function( graph ){
  stop("Not functioning at this time, I'm in Europe and will fix when I return")
  
  if( !is(graph,"popgraph") && !is(graph,"igraph"))
    stop("This function only works by with a population graph or igraph object")
  if( !("Longitude" %in% list.vertex.attributes(graph)))
    stop("This function requires 'Longitude' to be a vertex attributed.")
  if( !("Latitude" %in% list.vertex.attributes(graph)))
    stop("This function requires 'Latitude' to be a vertex attributed.")
  if( !("name" %in% list.vertex.attributes(graph)))
    stop("This function uses the 'name' property of the graph and it is missing.")
  
  coords <- cbind(igraph::V(graph)$Longitude, igraph::V(graph)$Latitude )
  nodes <- igraph::V(graph)$name
  K <- length(igraph::V(graph))
  
  
  # plot the edges, one at a time.
  A <- to_matrix(graph,mode="adjacency")
  if( !("color" %in% list.edge.attributes(graph)))
      E(graph)$color <- "red"

  for( i in 1:K ) {
    for( j in i:K) {
      if( A[i,j] > 0 ){
        x <- c(coords[i,1],coords[j,1])
        y <- c(coords[i,2],coords[j,2])
        lines(x,y,col=E(graph)$color )
      }
    }
  }  
  
  # plot the nodes
  if( !("color" %in% list.vertex.attributes(graph) ))
    igraph::V(graph)$color <- "#dd7f4c"
  points( coords, pch=16, col=igraph::V(graph)$color )
  
  # add the names
  text( coords, igraph::V(graph)$name , cex=.75)
  

}
