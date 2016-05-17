#' Randomize Graph
#' 
#' This function randomizes the edges in a popgraph and 
#'  returns a new graph.
#' @param graph An object of type \code{popgraph} or \code{igraph}
#' @param mode The kind of randomization to conduct, can be "full"
#'  which makes a new graph with the same number of edges as the 
#'  original one, or "degree" which preserves the degree distribution
#'  of the 
#' 

randomize_graph <- function( graph=NULL, mode=c("full","degree")[2] ) {
  if( is.null(graph))
    stop("Cannot run without a network")

  
  if( mode == "full"){
    num_edges <- length( igraph::E(graph ) ) 
    nodes <- igraph::V(graph)$name
    edge_list <- NULL
    
    for( i in 1:num_edges ) {
      
      
      nodes <- sample( nodes, size=length(nodes), replace=FALSE)
      
      
      edge_list <- rbind( edge_list, c(nodes[1:2]))
    }
    g <- igraph::graph.edgelist(edge_list, directed=FALSE )
    plot(g)    
  } 
  else if( mode == "degree" ){
    
  }
  
  stop("Unknown mode to randomize_graph")

}



