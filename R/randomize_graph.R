#' Randomize Graph
#' 
#' This function randomizes the edges in a popgraph and 
#'  returns a new graph.
#' @param graph An object of type \class{popgraph} or \class{igraph}
#' @param mode The kind of randomization to conduct, can be "full"
#'  which makes a new graph with the same number of edges as the 
#'  original one, or "
#' 

randomize_graph <- function( graph=NULL, mode=c("full","degree")[2] ) {
  if( is.null(graph))
    stop("Cannot run without a network")

  if( mode == "full"){
    
  }  
  
  
  
}



