#' Determines of edges are stretched or compressed and adds attribute
#'  to popgraph object.
#'  

edge_contortion <- function( graph, latitude="Latitude", longitude="Longitude"){
  if( !is(graph,"popgraph"))
    stop("This function only works with Population Graph objects")
  
  atr <- vertex_attr_names(graph)
  if( !(all( c(longitude,latitude) %in% atr)))
    stop("You must specify latitude & longitude as node attributes.")
  
  G <- as.matrix( get.adjacency(graph,attr = "weight") )
  G <- G / sum( G )
  G[ G==0 ] <- NA
  coords <- data.frame( Stratum=V(graph)$name,
                        Longitude=vertex_attr(graph,longitude),
                        Latitude=vertex_attr(graph,latitude))
  P <- gstudio::strata_distance(coords)
  P[ is.na(G)] <- 0
  P <- P / sum(P)
  
  C <- G - P
  
  idx <- igraph::get.edges(graph,es = 1:length(E(graph)))
  igraph::E(graph)$contortion <- 0
  igraph::E(graph)$stretch <- ""
  for( i in 1:nrow(idx) ) {
    val <- C[idx[i,1],idx[i,2]]
    igraph::E(graph)$contortion[i] <- val
    igraph::E(graph)$stretch[i] <- ifelse( igraph::E(graph)$contortion[i] > 0 , "Compressed", "Extended")
  }

  return( graph )
}