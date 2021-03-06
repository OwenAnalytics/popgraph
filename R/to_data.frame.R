#' Converts \code{popgraph} to \code{data.frame} based upon node attributes
#' 
#' This is a quick conversion of vertex attributes to a \code{data.frame}, essentially
#'  the reverse operation as \code{decorate_graph} function.
#' @param x The \code{popgraph} to grab stuff from.
#' @param mode An indication of what to return, a \code{data.frame} of node (the default) 
#'  or edge properties.
#' @param as.named A flag (default \code{TRUE}) indicating whether the nodes are returned
#'  as indices or names.
#' @param ... Ignored (generally).
#' @return An object of type \code{data.frame} with all the node attributes.
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @export
to_data.frame <- function( x, mode=c("nodes","edges")[1], as.named=TRUE, ... ){
  if( !is(x,"popgraph") & !(is(x,"igraph")))
    stop("What are you passing to to_data.frame()?")
  
  ret <- NULL
  
  if( mode=="nodes")  {
    cols <- list.vertex.attributes( x )
    ret <- data.frame( vertex.id=seq(1,length(igraph::V(x))))
    if( length(cols) > 0 ) {
      ret[[cols[1]]] <- get.vertex.attribute(x,name=cols[1])
      for( i in 2:length(x))
        ret[[cols[i]]] <- get.vertex.attribute(x, name=cols[i])  
    }
    if( !("vertex.id" %in% cols))
      ret$vertex.id <- NULL
    
  }
  else if( mode=="edges") {
    wt <- igraph::get.edge.attribute(x,"weight")
    if( all(is.null(wt)))
      wt <- 1
    nodes <- igraph::as_edgelist(x,names=as.named)
    ret <- data.frame( source=nodes[,1], target=nodes[,2], value=wt)
  }  
  else
    stop("Unrecognized mode passed to to_data.frame()")

  return( ret )

}