context("overlay_popgraph.R")

test_that("testing", {
 
  A <- matrix(0, nrow=5, ncol=5)
  A[1,2] <- A[2,3] <- A[1,3] <- A[3,4] <- A[4,5] <- 1
  A <- A + t(A)
  
  
  library(igraph)
  g <- graph_from_adjacency_matrix( A , mode="undirected")
  
  V(g)$name <- c("Olympia","Bellingham","St. Louis","Ames","Richmond")
  V(g)$group <- c("West","West", "Central","Central","East")
  V(g)$color <- "#cca160"
  
  V(g)$Latitude <- c( 47.15, 48.75,38.81, 42.26, 37.74 )
  V(g)$Longitude <- c(-122.89,-122.49,-89.98, -93.47, -77.16 )
  
  
  library(maps)
  library(popgraph)
  pg <- as.popgraph( g )
  map( "state" )
  overlay_popgraph( pg )
}
)
