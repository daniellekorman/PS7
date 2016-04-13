sg.int<-function(g, dimension, lower, upper)
  
{ 
  # packages required for this function
  require("SparseGrid")
  # for parallel
  require("plyr")
  # for unit testing
  require("testthat")

 lower<-floor(lower)

 upper<-ceiling(upper)

 if (any(lower>upper)) stop("lower must be smaller than upper")

 gridss<-as.matrix(expand.grid(seq(lower[1],upper[1]-1,by=1),seq(lower[2],upper[2]-1,by=1)))

 sp.grid <- createIntegrationGrid( 'KPU', dimension, k=5 )

 nodes<-gridss[1,]+sp.grid$nodes

 weights<-sp.grid$weights

 for (i in 2:nrow(gridss))

 {
    nodes<-rbind(nodes,gridss[i,]+sp.grid$nodes)  

    weights<-c(weights,sp.grid$weights)

  }

  gx.sp <- apply(nodes, 1, g,...)
  val.sp <- gx.sp %*%weights
  val.sp
  system.time(out2 <- laply(sg.int, .parallel=TRUE))
}
