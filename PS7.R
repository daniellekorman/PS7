# SparseGrid integration function
sg.int<-function(g, dim, lower, upper)
  
{ 
  # packages required for this function
  # for integration
  require("SparseGrid")
  # for parallel
  require("plyr")

  # Creates lower rounded down
 lower<-floor(lower)
  # Creates upper rounded up
 upper<-ceiling(upper)

 # Lengths of lower and upper must be equal
 test_that("length of lower equals length of upper", {
   expect_that(length(lower), equals(length(upper)))
 })
 # lower must be smaller than upper
 if (any(lower>upper)) stop("lower must be smaller than upper")
 # Creates a matrix of all combinations of numbers in lower/upper sequence
 gridss<- as.matrix(expand.grid(apply(seq(), rbind(lower, upper-1, by=1))))
 # Separate grids to get approximation on integral
 sp.grid <- createIntegrationGrid( 'KPU', dimension=dim, k=5 )

 nodes<-gridss[1,]+sp.grid$nodes

 weights<-sp.grid$weights

 for (i in 2:nrow(gridss))

 {
    nodes<-rbind(nodes,gridss[i,]+sp.grid$nodes)  

    weights<-c(weights,sp.grid$weights)

  }

  system.time(gx.sp <- apply(nodes, 1, g,..., .parallel=TRUE))
  val.sp <- gx.sp %*%weights
  val.sp
  }

# for unit testing
require("testthat")
# for integration
require("cubature")
# for testing speed
require("microbenchmark")