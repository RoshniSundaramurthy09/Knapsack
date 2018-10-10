#' Knapsack problem using dynamic algortihm
#'  
#' @param x a data.frame with two variables v and w (values and weights)
#' @param W knapsack size (Max weight capacity)
#' 
#' @return the maximum values and elements to be carried by knapsack
#' 
#' @examples knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500)
#' @examples knapsack_dynamic(x = knapsack_objects[1:12,], W = 2000)
#' 
#'


knapsack_dynamic <- function(x, W){
  stopifnot(is.data.frame(x) == TRUE)
  stopifnot(is.numeric(W) == TRUE)
  n <- nrow(x)
  w <- x[[1]]  #weights
  v <- x[[2]]  #values
  cap <- W     #capacity of knapsack
  elem <- c()
  m <- matrix(0,nrow = n+1,ncol = cap+1)
 
  #to return the max value 
  for (i in 2:n){
    for (j in 1:cap){
      if (w[i] > j) 
        m[i,j] <- m[i-1, j]
      else 
        m[i, j] <- max(m[i-1, j], m[i-1, j-w[i]] + v[i])
    }
  }
  
  val <- m[i, j]
  value <- round(val)
  
  #to print the elements
  while(cap>0 && n-1>0){
    if(m[n,cap]!=m[n-1,cap]){
      elem<-c(elem,n)
      cap<-cap-w[n]
      elements <- sort(elem)
    }
    n<-n-1
  }
  return(list("value"=value,"elements"=elements))
  
}

#Example dataframe
set.seed(42)
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
  )

#system.time({knapsack_dynamic(x = knapsack_objects[1:8,], W = 2000)})

#run time of a chunk of code
start.time <- Sys.time()
knapsack_dynamic(x = knapsack_objects[1:8,], W = 2000)
end.time <- Sys.time()
end.time - start.time

