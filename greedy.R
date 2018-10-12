set.seed(42)
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
  )



greedy_knapsack <- function(Z,W){
      X<- Z
       X <- transform(X, c= v/w )
       X<- X[order(X$c,decreasing = TRUE),]
       w8  <- vector()
       j=0
       Val <- vector()
       elements <- vector()
         while(sum(w8) <= W)
           {
           w8 <-  append(w8,X$w[j])
           Val <- append(Val,X$v[j])
           j <- j+1
           
         }

       if(sum(w8)> W)
       { 
         w8 <- head(w8,-1)
         Val <- head(Val,-1)
       }
       Val
       elements <- append(elements,which(Z$w  %in% w8))
       results <- list("value"= sum(Val),"elements"=elements,"weight"=sum(w8))
       return(results)
       
}



#greedy_knapsack(Z = knapsack_objects[1:1200,], W = 2000)



greedy_knapsack(Z = knapsack_objects[1:800,], W = 3500)

