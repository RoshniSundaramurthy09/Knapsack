
set.seed(42)
n <- 1000000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
  )



greedy_knapsack <- function(X,W){
       df <- transform(X, c= v/w )
       df<- df[order(df$c,decreasing = TRUE),]
       w8  <- vector()
       j=0
       Val <- vector()
       elements <- vector()
         while(sum(w8) <= W)
           {
           w8 <-  append(w8,df$w[j])
           Val <- append(Val,df$v[j])
           j <- j+1
           
         }

       if(sum(w8)> W)
       { 
         w8 <- head(w8,-1)
         Val <- head(Val,-1)
       }
       Val
       elements <- append(elements,which(X$w  %in% w8))
       results <- list("value"= sum(Val),"elements"=elements,"weight"=sum(w8))
       return(results)
       
}



#greedy_knapsack(Z = knapsack_objects[1:1200,], W = 2000)


#ptm <- proc.time()
#greedy_knapsack(X = knapsack_objects[1:1000000,], W = 3500)
#proc.time() - ptm

# time
#user  system elapsed 
#1.22    0.04    1.27 
