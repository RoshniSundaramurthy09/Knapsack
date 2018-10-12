#using proc.time() for n=16 it took: User:25.694. System:0.414. Elapsed:29.748
brute_force_knapsack <- function(x,W)
{stopifnot(is.data.frame(x),is.numeric(W))
  value <- 0
  elem <-c()
  
  for(i in 1:(2^nrow(x)-1))
  {wsum <- 0
  vsum <- 0
  loop <-c()
  binary <- intToBits(i)
  
  for(j in 1:length(binary))
  {if( binary[j] == TRUE )
  {
    wsum <- wsum + x[j,1]
    vsum <- vsum + x[j,2]
    loop <- c(loop,j)
  }
    
  }
  
  if(vsum > value && wsum <= W)
  {value<-vsum 
  elem<-loop}}
  
  return(list(value=round(unname(value)),elements=elem))
}