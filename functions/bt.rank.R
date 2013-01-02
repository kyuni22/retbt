####################################################################################################
## Convert Return Ranking
## input data in data.frame class, factor to rank, and by.factor 
####################################################################################################
bt.rank <- function(sim.data, rank.factor, by.factor){
  
  rank.result <- unsplit(lapply(split(sim.data[[rank.factor]], sim.data[[by.factor]]),
                 function(x){
                   rank(-x, na.last=TRUE)
                 }),
                 sim.data[[by.factor]])
  
  #Return
  return((rank.result))
}