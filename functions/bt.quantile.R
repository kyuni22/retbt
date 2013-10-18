####################################################################################################
## Making quantile
## sim.data -> data.frame of all data
## sim.factor -> factor to make quantile
## n -> number of quantile
####################################################################################################
bt.quantile <- function(sim.data, sim.factor, by.factor, n, name='sig'){
  #n Quantile
  sim.data[[name]] <- as.factor(unsplit(lapply(split(sim.data[[sim.factor]], sim.data[[by.factor]]),
                                                  function(x){
                                                    cut(x, breaks = quantile(x, seq(0, 1, 1/n),
                                                                                    na.rm = TRUE),
                                                               include.lowest = TRUE, labels = FALSE)
                                                  }), sim.data[[by.factor]]))
  
  #Return
  return((sim.data))
}