####################################################################################################
## Convert bactesting data xts into dataframe
## with mkt_cap_rank
####################################################################################################
bt.convertdata <- function(dataEnv){
  
  codes <- ls(dataEnv)
  
  alldata <- {}
  
  #Converting xts into data framed
  for (code in codes) {
    simdata <- data.frame(date=index(dataEnv[[code]]), coredata(dataEnv[[code]]))
    simdata <- cbind(simdata, rep(code, nrow(simdata)))
    colnames(simdata)[ncol(simdata)] <- "Code"
    simdata <- simdata[-1,] #lagging make first row unnecessary -> erase!
    alldata <- rbind(alldata, simdata)
  }
  #Return
  return((alldata))
}