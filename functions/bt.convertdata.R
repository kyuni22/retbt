####################################################################################################
## Convert bactesting data xts into dataframe
## with mkt_cap_rank
####################################################################################################
bt.convertdata <- function(dataEnv){
  
  codes <- ls(dataEnv)
  
  alldata <- {}
  
  #Converting xts into data framed
  for (code in codes) {
    #Lagging Except Price -> For backtesting purpose
    tmp <- lag(dataEnv[[code]])
    tmp$Close <- Cl(dataEnv[[code]])
    #Return 
    tmp$ret <- ROC(Cl(tmp), type="discrete")
    ## Note: you can make it fwd return instead of using backward return    
    
    #Converting
    simdata <- data.frame(date=index(tmp), coredata(tmp))
    simdata <- cbind(simdata, rep(code, nrow(simdata)))
    colnames(simdata)[ncol(simdata)] <- "Code"
    simdata <- simdata[-1,] #lagging make first row unnecessary -> erase!
    alldata <- rbind(alldata, simdata)
  }
  #Return
  return((alldata))
}