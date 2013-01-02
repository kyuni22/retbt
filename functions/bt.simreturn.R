####################################################################################################
## Return 'returns' of sig per date
## sim.data: data.frame having all data
## tax.cost: tax cost -> 0.3% now
####################################################################################################
bt.simreturn <- function(sim.data, tax.cost){
  
  # Mean of categorized return
  simsum <- tapply(sim.data$ret, list(sim.data$date, sim.data$sig), mean, na.rm=TRUE)
  
  #if tade cost > 0 then...
  if (tax.cost > 0) {
    #calculate Adjust Cost
    #split data per dates
    dates <- rownames(simsum)
    sim.data <- split(sim.data, sim.data$date)
    
    #split data per sig
    LastPF <- split(sim.data[[dates[1]]], sim.data[[dates[1]]][,'sig'])
    
    for(i in 2:length(dates)) {
      TodayPF <- split(sim.data[[dates[i]]], sim.data[[dates[i]]][,'sig'])  
      if(length(LastPF) > 0) { #if there's signal for LastPF
        Lastbuckets <- ls(LastPF)
        Todaybuckets <- ls(TodayPF)
        
        buckets <- setdiff(Lastbuckets, Todaybuckets) #differenct signals between Lastbuckets and Todaybuckets
        if(length(buckets) > 0) {
          #No sinal in Today PF. substract tax cost to Last Pf return
          for(bucket in buckets) {
            simsum[dates[i-1],bucket] <- simsum[dates[i-1],bucket] - tax.cost #substract tax cost
          }
        }
        
        buckets <- intersect(Lastbuckets, Todaybuckets) #Same signals between Lastbuckets and Todaybuckets
        if(length(buckets) > 0) {
          #Same signal exist in both today and Last PF. compare code
          for(bucket in buckets) {
            simsum[dates[i-1],bucket] <- simsum[dates[i-1],bucket] - tax.cost*length(setdiff(LastPF[[bucket]][,'Code'], 
                                                                                          TodayPF[[bucket]][,'Code']))/length(LastPF[[bucket]][,'Code']) #substract tax cost
          }
        }    
      } #else no effect to simsum returns
      LastPF <- TodayPF
    }
  }
  
  # Convert into xts for Performance Analysis
  simdate <- rownames(simsum)
  simdate <- as.POSIXct(simdate)
  
  rownames(simsum) <- rep("",nrow(simsum))
  
  simsum <- xts(simsum, simdate)
  simsum <- replace(simsum, is.na(simsum), 0)
  
  #Return
  return((simsum))
}