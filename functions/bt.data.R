####################################################################################################
## Retrieve Data From Database 
## and convert data class in to xts
## factor -> factor to bactest
####################################################################################################
bt.data <- function(factors=NULL, na.rm=TRUE){
  dbcon <- odbcDriverConnect('Driver={MySQL ODBC 5.1 Driver};DataBase=;Pwd=3450;UID=abcd;Server=localhost;OPTION=3')
  
  #env to load
  dataEnv <- new.env()
  
  #codes for Load
  codes <- sqlQuery(dbcon, paste('select sCode from sim.s01 where left(sCode,1) = \'A\' group by sCode'))[,1]
#   codes <- read.csv("Book6.csv", header=FALSE)
#   codes <- codes[,1]
  
  list.factors <- {}
  #factor
  for (factor in factors) {
    list.factors <- paste(list.factors, "f.", factor, ", ", sep="")
  }
  
  #load Data from DB
  for (code in codes) {
    if(!is.null(factors)) {
      loadquery <- paste("select s.tDate, s.Close, ", list.factors, "s.Mkt_cap
                        from sim.s01 s
                         inner join sim.f01 f
                         on s.tDate = f.tDate
                         and s.sCode = f.sCode
                         where s.Close is not null and s.sCode = '", code, "'", sep="")
    } else {
      loadquery <- paste("select s.tDate, s.Close, s.Mkt_cap
                      from sim.s01 s
                         where s.Close is not null and s.sCode = '", code, "'", sep="")
    }
    assign(code, sqlQuery(dbcon, paste(loadquery)), dataEnv)
    print(code)
  }
  
  #Close DB connection
  odbcClose(dbcon)
  
  #Convert data to xts format
  for (code in codes) {
    dataEnv[[code]] <- as.xts(dataEnv[[code]][,-1], order.by=as.POSIXct(dataEnv[[code]][,1]))
    #Lagging Except Price
    Close <- Cl(dataEnv[[code]])
    dataEnv[[code]] <- lag(dataEnv[[code]])
    dataEnv[[code]][,'Close'] <- Close
    #Return 
    dataEnv[[code]]$ret <- ROC(Cl(dataEnv[[code]]), type="discrete")
    #Remove all NA data - revise if necessary
    if (na.rm) {
      dataEnv[[code]] <- na.omit(dataEnv[[code]])
    }
  }

  
  #Return
  return((dataEnv))
}