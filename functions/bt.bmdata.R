####################################################################################################
## Retrieve Data From Database 
## and convert data class in to xts
## factor -> factor to bactest
####################################################################################################
bt.bmdata <- function(bm){
  dbcon <- odbcDriverConnect('Driver={MySQL ODBC 5.1 Driver};DataBase=;Pwd=3450;UID=abcd;Server=localhost;OPTION=3')
  
  code <- switch(bm, KOSPI200="IKS200", KOSPI="IKS001", KM1="KSFA020")
    
  #load Data from DB
  loadquery <- paste("select s.tDate, s.Close
                  from sim.s02 s
                     where s.sCode = '", code, "'", sep="")
  bm <- sqlQuery(dbcon, paste(loadquery))
  
  #Close DB connection
  odbcClose(dbcon)
  
  #Convert data to xts format
  bm <- as.xts(bm[,-1], order.by=as.POSIXct(bm[,1]))
  colnames(bm) <- 'Close'
  #Return 
  bm$ret <- ROC(Cl(bm), type="discrete")
  
  #Return
  return((bm))
}