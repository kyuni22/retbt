####################################################################################################
## Retrieve Data From mysql Database 
## and convert data class in to xts
## factor -> factor to bactest
####################################################################################################
getidhDBdata <- function(DB_info, factor, from='2006-01-01', env = .GlobalEnv){
  if (require(RODBC)) {
    dbcon <- odbcDriverConnect(DB_info)
  }
  
  ## load Data from DB
  # mkt cap
  return_data <- sqlQuery(dbcon, "select * from ftbt_kor.mktcap")
  names(return_data)[NCOL(return_data)] <- "mktCap"
  # mkt cap rank
  load_data <- sqlQuery(dbcon, "select * from ftbt_kor.mktcapsize")
  names(load_data)[NCOL(load_data)] <- "mktCapRank"  
  return_data <- merge(return_data, load_data, by=c("RefDate", "Code"))
  # stock return
  load_data <- sqlQuery(dbcon, "select * from ftbt_kor.stockreturn")
  # lag stock return
  if(require(DataCombine)) {
    load_data <- lapply(lapply(split(load_data, load_data$Code), slide, Var="Value", slideBy=1), na.omit)
  }
  load_data <- do.call(rbind, load_data)
  names(load_data)[(NCOL(load_data)-1):(NCOL(load_data))] <- c("t_ret","ret")
  return_data <- merge(return_data, load_data, by=c("RefDate", "Code"))
  # stock prc
  load_data <- sqlQuery(dbcon, "select * from ftbt_kor.stockprice")
  names(load_data)[NCOL(load_data)] <- "prc"  
  return_data <- merge(return_data, load_data, by=c("RefDate", "Code"))  
  # factor
  load_data <- sqlQuery(dbcon, 
                        paste("select RefDate, Code, FactorValue from ftbt_kor.factor where factorname = '", factor, "'", sep=""))
  names(load_data)[NCOL(load_data)] <- "factor"  
  return_data <- merge(return_data, load_data, by=c("RefDate", "Code"))
#  load_data <- sqlQuery(dbcon, "select * from ftbt_kor.sectorname")  
#  return_data <- merge(return_data, load_data, by=c("RefDate", "Code"))

  #data cleaning
  
  return(return_data)
  
  #Close DB connection
  odbcClose(dbcon)
}