## --------------------------- ##
## Function takes a vector of  ##
## dates as its input.         ##
##                             ##
## It produces a vector of     ##
## dates that are the first    ##
## in their respective months  ##
## --------------------------- ##

lastDayMonth<-function(x)
{           
  x=as.Date(as.character(x))
  day = format(x,format="%d")
  monthYr = format(x,format="%Y-%m")
  y = tapply(day,monthYr, max)
  last=as.Date(paste(row.names(y),y,sep="-"))
  return(last)
}