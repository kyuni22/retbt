
#load packages
library('RODBC') #Database Access Library
library('quantmod') #quantmode Library
library('PerformanceAnalytics')
library('xtable')
library('Hmisc')
library('reshape')
library('DataCombine')

#Load functions
sapply(list.files(pattern="[.]R$", path='functions/', full.names=TRUE), source)

##### initDate <- '2006-01-01'
DB_info <- "Driver={MySQL ODBC 5.1 Driver};DataBase=;Pwd=1234;UID=abcd;Server=175.17.62.121;OPTION=3"

#Load Data
load_data <- getidhDBdata(DB_info, factor="PER_Fwd_1M_BASED")

#get Simulation dates
sim_dates <- as.Date(as.character(unique(load_data[,1])), format="%Y%m%d")
rebal_dates <- lastDayMonth(sim_dates)
rebal_dates <- as.integer(format(rebal_dates, "%Y%m%d"))

## Simulation procedure
# remove mkt_cap under cetain level (ex. 200)
sim_data <- load_data[which(load_data$mktCapRank < 200+1),]
# preprocess factor data
# -> Please lag for real backtest --> develop later
rebal_data <- sim_data[which(sim_data$RefDate %in% rebal_dates),]
rebal_data$factor <- ifelse(rebal_data$factor > 0, 1/rebal_data$factor, ifelse(rebal_data$factor < -4, -4, rebal_data$factor))
# make quantile for each rebalancing date
rebal_data <- bt.quantile(rebal_data, sim.factor="factor", by.factor="RefDate", n=5)
# get number of shares for each date, each sig
sig_count <- tapply(rebal_data$Code, list(rebal_data$RefDate, rebal_data$sig), NROW)
sig_count <- melt(sig_count)
colnames(sig_count) <- c("RefDate", "sig", "sig_count")
rebal_data <- merge(rebal_data, sig_count, by=c("RefDate", "sig"))
rebal_data$ratio <- 1/rebal_data$sig_count/rebal_data$prc
# merge with daily return with quantile score
sim_data <- merge(sim_data[,c("RefDate", "Code", "t_ret", "ret", "prc")], rebal_data[,c("RefDate", "Code", "sig", "ratio")], by=c("RefDate", "Code"), all.x=TRUE)
sim_data <- Filter(function(x) NROW(x) > 0, split(sim_data, sim_data$Code)) # remove empty list for na.locf
sim_data <- lapply(lapply(sim_data, na.locf), na.omit) # run na.locf and omit NAs
#######sim_data <- Filter(function(x) NROW(x) > 0, sim_data) # remove empty list
########sim_data <- lapply(lapply(lapply(lapply(sim_data, slide, Var="sig", slideBy=-1), slide, Var="ratio", slideBy=-1), slide, Var="prc", slideBy=-1), na.omit)
sim_data <- do.call(rbind, sim_data) # make it as data.frame
# data cleaning
sim_data$ret <- as.numeric(sim_data$ret)/100 # change chr data to numeric
sim_data$ratio <- as.numeric(sim_data$ratio)*as.numeric(sim_data$prc) # change chr data to numeric
#sim_data$pre_prc <- as.numeric(sim_data[,c("prc-1")]) # change chr data to numeric
sim_data$ret <- sim_data$ret*sim_data$ratio
# run simulation
sim_data <- tapply(sim_data$ret, list(sim_data$RefDate, sim_data$sig), sum, na.rm=TRUE)
sim_dates <- as.Date(rownames(sim_data), format="%Y%m%d")
rownames(sim_data) <- rep("", NROW(sim_data))
colnames(sim_data) <- make.names(colnames(sim_data))
sim_data <- xts(sim_data, sim_dates)
# Chart Performance
charts.PerformanceSummary(sim_data[,c(5,1:4)], geometric=FALSE, wealth.index=TRUE)



#Convert to data.frame
allsimdata <- bt.convertdata(factorEnv)
#Ranking
allsimdata[['Mkt_cap_Rank']] <- bt.rank(allsimdata, 'Mkt_cap', 'date')
#### Mkt_Cap filtering
MktCap.limit <- 100
allsimdata <- allsimdata[which(allsimdata$Mkt_cap_Rank < MktCap.limit + 1),]

#Making Quantile
if(DoQuantile) {
  sliceNum <- 5
  allsimdata <- bt.quantile(allsimdata, factor, sliceNum, name='sig')
  #allsimdata <- bt.quantile(allsimdata, 'ret_1W', 5, name='sig2')
}

################## Re-run from here to adjust test.col
# Mean of categorized return
simsum <- bt.simreturn(allsimdata, 0.003)
simsum.s <- bt.simreturn(allsimdata, -0.003) # Return for Short! 
  ## Note: Tax Cost should be negative to be substracted later

# Benchmark
bmname <- 'KM1'
bm <- bt.bmdata(bmname)[-1,]
bm <- bm[index(bm) %in% index(factorEnv$A005930),]
bm$ret <- ROC(Cl(bm), type="discrete")
simsum <- cbind(simsum, bm$ret)

# Setting names
colnames(simsum) <- make.names(colnames(simsum))
colnames(simsum)[ncol(simsum)] <- bmname

#@@ Test Setting
best.col <- 1
worst.col <- 3
index.col <- 5
#@@

# Long/short, Long/Fut
simsum <- cbind(simsum, simsum[,best.col] - simsum[,index.col])
colnames(simsum)[ncol(simsum)] <- 'best-bm'
simsum <- cbind(simsum, simsum[,best.col] - simsum.s[,worst.col])
colnames(simsum)[ncol(simsum)] <- 'best-worst'

#@@ Test setting2
test.col <- best.col
peers.col <- c(1:7)
peers.col <- peers.col[!(peers.col %in% c(test.col, index.col)) ]
#@@
#@@ Period Setting
#simsum <- simsum['/2012-06']
#@@

# Rolling Setting
simsum.length <- dim(simsum)[1]
trailing1y.rows <- ((simsum.length - 51):simsum.length)
trailing3y.rows <- ((simsum.length - 103):simsum.length)

# Change to Monthly Return for table <- WARNING!! This might be wrong.
simsum.month <- {}
for(i in 1:ncol(simsum)) {
  simsum.month <- cbind(simsum.month, apply.monthly(simsum[,i],sum))
}

############# to here

filename <- paste("./reports/data/simsum_", factor, ".csv", sep="")
write.zoo(simsum, file=filename, sep=",")
filename <- paste("./reports/data/simsum.month_", factor, ".csv", sep="")
write.zoo(simsum.month, file=filename, sep=",")
filename <- paste("./reports/data/allsimdata_", factor, ".csv", sep="")
write.zoo(allsimdata, file=filename, sep=",")

## For viewing on R console
# 5yr Performance Analysis
charts.PerformanceSummary(simsum[,c(test.col,peers.col,index.col)], main='Cummulative Performance', geometric=TRUE, wealth.index=TRUE, ylog=TRUE, event.labels=TRUE)
charts.PerformanceSummary(simsum[,c(test.col,peers.col,index.col)], main='Simple Performance', geometric=FALSE, wealth.index=TRUE, event.labels=TRUE)
chart.RelativePerformance(simsum[,c(test.col,peers.col)], simsum[,index.col], legend.loc='topleft', ylog=TRUE)
chart.RelativePerformance(simsum[,c(test.col)], simsum[,peers.col], legend.loc='topleft', ylog=TRUE)
chart.Boxplot(simsum[,c(test.col,peers.col,index.col)])
chart.RiskReturnScatter(simsum[,c(test.col,peers.col,index.col)])

# 3yr Performance Analysis
charts.PerformanceSummary(simsum[trailing3y.rows,c(test.col,peers.col,index.col)], main='Cummulative Performance', geometric=TRUE, wealth.index=TRUE, ylog=TRUE, event.labels=TRUE)
charts.PerformanceSummary(simsum[trailing3y.rows,c(test.col,peers.col,index.col)], main='Simple Performance', geometric=FALSE, wealth.index=TRUE, event.labels=TRUE)
chart.RelativePerformance(simsum[trailing3y.rows,c(test.col,peers.col)], simsum[trailing3y.rows,index.col], legend.loc='topleft', ylog=TRUE)
chart.RelativePerformance(simsum[trailing3y.rows,c(test.col)], simsum[trailing3y.rows,peers.col], legend.loc='topleft', ylog=TRUE)
chart.Boxplot(simsum[trailing3y.rows,c(test.col,peers.col,index.col)])
chart.RiskReturnScatter(simsum[trailing3y.rows,c(test.col,peers.col,index.col)])

# 1yr Performance Analysis
charts.PerformanceSummary(simsum[trailing1y.rows,c(test.col,peers.col,index.col)], main='Cummulative Performance', geometric=TRUE, wealth.index=TRUE, ylog=TRUE, event.labels=TRUE)
charts.PerformanceSummary(simsum[trailing1y.rows,c(test.col,peers.col,index.col)], main='Simple Performance', geometric=FALSE, wealth.index=TRUE, event.labels=TRUE)
chart.RelativePerformance(simsum[trailing1y.rows,c(test.col,peers.col)], simsum[trailing1y.rows,index.col], legend.loc='topleft', ylog=TRUE)
chart.RelativePerformance(simsum[trailing1y.rows,c(test.col)], simsum[trailing1y.rows,peers.col], legend.loc='topleft', ylog=TRUE)
chart.Boxplot(simsum[trailing1y.rows,c(test.col,peers.col,index.col)])
chart.RiskReturnScatter(simsum[trailing1y.rows,c(test.col,peers.col,index.col)])

# Perfromance Table for alltime
t(table.CalendarReturns(simsum.month[,c(test.col,peers.col,index.col)]))
table.AnnualizedReturns(simsum[,c(test.col,peers.col,index.col)])
table.Stats(simsum[,c(test.col,peers.col,index.col)])
#Charts for Stats
# layout(rbind(c(1,2),c(3,4)))
chart.Histogram(simsum[,test.col], main = "Plain", methods = NULL)
chart.Histogram(simsum[,test.col], main = "Density", breaks=40,methods = c("add.density", "add.normal"))
chart.Histogram(simsum[,test.col], main = "Skew and Kurt", methods = c("add.centered", "add.rug"))
chart.Histogram(simsum[,test.col], main = "Risk Measures", methods = c("add.risk"))

#Downside Risk
table.DownsideRisk(simsum[,c(test.col,peers.col,index.col)])
table.Drawdowns(simsum[,test.col], top=10)
#Want to know hitratio..too..

#To Drawdown print
result.drawdown <- table.Drawdowns(simsum[,test.col], top=10)
result.drawdown$From <- as.character(result.drawdown$From)
result.drawdown$Trough <- as.character(result.drawdown$Trough)
result.drawdown$To <- as.character(result.drawdown$To)

#Generate Document
Sweave("PerformanceReport", encoding="UTF-8", syntax="SweaveSyntaxNoweb")
filename <- paste("allsimdata_", factor, ".csv", sep="")
write.csv(allsimdata, filename)
filename <- paste("simsum_", factor, ".csv", sep="")
write.zoo(simsum, file=filename, sep=",")