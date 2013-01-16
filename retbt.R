
#load packages
library('RODBC') #Database Access Library
library('quantmod') #quantmode Library
library('PerformanceAnalytics')
library('xtable')
library('Hmisc')

#Load functions
sapply(list.files(pattern="[.]R$", path='functions/', full.names=TRUE), source)

#What Code?
#symbols <- c('A005930','A030200','A055550','A005490','A000660','A010140',
#           'A105560','A000150','A015760','A096770','A036460','A051910',
#           'A029780','A053000','A009540','A066570','A000810','A042670',
#           'A012330','A010620','A005380','A034020','A034220','A006360',
#           'A004940','A000720','A010060','A006260','A000270','A024110',
#           'A028050','A006400','A003550','A009150','A028670','A004020',
#           'A005940','A011070','A001300','A060980','A117930','A004000',
#           'A000240','A012450','A069960','A036570','A001040','A000880',
#           'A023530','A016360','A003490','A000830','A003600','A078930',
#           'A033780','A011170','A042660','A006800','A138930','A009830',
#           'A010950','A035420','A010130','A010130')
symbols <- read.csv("./data/symbols.csv")[,1]

#Set Enviornment
factorEnv <- new.env()

#Setting
initDate <- '2006-01-01'
DB_info <- "Driver={MySQL ODBC 5.1 Driver};DataBase=;Pwd=3450;UID=abcd;Server=localhost;OPTION=3"

#Load Data - Factors
attr_condition <- "Select tDate, Close, Mkt_Cap, PS_12M_FWD from sim.f01 where sCode = "
getSymbols.DB(DB_info, attr_condition, symbols, from=initDate, env=factorEnv)

adj_factor <- "PS_12M_FWD"
factor <- "PS_12M_FWD"
adj <- FALSE

##Add Column
#factor_name <- "PE_12M_FWD"
#Adjusting Factor
#factor <- "EY_12M_FWD"
#allsimdata[[factor]] <- 1/allsimdata[[factor_name]]
#factor <- paste(factor_name,"Mkt_cap", sep="_")
#allsimdata[[factor]] <- allsimdata[[factor_name]] / allsimdata$Mkt_Cap
#####Let do factor adjusting here..

#Make adjut factorEnv
for(symbol in symbols) {
  if(adj) {
    #@@ Adjust Factor
    #Naming Part
    factorEnv[[symbol]]$adj_factor <- rep(NA, nrow(factorEnv[[symbol]]))
    names(factorEnv[[symbol]])[ncol(factorEnv[[symbol]])] <- factor
    #Factor adjust Part
    factorEnv[[symbol]][,factor] <- (factorEnv[[symbol]][,adj_factor]/lag(factorEnv[[symbol]][,adj_factor],4)) - 1
    factorEnv[[symbol]][which((factorEnv[[symbol]][,adj_factor] < 0) | (lag(factorEnv[[symbol]][,adj_factor],4) < 0)), factor] <- NA # NA Negative NI
    #@@ End of Adjusting factor
  }
  #Lagging Except Price -> For backtesting purpose
  Close <- Cl(factorEnv[[symbol]])
  factorEnv[[symbol]] <- lag(factorEnv[[symbol]])
  factorEnv[[symbol]][,'Close'] <- Close
  #Return 
  factorEnv[[symbol]]$ret <- ROC(Cl(factorEnv[[symbol]]), type="discrete")
  #factorEnv[[symbol]]$ret_1W <- lag(factorEnv[[symbol]]$ret) Unnecessary
}
## Note: you can make it fwd return instead of using backward return

#Convert to data.frame
allsimdata <- bt.convertdata(factorEnv)
#Ranking
allsimdata[['Mkt_cap_Rank']] <- bt.rank(allsimdata, 'Mkt_Cap', 'date')
#### Mkt_Cap filtering
MktCap.limit <- 100
allsimdata <- allsimdata[which(allsimdata$Mkt_cap_Rank < MktCap.limit + 1),]

#Making Quantile
sliceNum <- 5
allsimdata <- bt.quantile(allsimdata, factor, sliceNum, name='sig')
  #allsimdata <- bt.quantile(allsimdata, 'ret_1W', 5, name='sig2')

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
worst.col <- 5
index.col <- 6
#@@

# Long/short, Long/Fut
simsum <- cbind(simsum, simsum[,best.col] - simsum[,index.col])
colnames(simsum)[ncol(simsum)] <- 'best-bm'
simsum <- cbind(simsum, simsum[,best.col] - simsum.s[,worst.col])
colnames(simsum)[ncol(simsum)] <- 'best-worst'

#@@ Test setting2
test.col <- best.col
peers.col <- c(1:8)
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
t(table.CalendarReturns(simsum[,c(test.col,peers.col,index.col)]))
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