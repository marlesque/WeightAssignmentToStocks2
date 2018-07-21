rm(list=ls())
install.packages("PerformanceAnalytics")
install.packages("zoo")
install.packages("quantmod")
install.packages("lubridate")
install.packages("plyr")
install.packages("pryr")
install.packages("data.table")
install.packages("openxlsx")
install.packages("scales")
install.packages("Quandl")
install.packages("OECD")
install.packages("jpeg")
install.packages("devtools")
install.packages("reshape")
install.packages("reshape2")
install.packages("PortfolioAnalytics")
install.packages("ROI")
install.packages("ROI.plugin.quadprog")
install.packages("ROI.plugin.glpk")
install.packages("ggplot2")
install.packages("DEoptim")
library(PerformanceAnalytics)
library(PerformanceAnalytics)
library(zoo)
library(quantmod)
library(lubridate)
library(plyr)
library(pryr)
library(data.table)
library(openxlsx)
library(scales)
library(Quandl)
library(jpeg)
library(reshape2)
library(ggplot2)
library(xts)
library(PortfolioAnalytics)
library(ROI)
library(DEoptim)
library(ggplot2)
require(ROI.plugin.glpk)
require(ROI.plugin.quadprog)


Sys.setenv("R_ZIPCMD" = "C:/Rtools/bin/zip.exe")
options(scipen=999)

tickers <- c("BAC","AAPL","CSCO","S","T","GE",
             "MSFT","UN","AMD","MDLZ","FCX","VZ")
# compiles a list of the stocks
getSymbols(tickers, from="2007-01-01", to="2017-01-01")
year=c("2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016")
# a list of all the years for the stocks of interests
Adj.clse.prc <- do.call(merge, ly(tickers, function(x) Ad(get(x))))
# stores all the adjusted prices of the stocks

m <- dim(Adj.clse.prc)[1] # number of rows in Adj.clse.prc
n <- length(tickers)  # number of columns in Adj.clse.prc
storageArray=list() # creates a list
r.daily2 <- matrix(,m,n) # creates a m by n matrix

for (i in 1:n) {
  r.daily2[,i] <- dailyReturn(Adj.clse.prc[,i])
  }
#create a matrix all that stores all daily returns of the stocks
r.daily2 <- as.data.frame(r.daily2);
# converts it into a data frame
workbook=createWorkbook()
# creates a workbook
for (i in 1:n) {
  addWorksheet(workbook, sheetName=tickers[i])
  }
# add sheets to the workbook
for (i in 1:n) {
  r.daily=r.daily2[i]
  r.daily$Year <- year(index(Adj.clse.prc[,i]));#adj column and row names
  r.daily$Month <- month(index(Adj.clse.prc[,i]))
  colnames(r.daily)[1]="Return"
  track.record=dcast(data.frame(r.daily), Year~Month, value.var="Return", sum)
  track.record=track.record[,-1]
  colnames(track.record)=month.name
  row.names(track.record)=year
  storageArray[[i]]=track.record
  writeData(workbook, i, paste("Monthly Returns for ", toString(tickers[i])), startCol="B", startRow=2)
  writeData(workbook, i, track.record, startCol="B", startRow=3, rowNames=TRUE)
}
# A loop that takes the daily returns of the stocks and returns the tracking record monthly returns
storageArray=as.data.frame(storageArray)
# converts it into a data frame
weight= 1/(n-1)
# assigns an equal weight to the stocks

portfolio.matrix= matrix(,10,12)
# creates a matrix
for (i in 1:10) {
  for (j in 1:12) {
    sum=0;
    for (k in 1:n) {
      index3=j+(k-1)*12
      sum=sum+weight*storageArray[i,index3]
    }
    portfolio.matrix[i,j]=sum;
  }
}

tickers2 <- c("S&P 500")
# ticker for S&P 500
getSymbols(tickers2, from="2007-01-01", to="2017-01-01")
year=c("2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016")
# a list of all the years for S&P 500
Adj.clse.prc <- do.call(merge, lapply(tickers2, function(x) Ad(get(x))))
r.daily <- dailyReturn(Adj.clse.prc)
r.daily$Year=year(index(r.daily))
r.daily$Month=month(index(r.daily))
colnames(r.daily)[1]="Return"
# same as previous codes

track.record=dcast(data.frame(r.daily), Year~Month, value.var="Return", sum)

SP500matrix=track.record
# extract the tracking record monthly returns for S&P 500
portfolio.matrix=as.data.frame(portfolio.matrix)
# converts it into a data frame
colnames(portfolio.matrix)= month.name
# names the columns
row.names(portfolio.matrix)= year
# names the rows
colnames(SP500matrix)= month.name
# names the columns
row.names(SP500matrix)= year
# names the rows
l=length(year);
m= length(month.name)
column.name=c();
for (i in 1:l) {
  for (j in 1:m) {
    column.name=c(column.name, paste(month.name[j], " ", year[i]))
  }
}
# creates a list of the dates
value= matrix(, 1, 120)
for (j in 1:10) {
  for (i in 1:12) {
    index5= i+(j-1)*12;
    value[index5]= portfolio.matrix[j,i]
  }
}
# creates a list of all the track record monthly returns for the portfolio

value2= matrix(, 1, 120)
for (j in 1:10) {
  for (i in 1:12) {
    index6= i+(j-1)*12;
    value2[index6]= SP500matrix[j,i]
  }
}
# creates a list of all the track record monthly returns for S&P 500
addWorksheet(workbook, sheetName=paste("S&P 500"))
addWorksheet(workbook, sheetName=paste("sheet 1"))
# adds a worksheet
writeData(workbook, n+1, paste("Monthly Returns for S&P 500"), startCol="B", startRow=2)
writeData(workbook, n+1, SP500matrix, startCol="B", startRow=3, rowNames=TRUE)
writeData(workbook, n+2, paste("Monthly Returns for Portfolio"), startCol="B", startRow=2)
writeData(workbook, n+2, value, startCol="B", startRow=3, rowNames=TRUE)
writeData(workbook, n+2, paste("Monthly Returns for S&P 500"), startCol="B", startRow=4)
writeData(workbook, n+2, value2, startCol="B", startRow=5, rowNames=TRUE)
# writes the track record monthly returns of the portfolio and S&P 500 to the spreadsheet
openXL(workbook)
# opens the spreadsheet
saveWorkbook(workbook, "b2100hw2", overwrite = TRUE)
# save the spreadsheet

amazon <- read.csv("AMZN.csv")
apple <- read.csv("AAPL.csv")
facebook <- read.csv("FB.csv")
google <- read.csv("GOOG.csv")
walmart <- read.csv("WMT.csv")
amazon_rev <- ts(amazon[,c(-1, -2, -3, -4, -5, -7)], start=2012-05-20, end= 2017-05-20, frequency=60)
amazon_rev <- data.frame(Date=amazon$Date, Y=amazon$Adj.Close)
amazon_rev <- xts(amazon[,c(-1, -2, -3, -4, -5, -7)], order.by=as.Date(amazon[,1], "%m/%d/%Y"))
returns <- diff(amazon_rev, arithmetic=FALSE ) - 1
row.names(returns) <- amazon$Date
apple_rev <- xts(apple[,c(-1, -2, -3, -4, -5, -7)], order.by=as.Date(apple[,1], "%m/%d/%Y"))
returns_2 <- diff(apple_rev, arithmetic=FALSE) - 1
row.names(returns_2) <- amazon$Date
facebook_rev <- xts(facebook[,c(-1, -2, -3, -4, -5, -7)], order.by=as.Date(facebook[,1], "%m/%d/%Y"))
returns_3 <- diff(facebook_rev, arithmetic=FALSE) - 1
row.names(returns_3) <- amazon$Date
google_rev <- xts(google[,c(-1, -2, -3, -4, -5, -7)], order.by=as.Date(google[,1], "%m/%d/%Y"))
returns_4 <- diff(google_rev, arithmetic=FALSE) - 1
row.names(returns_4) <- amazon$Date
walmart_rev <- xts(walmart[,c(-1, -2, -3, -4, -5, -7)], order.by=as.Date(walmart[,1], "%m/%d/%Y"))
returns_5 <- diff(walmart_rev, arithmetic=FALSE) - 1
row.names(returns_5) <- amazon$Date
data<- data.frame(returns, returns_2, returns_3, returns_4, returns_5)

row.names(data) <- amazon$Date
#data$Apple <- returns
#data$Amazon <- returns_2
#data$Facebook <- returns_3
#data$Google <- returns_4
#data$Walmart <- returns_5
colnames(data) <- c("Apple","Amazon", "Facebook", "Google", "Walmart")
pspec <- portfolio.spec(asset=names)
init <- portfolio.spec(assets=colnames(data))
init <- add.constraint(portfolio=init, type="leverage", min_sum=0, max_sum=1.01)
init <- add.constraint(portfolio=init, type="box", min=0.05, max=0.65)
maxret <- add.objective(portfolio=init, type="return", name="mean")
names <- colnames(data)
workbook=createWorkbook()
addWorksheet(workbook, sheetName="stock returns")

writeData(workbook, 1, data, startCol="B", startRow=3, rowNames=TRUE)
openXL(workbook)
print.default(pspec)
row.names(data) <- amazon$Date
a <- optimize.portfolio(R=data, portfolio=maxret, optimize_method="ROI", trace=TRUE)
minvar <- add.objective(portfolio=init, type="risk", name="var")
b <- optimize.portfolio(R=data, portfolio=minvar, optimize_method="ROI", trace= TRUE)
c <- optimize.portfolio.rebalancing(R=data, portfolio=maxret, optimize_method = "ROI", search_size= 1258, rebalance_on=NULL, training_period=NULL, rolling_window=NULL)
qu <- add.objective(portfolio=init, type="return", name="mean")
qu <- add.objective(portfolio=qu, type="risk", name="var", risk_aversion=0.25)
opt_qu <- optimize.portfolio(R=data, portfolio=qu, optimize_method="ROI", trace=TRUE)
optimize.portfolio(R=data, portfolio = minvar, optimize_method="ROI", trace=TRUE)
