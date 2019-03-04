#Package&Source
source("DSC425-Util.R")
library(ggplot2)
library(tseries)
library(fBasics)
library(zoo)
library(lmtest)
library(forcats)
library(TSA)
library(adfExplorer)
library(forcats)
library(fUnitRoots)
library(lubridate)
library(sqldf)
library(xts)
library(highfrequency)

#Import dataset
myd=read.csv("Online Retail copy by mins.csv", header = T)
head(myd)

myd <- na.omit(myd)
#Format time
myd$Minutes=strptime(myd$Minutes, "%m/%d/%y %H:%M")
head(myd$Minutes,100)

#Format sales
myd$Total.Sales=xts(myd$Total.Sales, order.by = myd$Minutes)
head(myd$Total.Sales)
dim(myd$Total.Sales)
tsales <- aggregatets(myd$Total.Sales, on="minutes", k=720)
head(tsales)
print(tsales)

#Time Series Plot
plot(tsales,type='l')
test <- decompose(tsales)
tsales <- aggregatets(myd$Total.Sales, on="minutes", k=300)
diff.tsales <- diff(tsales)
plot(diff.tsales,type='l')

#ACF&PACF Plot
acf(tsales)
pacf(tsales)
diff_tsales <- diff(tsales)
acf(diff_tsales,na.action = na.omit)
pacf(diff_tsales,na.action = na.omit)

#Eacf plot
eacf(tsales)
eacf(diff.tsales,na.action = na.omit)

#tsales model
m1=arima(tsales, order=c(6,0,0))
coeftest(m1)
source("backtest.R")
ntest=round(length(myd$Sales)*0.8)
bm1=backtest(m1,myd$Sales, ntest, 1,inc.mean=F)
acf(m1$residuals, main="tsales Model Residual")
pacf(m1$residuals, main="tsales Model residual")
Box.test(m1$residuals, lag=10, type = "Ljung-Box")

#Auto arima tsales model
library(forecast)
coeftest(auto.arima(tsales))
m2=arima(tsales, order=c(1,0,3), fixed=c(NA,NA,0,NA,NA))
coeftest(m2)
acf(m2$residuals,main="autotsales Model Residual")
pacf(m2$residuals,main="autotsales Model Residual")
Box.test(m2$residuals, lag=10, type = "Ljung-Box")

#Forecast
library(forecast)
library(tseries)
forecast.Arima(m1,n.ahead=10)

#Correlation
cor(myd$Total.Sales, myd$Quantity)
myd$Total.Sales=xts(myd$Total.Sales, order.by = myd$Minutes)
myd$Quantity=xts(myd$Quantity,order.by=myd$Minutes)
tsales <- aggregatets(myd$Total.Sales, on="minutes", k=720)
tquantity <- aggregate(myd$Quantity, on="minutes" ,k=720)
plot(myd$Quantity,type="l")
cor(myd$Total.Sales,myd$Quantity)
cor(myd$Total.Sales,as.numeric(myd$InvoiceDate))
cor(myd$Total.Sales, myd$UnitPrice)
cor(myd$Total.Sales,myd$Country)
library(gtools)
running(myd$Total.Sales,myd$Quantity,fun=cor,width = 5,allow.fewer=TRUE, align=c("right"), simplify=TRUE)
ccf(myd$Total.Sales,myd$Quantity,main="")
