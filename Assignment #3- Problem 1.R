#Package&Source
source("DSC425-Util.R")
library("ggplot2")
library(tseries)
library(fBasics)
library(zoo)
library(lmtest)
library(TSA)

#Import data
myd=read.csv('Online Retail.csv', header=T)

#Check firsr six rows
head(myd)

#Create sales
myd$Sales=myd$UnitPrice*myd$Quantity  
head(myd$Sales)
myd$InvoiceDate=as.Date(myd$InvoiceDate,"%m/%d/%Y")
head(myd)
myd$Sales=as.numeric(myd$Sales)
dim(myd)
#Select the sales and quantity that > 0
positive=subset(myd, Sales>0 & Quantity>0)
dim(positive)
#Plot
ggplot(positive, aes(x=positive$InvoiceDate, y=positive$Sales)) + geom_line(color="black")+ylim(c(0,500))+xlab("Invoice Date")+ylab("Sales")

acf(positive$Sales, lag=50, main="Sales")

pacf(positive$Sales, lag=50, main="Sales")       

eacf(myd$Sales,10,10)

# AUTO.ARIMA SELECTS ARMA(P,Q) MODEL BASED ON AIC OR BIC
library(forecast)
# optimal w.r.t. BIC criterion
auto.arima(positive$Sales, max.P=8, max.Q=8, ic="bic")
# optimal w.r.t. AIC criterion 
auto.arima(positive$Sales, max.P=8, max.Q=8, ic="aic")

m1= arima(myd$Sales, order=c(5,0,2), method='ML', include.mean=T)
coeftest(m1)

m2= arima(myd$Sales, order=c(5,0,3), method='ML', include.mean=T)
coeftest(m2)

loadPkg("fUnitRoots")
adfTest(positive$Sales, lags=50, type="nc")  # Bare test for unit root
adfTest(positive$Sales, lags=50, type="c")   # Is it stationary after subtracting drift
adfTest(positive$Sales, lags=50, type="ct")  # Is it stationary after subtracting drift and trend

library(forecast)
# optimal w.r.t. BIC criterion
auto.arima(positive$Sales, max.P=8, max.Q=8)
m1=arima(positive$Sales, order=c(1,0,2))
coeftest(m1)

acf(m1$residuals, main="Series Residuals", lag.max = 50)
pacf(m1$residuals, main="Series Residuals")
normplot(m1$residuals)
Box.test(m1$residuals, lag = 10,ctype = "Ljung-Box")

ntest=round(length(positive$Sales)*0.8)  #size of testing set
source("backtest.R")
pm3 = backtest(m1, positive$Sales, ntest, 1,inc.mean=F)
