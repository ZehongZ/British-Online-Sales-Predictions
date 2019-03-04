#Package&Source
source("DSC425-Util.R")
library(ggplot2)
library(tseries)
library(fBasics)
library(zoo)
library(lmtest)
library(forecast)
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

#Format time
myd$Minutes=strptime(myd$Minutes, "%m/%d/%y %H:%M")
head(myd$Minutes,100)

#Distirbution plot
hist(myd$Total.Sales)
qqnorm(myd$Total.Sales)

#Zoo packages
ZooSales=zoo(myd$Total.Sales, order.by =myd$Minutes)
plot(ZooSales, type="l")

#Time Sereis packages
Tsales=ts(myd$Total.Sales,start=c(2011,12), frequency = 2880)
ggplot(myd, aes(x=myd$Minutes, y=Tsales)) + geom_line(color="black")


#High Frequennce
myd$Total.Sales=xts(myd$Total.Sales, order.by = myd$Minutes)
na.omit(myd$Minutes)
head(myd$Total.Sales)
dim(myd$Total.Sales)
tsales <- aggregatets(myd$Total.Sales, on="minutes", k=60)
head(tsales)
print(tsales)

#Log transformation
logSales=log(myd$Total.Sales)
head(logSales,1000)

#plot logSales
hist(logSales)
qqnorm(logSales, ylim=c(0,10000))

#Format logSales
head(myd$Minutes)
logSales=ts(logSales,start=c(2010-12-01, 08:26:00),end=c(2011-12-9), frequency = 720)
plot(logSales)
