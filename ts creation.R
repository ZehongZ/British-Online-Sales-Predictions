library(forecast) 
library(tseries)
library(lubridate)
library(stringi)
options(scipen=999) #turn off scientific notation
test <- X1
head(test)
OR1 <- test[,c("Date","TotalPrice")]
OR1 <- na.omit(OR1)
OR1$Date <- as.Date(OR1$Date, format = "%m-%d-%y")
dayOfYear = as.numeric(format(OR1$Date, "%j"))
head(OR1)
plot.ts(OR1)
tsales <- ts(OR1$TotalPrice, start = c(2010,335), end = c(2011,343),frequency = 360)
head(tsales)
plot.ts(tsales)