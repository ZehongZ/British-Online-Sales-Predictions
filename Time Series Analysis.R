#Package&Source
source("DSC425-Util.R")
library("ggplot2")
library(tseries)
library(fBasics)
library(zoo)
library(lmtest)
library(forcats)

#Create time opject
intel$Date = as.Date(intel$Date, "%m/%d/%Y")
# Creates time series object using zoo package
ratets = zoo(x=rate, as.Date(as.character(myd$date), format = "%m/%d/%Y"))
# Creates time series object using zoo package
ratets = ts(rate, start=c(2001,7), freq=12)
plot(ratets)

#White Noise
whitenoise = rnorm(1000, 0, .1)#.1 means standard deviation
plot(whitenoise, type="l")
#add circles on data points in the time plot
points(y)
ggplot(intel, aes(x=Date, y=Adj.Close)) + geom_line(color="black")


# Compute the series lagged by 1 (actually it is -1, but we know 
# that the autocorrelation is the same for lag 1 & -1!).  We do this by 
# removing the first element of the series.  Thus rw1[0] = rwalk[1],
# rw1[1] = rwalk[2], etc.  
rw1 = whitenoise[-1]
cor(whitenoise[-length(whitenoise)], rw1)
rw2 = whitenoise[-1:-2]
cor(whitenoise[-(length(whitenoise)-1):-length(whitenoise)], rw2)
rw3 = whitenoise[-1:-3]
cor(whitenoise[-(length(whitenoise)-2):-length(whitenoise)], rw3)

#CREATE HISTOGRAM
# creates 2 by 2 display for 4 plots
par(mfcol=c(1,1)) 
hist(ratets, xlab="Unemployment rates", prob=TRUE, main="Histogram")
# add approximating normal density curve
xfit<-seq(min(rate),max(rate),length=40)
yfit<-dnorm(xfit,mean=mean(rate),sd=sd(rate))
lines(xfit, yfit, col="blue", lwd=2) 

#CREATE NORMAL PROBABILITY PLOT
qqnorm(rate)
qqline(rate, col = 2) 

#CREATE TIME PLOT 
# use time series object lnatts to draw time plot indexed with time
plot(ratets, type='l', xlab='time', ylab='Weekly change rates for oil prices')

# The lag plot
lag.plot(whitenoise)

#Compute the auto-correlation function
acf(whitenoise, lag.max = 20)

# plots pacf values up to lag 15. 
pacf(rate, lag = 15)

# creates 2 by 1 display for 2 plots
par(mfcol=c(2,1)) 
#plots acf (correlogram)
acf(rate, plot=T, lag=20)
# plots pacf values up to lag 15. 
pacf(rate, lag = 15)

# Perform a Ljung-Box test on the data to test for significant autocorrelation
Box.test(whitenoise, lag=10, type = "Ljung-Box")

# So, we check this with Dickey-Fuller, and we certainly cannot reject
# non-stationarity.  The first function here always tests for both drift
# and time-dependent trend effects
loadPkg("tseries")
adf.test(lPass, alternative ="stationary", k=12)

# This one is the most flexible because it gives us control over both the
# lags and the version of the Dickey-Fuller test we want to use.
loadPkg("fUnitRoots")
adfTest(lPass, lags=12, type="nc")  # Bare test for unit root
adfTest(lPass, lags=12, type="c")   # Is it stationary after subtracting drift
adfTest(lPass, lags=12, type="ct")  # Is it stationary after subtracting drift and trend

# compute seasonal difference for quarterly data (s=4)
sdy=diff(dy,4)
# create acf plot 
acf(as.vector(sdy),lag.max=16, main="ACF of DSDX log starts")

# The package "forecast" has a nsdiffs function which applies multiple root 
# tests and tells us how many differences are needed in an Arima
nsdiffs(lPass)

#Build ARMA (1,1) Model
p = rep(0, 1000)
p[1] = 0
phi_1 = .8  
theta_1 = .5  
for (i in 2:1000) 
{
  p[i] = phi_1 * p[i-1] + a[i] + theta_1 * a[i-1]
}

# Fit an ARIMA model to a univariate time series.
# arima(x, order = c(p, 0, q))
# include.mean=F to remove intercept that is not significant
m1= arima(rate, order=c(8,0,0), method='ML', include.mean=T)
m1

#  FIT AR(8) Model without lag 4,5,6,7 parameters
#
# Use fixed option in arima function to fix parameter values,
# where 0 means fixing the model coefficient to be removed, 
# and NA means parameters to be fitted. The ordering of the paramter 
# can be found using m2$coef. 
m2= arima(rate, order=c(8,0,0), fixed=c(NA,NA, NA, 0,0,0,0,NA), include.mean=F)
coeftest(m2)

# fit multiplicative seasonal model ARIMA(1,1,1)?(1,1,1)4
m1=arima(y, order=c(1,1,1),seasonal=list(order=c(1,1,1),period=4), method="ML") 
m1

# Now, let's see if we can reconstruct a forecast for the original series = e^lPass
f = forecast(fit, h=20)
f$x                     # Holds the original series
f$mean                  # Holds the forecasted mean for the 20 time points

# Now, plot the exponential of these
plot(exp(f$x), xlim=c(1950,1965), ylim=c(100, 700))
lines(exp(f$mean), col="red")

# AUTO.ARIMA SELECTS ARMA(P,Q) MODEL BASED ON AIC OR BIC
library(forecast)
# optimal w.r.t. BIC criterion
auto.arima(rate, max.P=8, max.Q=8, ic="bic")
# optimal w.r.t. AIC criterion 
auto.arima(rate, max.P=8, max.Q=8, ic="aic")

# A stationary series.  Also, we get exponential falloff in both
# the acf and the pacf.
plot(p, type="l")
lag.plot(p)
acf(p, lag.max = 100)
pacf(p, lag.max = 30)

# T-tests on coefficients
library(lmtest)
coeftest(m1)

#Test Residual
fit = arima(p, order=c(1, 0, 1))
print(fit)
plot(fit$residuals)
acf(fit$residuals)
Box.test(fit$residuals, lag=10, type = "Ljung-Box")
plot(m4$resid/sqrt(m4$sigma))

# COMPUTE PREDICTIONS
# predict(object, n.ahead = 1, newxreg = NULL, se.fit = TRUE, ...)
#	object is the result of an arima fit.
#	n.ahead is the number of steps ahead for which prediction is required.
#	se.fit = T --> standard errors of forecasts are printed
pr=predict(m4,n.ahead=10, se.fit=T)
pr

#Forecast.Arima(model) 
library(forecast)
forecast.Arima(m4, h=5)

#PLOT PREDICTIONS FOR 10 STEPS AHEAD 
plot(forecast.Arima(m4, h=5), include=50)

# APPLY EACF MODEL SELECTION PROCEDURES
#use TSA package
library(TSA)
#compute eacf values for p<=6 and q<=6
m1=eacf(rate, 8,8)
names(m1)
print(m1$eacf, digits=2)

# use arimaPrint() function in arimaPrint.R file 
source("arimaPrint.R")
arimaPrint(m1)
# One plot per page 
par(mfcol=c(1,1)) 
# plot residual analysis for model checking
tsdiag(m1) 
# "approximated" ljung box test on residuals
Box.test(m1$residuals, 6, "Ljung-Box") 

Box.test(m1$residuals, 12, "Ljung-Box") 

# compute predictions for up to 16-step aheads
f1=predict(m1,4)
names(f1)
f1

# creates forecast plot for logged variable
# join data with forecasts
# s2011 contains new observations not used for model estimation.
# observations are from Q4-2010 to Q3-2011
s2011 = c(y, log(c(96, 90, 123, 117)))
s1=c(y,f1$pred) 
# computes lower limit for 95% interval
lcl=c(y,f1$pred-2*f1$se) 
# computes upper limit for 95% interval
ucl=c(y,f1$pred+2*f1$se) 
# Forecast plot
plot(s1,type='l',col='red', xlim=c(100,150), ylim=c(4,7)) 
points(s2011)
lines(1:length(s1),ucl,lty=2)
lines(1:length(s1),lcl,lty=2)

##############################################
## BACKTESTING #################
##############################################

# Use backtesting procedures to compare models
# download backtest.R file in work directory
# load file and function
# input parameters of backtest(m1, orig=o, rt=r, h=h, fixed=c())
# m1: is a time-series model object
# orig: is the starting forecast origin
# rt: the time series
# xre: the independent variables
# h: forecast horizon
# fixed: parameter constraint
# inc.mean: flag for constant term of the model.
#
ntest=round(length(rate)*0.8)  #size of testing set
source("backtest.R")
pm2 = backtest(m2, rate, ntest, 1, fixed=c(0,NA, NA, NA, NA,NA))
pm3 = backtest(m3, rate, ntest, 1,inc.mean=F)