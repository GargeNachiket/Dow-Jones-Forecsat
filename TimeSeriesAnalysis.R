rm(list = ls())
##############################################################################################

#Date    : 11/24/2016
#Project : Time Series Analysis
#Author  : Nachiket Garge

##############################################################################################
library(TTR)
library(forecast)

# Reading the data in R

tsf <- read.csv("C:/Users/Nachiket Garge/Downloads/R/Proj Time Series/timeseriesdata_to_forecast.csv")
View(tsf)
tsf1 = ts(tsf$DJI_Closing_value,frequency = 12,start = c(1987,1))
tsf1

plot.ts(tsf1)

#######################################################################################################
# Seasonality does not appear to vary much with time thus I did not apply log function 


SMADowJones = SMA(tsf1,n = 5)
plot.ts(SMADowJones)

# Moving average smoothing with order 5 shows the general trend of the Curve
# It's an increasing curve with a few dips that we can analyze later 

# Checking Time Series Components of the file

decompose_tsf = decompose(tsf1) 
plot(decompose_tsf)

# We can observe clear trend, seasonal components and random noise component of the data

# Let's try to model this

# Fitting exponential Model using Holt Winter Model keeping all 3 variables 
# alpha,beta and gamma

DowJonesForecast = HoltWinters(tsf1)
DowJonesForecast

# High value of aplha and gamma indicate that the series estimate of level and
# seasonal values highly depend of the recent values than older ones
# low value of b indicates that trend does not changes it's direction much

DowJonesForecast$SSE
# SSE is 58473045

plot(DowJonesForecast)
# Plot shows pretty accurate forecast of smoothing model
# STill we need to check error performance and forecast stability

DowJonesForecast2 = forecast.HoltWinters(DowJonesForecast,h = 24)
plot(DowJonesForecast2)
# This plot shows forecast of next 24 months of dowjones along with 80% and 95% CONFIDENCE INTERVALS 

acf(DowJonesForecast2$residuals,lag.max = 20)
# Checking residual auto correlation for 20 lags
Box.test(DowJonesForecast2$residuals,lag = 20,type = "Ljung-Box")

# Box-Ljung test

# data:  DowJonesForecast2$residuals
# X-squared = 24.376, df = 20, p-value = 0.2263

# Except a few all acf lines were less than 95% confidence limits
# As p value is high we can say all errors are independent of each other for 20 lags

plot.ts(DowJonesForecast2$residuals)
# Errors are evennly distributed but increasing over time

plot(density(DowJonesForecast2$residuals))
# We can see errors are normally distributed except a long tail in negative side

#############################################################################################
# Plotting different ARIMA models

# Let's analyze first the effect of differencing on the time series

tsfdiff = diff(tsf1,differences = 1)
plot.ts(tsfdiff)

# differencing has a significant effect on trend and time series looks better 
# Let's observe the results of acf and pacf for 20 lags

acf(tsfdiff,lag.max = 20)
pacf(tsfdiff,lag.max = 20)
#Beer Time Yay!!

tsfforecast = arima(tsf1,order = c(0,1,0))
tsfforecast

tsfforecast2 = forecast.Arima(tsfforecast,h=24)
plot.forecast(tsfforecast2)
acf(tsfforecast2$residuals)
pacf(tsfforecast2$residuals)
plot.ts(tsfforecast2$residuals)
plot(density(tsfforecast2$residuals))

########################################################################################################

