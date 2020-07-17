

# Setting working directory and reading dataset
getwd()
setwd("C:\\Users\\karis\\Documents\\R CODING FILES")
my_data <- read.csv("Population Data.csv", header = TRUE)

# Clear all the variable in work place

rm(list=ls())

#loading necessary libraries

library(forecast)
library(ggplot2)
library(psych)
library(dplyr)
library(tseries)
library(fpp2)

#install.packages("expsmooth")

# Descrpitive abalysis of data

head(my_data)
tail(my_data)
summary(my_data)
str(my_data)
describe(my_data)

# Checking missing values

my_data[is.na(my_data$value)]

        
        
#Declare this as a time series data
        
tso <- ts(my_data[,2],start = c(1952,1),  end = c(2019,12),frequency = 12)
        
        
#-----------------Preliminary Analysis--------------------------------------
        
#Time plot
        
autoplot(tso) +
  ggtitle("US Population Plot")+
  ylab("Number of people")+
  xlab("Years") +
  theme_light()
        
        
# From the plot we can see that data is is non stationary since we can see 
#a growing trend, or its volatility might increase over time (meaning that variance is changing).
        
#Its a positive trend. it has a strong trend. 
#Because it has a strong trend we will investingate the transformation
# Lets take the first diffrence of the data to remove the trend.
        
Diff_tso <- diff(tso)
        
autoplot(Diff_tso) +
ggtitle("US Population Plot")+
ylab("Number of people")+
xlab("Years") +
theme_light()
        
#Series apprear trend - seasonality , use to investigate seasonality

ggseasonplot(Diff_tso)+
  ggtitle("Seasonal plot : US Population")+
  ylab("Numbers")+
  xlab("Months") +
  theme_light()

#Lets look at the other seasonal plot

ggsubseriesplot(Diff_tso)

# Our series tso has trend and seasonality
# To remove trend we first take the difference.

#-----------------------Forcaste with various method----------------

# Use a Bechmark method for forecast.

#----------------Lets use seasonal naive method for forcast.

# Fit the model

fit <- snaive(Diff_tso) # residual SD : 22.7419
print(summary(fit))
checkresiduals(fit)

#-------------- Exponential smoothing method for forecast

fit_ets <- ets(tso) # Residual SD: 20.0386
print(summary(fit_ets))
checkresiduals(fit_ets)

#--------------Fit an ARIMA Model-----------------

# To apply ARIMA Model data has to be stationary
# d =1 ( for regular difference) which tells r before you fitting the ARIMA Model, 
#take the 1st difference of data.
# D = 1 for seasonal difference.


fit_arima <- auto.arima(tso, d=1, D=1, stepwise = FALSE, 
                        approximation = FALSE , trace = TRUE) # Residula SD for this24.684
print(summary(fit_arima))
#After summary take sqrt of sigma^2 to get Residual or SD
checkresiduals(fit_arima)

#----------------------Forecasting----------------------

fcst <- forecast(fit_ets, h = 24  )
autoplot(fcst , include = 120)
print(summary(fcst))
