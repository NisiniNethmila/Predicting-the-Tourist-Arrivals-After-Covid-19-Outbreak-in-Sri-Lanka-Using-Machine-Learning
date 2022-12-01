#load the required libraries
library(caret)
library(forecast)
library(tseries)

#load the data set
arrivals <- read.csv("tourist_arrivals.csv")
head(arrivals)
str(arrivals)

#-----check NA values--------

#check NA values in arrivals
any(is.na(arrivals))


#splitting arrivals data frame into training and testing
arrivals_train <- arrivals[1:516, ]
arrivals_test <- arrivals[517:540, ]

#convert to time series format
arrivals_timeseries <- ts(arrivals_train$Tourist_Arrivals, start = c(1977,1),
                          end = c(2019,12), frequency = 12)

#arima model
arrivals_model <- auto.arima(arrivals_timeseries)
arrivals_model

summary(arrivals_model)

#testing the predicted data
forecasting_arima = forecast(arrivals_model, h=2*12)
summary(forecasting_arima)
plot(forecasting_arima)


