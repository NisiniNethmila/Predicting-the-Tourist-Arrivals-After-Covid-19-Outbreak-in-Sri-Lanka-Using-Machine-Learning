library(zoo)
library(forecast)

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

set.seed(124)

#Neural network
nnar_model = nnetar(arrivals_timeseries)

#testing the predicted data
time_nn_forecast = forecast(nnar_model, h =2*12)
plot(time_nn_forecast)

summary(time_nn_forecast)
