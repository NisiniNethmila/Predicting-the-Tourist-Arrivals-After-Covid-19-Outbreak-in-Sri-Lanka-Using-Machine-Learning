#load the data set
toursit_arrivals <- read.csv("tourist_arrivals.csv")

head(toursit_arrivals)
tail(toursit_arrivals)
str(toursit_arrivals)

#keep only 3rd column and remove others 
toursit_arrivals_num <- toursit_arrivals[-c(1,2)]

str(toursit_arrivals_num)



#---------------------------input vector--------------------------

#for 5 months interval standard deviation values
five_months_sd <- by(toursit_arrivals_num$Tourist.Arrivals, ceiling(1:168 / 5), sd)

#for 4 months interval standard deviation values
four_months_sd <- by(toursit_arrivals_num$Tourist.Arrivals, ceiling(1:168 / 4), sd)

#for 3 months interval standard deviation values
three_months_sd <- by(toursit_arrivals_num$Tourist.Arrivals, ceiling(1:168 / 3), sd)

#for 2 months interval standard deviation values
two_months_sd <- by(toursit_arrivals_num$Tourist.Arrivals, ceiling(1:168 / 2), sd)


#---------rolling window standard deviation----------

#load the required library
library(roll)

toursit_arrivals_num_1 <- as.matrix(toursit_arrivals_num)

#rolling standard deviation
rolling_sd_five <- roll_sd(toursit_arrivals_num_1, 5)
rolling_sd_four <- roll_sd(toursit_arrivals_num_1, 4)
rolling_sd_three <- roll_sd(toursit_arrivals_num_1, 3)
rolling_sd_two <- roll_sd(toursit_arrivals_num_1, 2)

#create a matrix 
toursit_arrivals_num_new <- cbind(rolling_sd_five, rolling_sd_four, rolling_sd_three,
                                  rolling_sd_two, toursit_arrivals_num$Tourist.Arrivals)

#create a data frame
toursit_arrivals_num_df <- data.frame(toursit_arrivals_num_new)

#rename all the column names
names(toursit_arrivals_num_df) <- c('Int sd 5m arrivals', 'Int sd 4m arrivals',
                                    'Int sd 3m arrivals', 'Int sd 2m arrivals',
                                    'Tourist.Arrivals')
names(toursit_arrivals_num_df)

summary(toursit_arrivals_num_df)

#-----check NA values--------

#check NA values in toursit_arrivals_num_df
any(is.na(toursit_arrivals_num_df))

#--------remove NA values------------

#remove NA values from toursit_arrivals_num_df
tour_arrivals_df_clean <- na.omit(toursit_arrivals_num_df)

summary(tour_arrivals_df_clean)
str(tour_arrivals_df_clean)


#-----------------------------normalization-----------------------

#normalize function
normalize <- function(x){
  return((x - min(x)) / (max(x) - min(x)))
}

#applying the min-max normalization for the tour_arrivals_df_clean
tour_arrivals_df_clean_nor <- as.data.frame(lapply(tour_arrivals_df_clean, 
                                                   normalize))

summary(tour_arrivals_df_clean_nor)


#-------------Implementation of MLPs----------------

#load the required libraries
library(neuralnet)
library(ggplot2)


#------Models for data frame of adopted input vector type -------------

set.seed(234)

#splitting tour_arrivals_df_clean_nor data frame into training and testing
tour_arr_df_clean_nor_train <- tour_arrivals_df_clean_nor[1:144, ]
tour_arr_df_clean_nor_test <- tour_arrivals_df_clean_nor[145:164, ]

#build the neural network model 1
tour_arr_df_clean_nor_model_1 <- neuralnet(Tourist.Arrivals ~ Int.sd.2m.arrivals +
                                             Int.sd.3m.arrivals + 
                                             Int.sd.4m.arrivals +
                                             Int.sd.5m.arrivals, hidden = c(2,10),
                                           data = tour_arr_df_clean_nor_train)

plot(tour_arr_df_clean_nor_model_1)


#evaluate the performance of the neural network
#obtain model results
tour_arr_model_1_result <- compute(tour_arr_df_clean_nor_model_1, 
                                   tour_arr_df_clean_nor_test[1:4])

#obtain predicted exchange eur rate values
predicted_arrivals <- tour_arr_model_1_result$net.result

head(predicted_arrivals)

#examine the correlation between predicted and actual values
cor(predicted_arrivals, tour_arr_df_clean_nor_test$Tourist.Arrivals)

#unnormalize the predicted exchange eur rate values
arrivals_min = min(tour_arrivals_df_clean$Tourist.Arrivals)
arrivals_max = max(tour_arrivals_df_clean$Tourist.Arrivals)

unnormalize <- function(x, min, max) {
  return((max - min)*x + min )
}

pred_arr_ori_scale <- unnormalize(predicted_arrivals, arrivals_min, arrivals_max)


#extracting the original exchange rate values from the initial data frame
ex_test_original_arrivals <- tour_arrivals_df_clean[145:164, "Tourist.Arrivals"]
ex_test_original_arrivals


#--------------find errors using RMSE, MAE and MAPE------------------

#find RMSE (Root Mean Squared Error)
rmse <- function(error)
{
  sqrt(mean(error^2))
}

#error of the predictions in this neural network
NN_error = pred_arr_ori_scale - ex_test_original_arrivals

#RMSE for the neural network
NN_RMSE = rmse(NN_error)
NN_RMSE


#find MAE (Mean Absolute Error)
mae <- function(error)
{
  mean(abs(error))
}

NN_MAE <- mae(NN_error)
NN_MAE

#find MAPE (Mean Absolute Percentage Error)
MAPE <- mean(abs(NN_error)/ ex_test_original_arrivals)
MAPE







