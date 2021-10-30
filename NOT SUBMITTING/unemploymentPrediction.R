library(fpp2)
library(forecast)
library(ggplot2)
library("readxl")
library(moments)
library(forecast)
require(forecast)  
require(tseries)
require(markovchain)
require(data.table)
library(Hmisc)
library(ascii)
library(pander)
library(QuantTools)
library(lubridate)

set.seed(1)

#Loading Monthly Unemployment Rate Data
MUR <- read_excel("formated_data.xlsx", sheet = "Extended Unemployment Rate")
SGMUR<-MUR$Singapore
UKMUR<-MUR$`United Kingdom`
USMUR<-MUR$`United States`

####NNAR model for Singapore####
original_data=SGMUR[33:53]
y_lab <- "Monthly Unemployment rate in SG"   # input name of data
rows <- NROW(original_data) # calculate number of rows in time series (number of days)
training_data<-original_data[1:17] # Training data
testing_data_start_index = 18
testing_data<-original_data[testing_data_start_index:21] #testing data

AD<-MUR$Date # Input range for actual date
Forecast_date_interval <- c("2021/10/01","2021/10/31")
frequency<-"months"
FD<-seq(as.Date(Forecast_date_interval[1]), as.Date(Forecast_date_interval[2]), frequency)  # Input range forecasting date
N_forecasting_months<-nrow(data.frame(FD))  # Number of days to forecast
validation_dates<-tail(AD, NROW(testing_data)) # Number of validation dates
no_validation_data_months<-NROW(testing_data)


data_series<-ts(training_data)
#Number_Neural<-1    # Number of Neural For model NNAR Model
model_NNAR<-nnetar(data_series)
accuracy(model_NNAR)  # Accuracy on training data #Print Model Parameters
model_NNAR

# Testing Data Evaluation
one_month_ahead_forecast <- forecast(model_NNAR, h=1)
se_1<-(testing_data[1]-one_month_ahead_forecast$mean)^2
se_vec<-c(se_1)
forecast_vec = c(one_month_ahead_forecast$mean)
for (i in 1:(NROW(testing_data)-1)){
  input_data<-c(training_data, testing_data[1:i])
  temp_model<-nnetar(input_data, model=model_NNAR)
  actual_data<-testing_data[i+1]
  one_month_ahead_forecast <- forecast(temp_model, h=1)
  forecast_vec<-c(forecast_vec, one_month_ahead_forecast$mean)
  se<-(actual_data-one_month_ahead_forecast$mean)^2
  se_vec<-c(se_vec, se)
}
plot(c(training_data, forecast_vec),xlab = paste ("Time in", frequency ,y_lab , sep=" "), ylab=y_lab, type='l')
x1_test <- ts(testing_data, start=NROW(training_data)+1 )
lines(x1_test, col='red',lwd=2)

MSE_Per_Day<-round(mean(se_vec), 3)
paste ("MSE % For",no_validation_data_months,frequency,"by using NNAR Model for  ==> ",y_lab, sep=" ")
MSE_Mean_All<-paste(MSE_Per_Day," MSE ",no_validation_data_months,frequency,y_lab,sep=" ")
paste ("MSE that's Error of Forecasting for ",no_validation_data_months," months in NNAR Model for  ==> ",y_lab, sep=" ")
paste(MSE_Mean_All)

NNAR_forecast_model<-nnetar(original_data, model=model_NNAR)
NNAR_forecast <- forecast(NNAR_forecast_model, h=N_forecasting_months)
print(ascii(data.frame(FD, forecasting_by_NNAR=NNAR_forecast$mean)), type = "rest")
plot(c(original_data, NNAR_forecast$mean), xlab = paste ("Time in", frequency ,y_lab , sep=" "), ylab=y_lab, type='l')




####NNAR model for UK####
original_data=UKMUR
y_lab <- "Monthly Unemployment rate in UK"   # input name of data
rows <- NROW(original_data) # calculate number of rows in time series (number of days)
training_data<-original_data[1:49] # Training data
testing_data_start_index = 50
testing_data<-original_data[testing_data_start_index:53] #testing data

AD<-MUR$Date # Input range for actual date
Forecast_date_interval <- c("2021/10/01","2021/10/31")
frequency<-"months"
FD<-seq(as.Date(Forecast_date_interval[1]), as.Date(Forecast_date_interval[2]), frequency)  # Input range forecasting date
N_forecasting_months<-nrow(data.frame(FD))  # Number of days to forecast
validation_dates<-tail(AD, NROW(testing_data)) # Number of validation dates
no_validation_data_months<-NROW(testing_data)


data_series<-ts(training_data)
#Number_Neural<-1    # Number of Neural For model NNAR Model
model_NNAR<-nnetar(data_series)
accuracy(model_NNAR)  # Accuracy on training data #Print Model Parameters
model_NNAR

# Testing Data Evaluation
one_month_ahead_forecast <- forecast(model_NNAR, h=1)
one_month_ahead_forecast["fitted"]
one_month_ahead_forecast["residuals"]
se_1<-(testing_data[1]-one_month_ahead_forecast$mean)^2
se_vec<-c(se_1)
forecast_vec = c(one_month_ahead_forecast$mean)
for (i in 1:(NROW(testing_data)-1)){
  input_data<-c(training_data, testing_data[1:i])
  temp_model<-nnetar(input_data, model=model_NNAR)
  actual_data<-testing_data[i+1]
  one_month_ahead_forecast <- forecast(temp_model, h=1)
  forecast_vec<-c(forecast_vec, one_month_ahead_forecast$mean)
  se<-(actual_data-one_month_ahead_forecast$mean)^2
  se_vec<-c(se_vec, se)
}
plot(c(training_data, forecast_vec),xlab = paste ("Time in", frequency ,y_lab , sep=" "), ylab=y_lab, type='l')
x1_test <- ts(testing_data, start=NROW(training_data)+1 )
lines(x1_test, col='red',lwd=2)

MSE_Per_Day<-round(mean(se_vec), 3)
paste ("MSE % For",no_validation_data_months,frequency,"by using NNAR Model for  ==> ",y_lab, sep=" ")
MSE_Mean_All<-paste(MSE_Per_Day," MSE ",no_validation_data_months,frequency,y_lab,sep=" ")
paste ("MSE that's Error of Forecasting for ",no_validation_data_months," months in NNAR Model for  ==> ",y_lab, sep=" ")
paste(MSE_Mean_All)

NNAR_forecast_model<-nnetar(original_data, model=model_NNAR)
NNAR_forecast <- forecast(NNAR_forecast_model, h=N_forecasting_months)
print(ascii(data.frame(FD, forecasting_by_NNAR=NNAR_forecast$mean)), type = "rest")
plot(c(original_data, NNAR_forecast$mean), xlab = paste ("Time in", frequency ,y_lab , sep=" "), ylab=y_lab, type='l')







####NNAR model for US####
original_data=USMUR
y_lab <- "Monthly Unemployment rate in US"   # input name of data
rows <- NROW(original_data) # calculate number of rows in time series (number of days)
training_data<-original_data[1:49] # Training data
testing_data_start_index = 50
testing_data<-original_data[testing_data_start_index:53] #testing data

AD<-MUR$Date # Input range for actual date
Forecast_date_interval <- c("2021/10/01","2021/10/31")
frequency<-"months"
FD<-seq(as.Date(Forecast_date_interval[1]), as.Date(Forecast_date_interval[2]), frequency)  # Input range forecasting date
N_forecasting_months<-nrow(data.frame(FD))  # Number of days to forecast
validation_dates<-tail(AD, NROW(testing_data)) # Number of validation dates
no_validation_data_months<-NROW(testing_data)


data_series<-ts(training_data)
#Number_Neural<-1    # Number of Neural For model NNAR Model
model_NNAR<-nnetar(data_series)
accuracy(model_NNAR)  # Accuracy on training data #Print Model Parameters
model_NNAR

# Testing Data Evaluation
one_month_ahead_forecast <- forecast(model_NNAR, h=1)
one_month_ahead_forecast["fitted"]
one_month_ahead_forecast["residuals"]
se_1<-(testing_data[1]-one_month_ahead_forecast$mean)^2
se_vec<-c(se_1)
forecast_vec = c(one_month_ahead_forecast$mean)
for (i in 1:(NROW(testing_data)-1)){
  input_data<-c(training_data, testing_data[1:i])
  temp_model<-nnetar(input_data, model=model_NNAR)
  actual_data<-testing_data[i+1]
  one_month_ahead_forecast <- forecast(temp_model, h=1)
  forecast_vec<-c(forecast_vec, one_month_ahead_forecast$mean)
  se<-(actual_data-one_month_ahead_forecast$mean)^2
  se_vec<-c(se_vec, se)
}
plot(c(training_data, forecast_vec),xlab = paste ("Time in", frequency ,y_lab , sep=" "), ylab=y_lab, type='l')
x1_test <- ts(testing_data, start=NROW(training_data)+1 )
lines(x1_test, col='red',lwd=2)

MSE_Per_Day<-round(mean(se_vec), 3)
paste ("MSE % For",no_validation_data_months,frequency,"by using NNAR Model for  ==> ",y_lab, sep=" ")
MSE_Mean_All<-paste(MSE_Per_Day," MSE ",no_validation_data_months,frequency,y_lab,sep=" ")
paste ("MSE that's Error of Forecasting for ",no_validation_data_months," months in NNAR Model for  ==> ",y_lab, sep=" ")
paste(MSE_Mean_All)

NNAR_forecast_model<-nnetar(original_data, model=model_NNAR)
NNAR_forecast <- forecast(NNAR_forecast_model, h=N_forecasting_months)
print(ascii(data.frame(FD, forecasting_by_NNAR=NNAR_forecast$mean)), type = "rest")
plot(c(original_data, NNAR_forecast$mean), xlab = paste ("Time in", frequency ,y_lab , sep=" "), ylab=y_lab, type='l')





