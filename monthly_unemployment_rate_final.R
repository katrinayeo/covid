# Group name; Ace
# Final script with the finalised models for predicting Monthly Unemployment Rate for SG, UK and US
# Selected models are:
# Singapore - ARIMA
# United Kingdom - NNAR
# United States - NNAR

library(fpp2)
library(ggplot2)
library("readxl")
library(moments)
library(forecast)
require(forecast)  
library(tseries)
require(tseries)
require(markovchain)
require(data.table)
library(Hmisc)
library(ascii)
library(pander)
library(urca)
library(vars)
library("MTS")
set.seed(1)

################
# READING DATA #
################
#Loading Monthly Unemployment Rate Data
SGMUR_Train<-read_excel("formated_data.xlsx", sheet = "MUR SG Train")$MUR
UKMUR_Train<-read_excel("formated_data.xlsx", sheet = "MUR UK Train")$MUR
USMUR_Train<-read_excel("formated_data.xlsx", sheet = "MUR US Train")$MUR

SGMUR_Validate <- read_excel("formated_data.xlsx", sheet = "MUR Validate")$`Singapore`
UKMUR_Validate <- read_excel("formated_data.xlsx", sheet = "MUR Validate")$`United Kingdom`
USMUR_Validate <- read_excel("formated_data.xlsx", sheet = "MUR Validate")$`United States`

###############################################
# PREDICTION FOR SG MONTHLY UNEMPLOYMENT RATE #
###############################################
# with SGP data
acf(SGMUR_Train)  # check stationarity - not stationary
acf(diff(SGMUR_Train))  # check stationarity of diff data - stationary

arima_model_aic <- auto.arima(SGMUR_Train,ic="aic")  # it suggested a ARIMA(0,2,1), which fits the data

arima_model_SG <- arima_model_aic
forecast_raw_SG = forecast(arima_model_SG, length(SGMUR_Validate))
forecast_df_SG = as.data.frame(forecast_raw_SG)
forecasted_value_SG = forecast_df_SG$`Point Forecast`
MSE_each_month_SG<-round((SGMUR_Validate-forecasted_value_SG)^2, 7)
SGMSE <- round(mean(MSE_each_month_SG),5)

y_lab <- "Monthly Unemployment rate in SG" 
frequency<-"months"
plot(forecast_raw_SG,xlab = paste ("Time in", frequency ,y_lab , sep=" "), ylab=y_lab)

SGM6_M10 <- numeric()
SGM6_M10 <- c(SGM6_M10, predict(arima_model_SG, 5)$pred)
print('Forecast for SG from 2021M6 to 2021M10: ')
print(SGM6_M10)
# 2.720442 2.640885 2.561327 2.481769 2.402212

###############################################
# PREDICTION FOR UK MONTHLY UNEMPLOYMENT RATE #
###############################################
y_lab <- "Monthly Unemployment rate in UK"   # input name of data
training_data<-UKMUR_Train
testing_data<-UKMUR_Validate
original_data <- append(training_data, testing_data)

AD<-read_excel("formated_data.xlsx", sheet = "MUR UK Train")$Date
AD<-append(AD, read_excel("formated_data.xlsx", sheet = "MUR Validate")$Date)
Forecast_date_interval <- c("2021/10/01","2021/10/31")
frequency<-"months"
FD<-seq(as.Date(Forecast_date_interval[1]), as.Date(Forecast_date_interval[2]), frequency)  # Input range forecasting date
N_forecasting_months<-nrow(data.frame(FD))  # Number of days to forecast
validation_dates<-tail(AD, NROW(testing_data)) # Number of validation dates
no_validation_data_months<-NROW(testing_data)


data_series<-ts(training_data)
model_NNAR<-nnetar(data_series)
accuracy(model_NNAR)  # Accuracy on training data #Print Model Parameters
model_NNAR

# Testing Data Evaluation
one_month_ahead_forecast <- forecast(model_NNAR, h=1)
one_month_ahead_forecast["fitted"]
one_month_ahead_forecast["residuals"]
UK_se_1<-(testing_data[1]-one_month_ahead_forecast$mean)^2
UK_se_vec<-c(UK_se_1)
forecast_vec = c(one_month_ahead_forecast$mean)
for (i in 1:(NROW(testing_data)-1)){
  input_data<-c(training_data, testing_data[1:i])
  temp_model<-nnetar(input_data, model=model_NNAR)
  actual_data<-testing_data[i+1]
  one_month_ahead_forecast <- forecast(temp_model, h=1)
  forecast_vec<-c(forecast_vec, one_month_ahead_forecast$mean)
  se<-(actual_data-one_month_ahead_forecast$mean)^2
  UK_se_vec<-c(UK_se_vec, se)
}
plot(c(training_data, forecast_vec),xlab = paste ("Time in", frequency ,y_lab , sep=" "), ylab=y_lab, type='l')
x1_test <- ts(testing_data, start=NROW(training_data)+1 )
lines(x1_test, col='red',lwd=2)

MSE_Per_Day<-round(mean(UK_se_vec), 3)
paste ("MSE % For",no_validation_data_months,frequency,"by using NNAR Model for  ==> ",y_lab, sep=" ")
MSE_Mean_All<-paste(MSE_Per_Day," MSE ",no_validation_data_months,frequency,y_lab,sep=" ")
paste ("MSE that's Error of Forecasting for ",no_validation_data_months," months in NNAR Model for  ==> ",y_lab, sep=" ")
paste(MSE_Mean_All)
UKMSE <- MSE_Per_Day

NNAR_forecast_model<-nnetar(original_data, model=model_NNAR)
NNAR_forecast <- forecast(NNAR_forecast_model, h=N_forecasting_months)
print(ascii(data.frame(FD, forecasting_by_NNAR=NNAR_forecast$mean)), type = "rest")
plot(c(original_data, NNAR_forecast$mean), xlab = paste ("Time in", frequency ,y_lab , sep=" "), ylab=y_lab, type='l')

UKM6_M10 = append(forecast_vec, NNAR_forecast$mean[1])
print('Forecast for SG from 2021M6 to 2021M10: ')
print(UKM6_M10)
# 4.624362 4.462292 4.368683 4.158946 4.099160


#### Please clear Environment in between NNAR models#####
###############################################
# PREDICTION FOR US MONTHLY UNEMPLOYMENT RATE #
###############################################

####NNAR model for US####
y_lab <- "Monthly Unemployment rate in US"   # input name of data
training_data<-USMUR_Train # Training data
testing_data<-USMUR_Validate #testing data
original_data <- append(training_data, testing_data)

AD<-read_excel("formated_data.xlsx", sheet = "MUR US Train")$Date
AD<-append(AD, read_excel("formated_data.xlsx", sheet = "MUR Validate")$Date)
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
US_se_1<-(testing_data[1]-one_month_ahead_forecast$mean)^2
US_se_vec<-c(US_se_1)
forecast_vec = c(one_month_ahead_forecast$mean)
for (i in 1:(NROW(testing_data)-1)){
  input_data<-c(training_data, testing_data[1:i])
  temp_model<-nnetar(input_data, model=model_NNAR)
  actual_data<-testing_data[i+1]
  one_month_ahead_forecast <- forecast(temp_model, h=1)
  forecast_vec<-c(forecast_vec, one_month_ahead_forecast$mean)
  se<-(actual_data-one_month_ahead_forecast$mean)^2
  US_se_vec<-c(US_se_vec, se)
}
plot(c(training_data, forecast_vec),xlab = paste ("Time in", frequency ,y_lab , sep=" "), ylab=y_lab, type='l')
x1_test <- ts(testing_data, start=NROW(training_data)+1 )
lines(x1_test, col='red',lwd=2)

MSE_Per_Day<-round(mean(US_se_vec), 3)
paste ("MSE % For",no_validation_data_months,frequency,"by using NNAR Model for  ==> ",y_lab, sep=" ")
MSE_Mean_All<-paste(MSE_Per_Day," MSE ",no_validation_data_months,frequency,y_lab,sep=" ")
paste ("MSE that's Error of Forecasting for ",no_validation_data_months," months in NNAR Model for  ==> ",y_lab, sep=" ")
paste(MSE_Mean_All)
USMSE <- MSE_Per_Day

NNAR_forecast_model<-nnetar(original_data, model=model_NNAR)
NNAR_forecast <- forecast(NNAR_forecast_model, h=N_forecasting_months)
print(ascii(data.frame(FD, forecasting_by_NNAR=NNAR_forecast$mean)), type = "rest")
plot(c(original_data, NNAR_forecast$mean), xlab = paste ("Time in", frequency ,y_lab , sep=" "), ylab=y_lab, type='l')

print(paste('SG MSE: ', SGMSE, 'UK MSE: ', UKMSE, 'US MSE: ', USMSE))
"SG MSE:  0.01474 UK MSE:  0.002 US MSE:  0.203"

USM6_M10 = append(forecast_vec, NNAR_forecast$mean[1])
print('Forecast for US from 2021M6 to 2021M10: ')
print(USM6_M10)
# 5.884568 5.964714 5.562925 5.401515 5.077651

Global = (SGM6_M10 + UKM6_M10 + USM6_M10) / 3
print(Global)
# 4.409791 4.355963 4.164312 4.014077 3.859674
