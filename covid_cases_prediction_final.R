# Group name; Ace
# Final script with the finalised models for predicting COVID cases for SG, UK and US
# Selected models are:
# Singapore - BATS
# United Kingdom - BATS
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


#################################
# PREDICTION FOR SG COVID CASES #
#################################

# BATS Model (Box-Cox Transformation, ARMA residuals, Trend and Seasonality)
all_countries_covid_data <- read_excel("formated_data.xlsx", sheet = "Daily New Cases")
original_data<-all_countries_covid_data$`Singapore`

y_lab <- "Daily Covid 19 Infection cases in Singapore"   # input name of data
Actual_date_interval <- c("2020/01/01","2021/09/31")
Forecast_date_interval <- c("2021/10/01","2021/10/31")

frequency<-"days"
country.name <- "Singapore"
# Data Preparation & calculate some of statistics measures
summary(original_data) # Summary your time series
# calculate standard deviation 
data.frame(kurtosis=kurtosis(original_data))   # calculate Cofficient of kurtosis
data.frame(skewness=skewness(original_data))  # calculate Cofficient of skewness
data.frame(Standard.deviation=sd(original_data))
acf(original_data)
pacf(original_data)
kpss.test(original_data)

#processing on data (input data)
rows <- NROW(original_data) # calculate number of rows in time series (number of days)
training_data<-original_data[1:517] # Training data
testing_data_start_index = 518
testing_data<-original_data[testing_data_start_index:NROW(original_data)] #testing data

AD<-all_countries_covid_data$Date # Input range for actual date
FD<-seq(as.Date(Forecast_date_interval[1]), as.Date(Forecast_date_interval[2]), frequency)  # Input range forecasting date
N_forecasting_days<-nrow(data.frame(FD))  # Number of days to forecast
validation_dates<-tail(AD, NROW(testing_data)) # Number of validation dates
validation_data_by_name<-weekdays(validation_dates) # Name of validation dates
forecasting_data_by_name<-weekdays(FD)  # Names of forecasting dates
validation_data_days<-NROW(testing_data)

data_series<-ts(training_data) # make your data to time series
autoplot(data_series ,xlab=paste ("Time in", frequency, sep=" "), ylab = y_lab, main=paste ("Actual Data :", y_lab, sep=" "))
model_bats<-bats(data_series, use.arma.errors=TRUE)
accuracy(model_bats)  # accuracy on training data
# Print Model Parameters
model_bats
#ploting BATS Model
plot(model_bats,xlab = paste ("Time in", frequency ,y_lab , sep=" "))

# Testing Data Evaluation
one_day_ahead_forecast <- predict(model_bats, h=1)
se_1<-(testing_data[1]-one_day_ahead_forecast$mean)^2
se_vec<-c(se_1)
forecast_vec = c(one_day_ahead_forecast$mean)
for (i in 1:(NROW(testing_data)-1)){
  input_data<-c(training_data, testing_data[1:i])
  temp_model<-bats(input_data, model=model_bats, use.arma.errors=TRUE)
  actual_data<-testing_data[i+1]
  one_day_ahead_forecast <- predict(temp_model, h=1)
  forecast_vec<-c(forecast_vec, one_day_ahead_forecast$mean)
  se<-(actual_data-one_day_ahead_forecast$mean)^2
  se_vec<-c(se_vec, se)
}
plot(c(training_data, forecast_vec),xlab = paste ("Time in", frequency ,y_lab , sep=" "), ylab=y_lab, type='l')
x1_test <- ts(testing_data, start=NROW(training_data)+1 )
lines(x1_test, col='red',lwd=2)
MSE_Per_Day<-round(mean(se_vec), 3)

BATS_forecast_model<-bats(original_data, model=model_bats, use.arma.errors=TRUE)
BATS_forecast <- forecast(BATS_forecast_model, h=N_forecasting_days)
print(ascii(data.frame(FD,forecating_date=forecasting_data_by_name,forecasting_by_NNAR=BATS_forecast$mean)), type = "rest")
plot(c(original_data, BATS_forecast$mean),xlab = paste ("Time in", frequency ,y_lab , sep=" "), ylab=y_lab, type='l')
oct_forecast <- ts(BATS_forecast$mean, start=NROW(original_data)+1 )
lines(oct_forecast, col='red',lwd=2)
MSE_Per_Day





#################################
# PREDICTION FOR UK COVID CASES #
#################################

# BATS Model (Box-Cox Transformation, ARMA residuals, Trend and Seasonality)
all_countries_covid_data <- read_excel("formated_data.xlsx", sheet = "Daily New Cases")
original_data<-all_countries_covid_data$`United Kingdom`

y_lab <- "Daily Covid 19 Infection cases in UK" 
Actual_date_interval <- c("2020/01/01","2021/09/31")
Forecast_date_interval <- c("2021/10/01","2021/10/31")

frequency<-"days"
country.name <- "UK"
# Data Preparation & calculate some of statistics measures
summary(original_data)
data.frame(kurtosis=kurtosis(original_data))  
data.frame(skewness=skewness(original_data))  
data.frame(Standard.deviation=sd(original_data))
acf(original_data)
pacf(original_data)
kpss.test(original_data)

#processing on data (input data)
rows <- NROW(original_data) # calculate number of rows in time series (number of days)
training_data<-original_data[1:517] # Training data
testing_data_start_index = 518
testing_data<-original_data[testing_data_start_index:NROW(original_data)] #testing data

AD<-all_countries_covid_data$Date # Input range for actual date
FD<-seq(as.Date(Forecast_date_interval[1]), as.Date(Forecast_date_interval[2]), frequency)  # Input range forecasting date
N_forecasting_days<-nrow(data.frame(FD))  # Number of days to forecast
validation_dates<-tail(AD, NROW(testing_data)) # Number of validation dates
validation_data_by_name<-weekdays(validation_dates) # Name of validation dates
forecasting_data_by_name<-weekdays(FD)  # Names of forecasting dates
validation_data_days<-NROW(testing_data)

# Data Modeling
data_series<-ts(training_data) # make your data to time series
autoplot(data_series ,xlab=paste ("Time in", frequency, sep=" "), ylab = y_lab, main=paste ("Actual Data :", y_lab, sep=" "))
model_bats<-bats(data_series, use.arma.errors=TRUE)
accuracy(model_bats)  # accuracy on training data
# Print Model Parameters
model_bats
#ploting BATS Model
plot(model_bats,xlab = paste ("Time in", frequency ,y_lab , sep=" "))

# Testing Data Evaluation
one_day_ahead_forecast <- predict(model_bats, h=1)
se_1<-(testing_data[1]-one_day_ahead_forecast$mean)^2
se_vec<-c(se_1)
forecast_vec = c(one_day_ahead_forecast$mean)
for (i in 1:(NROW(testing_data)-1)){
  input_data<-c(training_data, testing_data[1:i])
  temp_model<-bats(input_data, model=model_bats, use.arma.errors=TRUE)
  actual_data<-testing_data[i+1]
  one_day_ahead_forecast <- predict(temp_model, h=1)
  forecast_vec<-c(forecast_vec, one_day_ahead_forecast$mean)
  se<-(actual_data-one_day_ahead_forecast$mean)^2
  se_vec<-c(se_vec, se)
}
plot(c(training_data, forecast_vec),xlab = paste ("Time in", frequency ,y_lab , sep=" "), ylab=y_lab, type='l')
x1_test <- ts(testing_data, start=NROW(training_data)+1 )
lines(x1_test, col='red',lwd=2)
MSE_Per_Day<-round(mean(se_vec), 3)

BATS_forecast_model<-bats(original_data, model=model_bats, use.arma.errors=TRUE)
BATS_forecast <- forecast(BATS_forecast_model, h=N_forecasting_days)
print(ascii(data.frame(FD,forecating_date=forecasting_data_by_name,forecasting_by_NNAR=BATS_forecast$mean)), type = "rest")
plot(c(original_data, BATS_forecast$mean),xlab = paste ("Time in", frequency ,y_lab , sep=" "), ylab=y_lab, type='l')
oct_forecast <- ts(BATS_forecast$mean, start=NROW(original_data)+1 )
lines(oct_forecast, col='red',lwd=2)
MSE_Per_Day





#################################
# PREDICTION FOR US COVID CASES #
#################################

# NNAR Model
all_countries_covid_data <- read_excel("formated_data.xlsx", sheet = "Daily New Cases")
original_data<-all_countries_covid_data$`United States`

y_lab <- "Daily Covid 19 Infection cases in US"   # input name of data
Actual_date_interval <- c("2020/01/01","2021/09/31")
Forecast_date_interval <- c("2021/10/01","2021/10/31")

frequency<-"days"
country.name <- "US"
# Data Preparation & calculate some of statistics measures
summary(original_data) # Summary your time series
# calculate standard deviation 
data.frame(kurtosis=kurtosis(original_data))   # calculate Cofficient of kurtosis
data.frame(skewness=skewness(original_data))  # calculate Cofficient of skewness
data.frame(Standard.deviation=sd(original_data))
acf(original_data)
pacf(original_data)
kpss.test(original_data)

#processing on data (input data)
rows <- NROW(original_data) # calculate number of rows in time series (number of days)
training_data<-original_data[1:517] # Training data
testing_data_start_index = 518
testing_data<-original_data[testing_data_start_index:NROW(original_data)] #testing data

AD<-all_countries_covid_data$Date # Input range for actual date
FD<-seq(as.Date(Forecast_date_interval[1]), as.Date(Forecast_date_interval[2]), frequency)  # Input range forecasting date
N_forecasting_days<-nrow(data.frame(FD))  # Number of days to forecast
validation_dates<-tail(AD, NROW(testing_data)) # Number of validation dates
validation_data_by_name<-weekdays(validation_dates) # Name of validation dates
forecasting_data_by_name<-weekdays(FD)  # Names of forecasting dates
validation_data_days<-NROW(testing_data)

# Build model
data_series<-ts(training_data)
Number_Neural<-1   
model_NNAR<-nnetar(data_series, size=Number_Neural)
accuracy(model_NNAR)
model_NNAR

# Testing Data Evaluation
one_day_ahead_forecast <- forecast(model_NNAR, h=1)
se_1<-(testing_data[1]-one_day_ahead_forecast$mean)^2
se_vec<-c(se_1)
forecast_vec = c(one_day_ahead_forecast$mean)
for (i in 1:(NROW(testing_data)-1)){
  input_data<-c(training_data, testing_data[1:i])
  temp_model<-nnetar(input_data, model=model_NNAR)
  actual_data<-testing_data[i+1]
  one_day_ahead_forecast <- forecast(temp_model, h=1)
  forecast_vec<-c(forecast_vec, one_day_ahead_forecast$mean)
  se<-(actual_data-one_day_ahead_forecast$mean)^2
  se_vec<-c(se_vec, se)
}
plot(c(training_data, forecast_vec),xlab = paste ("Time in", frequency ,y_lab , sep=" "), ylab=y_lab, type='l')
x1_test <- ts(testing_data, start=NROW(training_data)+1 )
lines(x1_test, col='red',lwd=1)
MSE_Per_Day<-round(mean(se_vec), 3)

NNAR_forecast_model<-nnetar(original_data, model=model_NNAR)
NNAR_forecast <- forecast(NNAR_forecast_model, h=N_forecasting_days)
print(ascii(data.frame(FD,forecating_date=forecasting_data_by_name, forecasting_by_NNAR=NNAR_forecast$mean)), type = "rest")
plot(c(original_data, NNAR_forecast$mean), xlab = paste ("Time in", frequency ,y_lab , sep=" "), ylab=y_lab, type='l')
oct_forecast <- ts(NNAR_forecast$mean, start=NROW(original_data)+1 )
lines(oct_forecast, col='red',lwd=1)
MSE_Per_Day