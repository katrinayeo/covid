# Group name; Ace
# Working file to test out different models (final submission is in covid_cases_prediction_final.R)

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
USMUR_Validate <- read_excel("formated_data.xlsx", sheet = "MUR Validate")$`United Kingdom`
UKMUR_Validate <- read_excel("formated_data.xlsx", sheet = "MUR Validate")$`United States`

#Loading Government Response Index Data
GRI <- read_excel("formated_data.xlsx", sheet = "GRI Monthly")
SGGRI<-GRI$Singapore
UKGRI<-GRI$`United Kingdom`
USGRI<-GRI$`United States`

#Loading Containment Health Index Data
CHI <- read_excel("formated_data.xlsx", sheet = "CHI Monthly")
SGCHI<-CHI$Singapore
UKCHI<-CHI$`United Kingdom`
USCHI<-CHI$`United States`

#Loading Economic Support Index Data
ESI <- read_excel("formated_data.xlsx", sheet = "ESI Monthly")
SGESI<-ESI$Singapore
UKESI<-ESI$`United Kingdom`
USESI<-ESI$`United States`

#Loading Stringent Index Data
SI <- read_excel("formated_data.xlsx", sheet = "SI Monthly")
SGSI<-SI$Singapore
UKSI<-SI$`United Kingdom`
USSI<-SI$`United States`

# ==============
# Single variate ARIMA
# ==============

# with SGP data
acf(SGMUR_Train)  # check stationarity - not stationary
acf(diff(SGMUR_Train))  # check stationarity of diff data - stationary

arima_model_aic <- auto.arima(SGMUR_Train,ic="aic")
arima_model_bic <- auto.arima(SGMUR_Train,ic="bic")

arima_model_SG <- arima_model_aic
forecast_raw_SG = forecast(arima_model_SG, length(SGMUR_Validate))
forecast_df_SG = as.data.frame(forecast_raw_SG)
forecasted_value_SG = forecast_df_SG$`Point Forecast`
MSE_each_month_SG<-round((SGMUR_Validate-forecasted_value_SG)^2, 7)
SGMSE <- round(mean(MSE_each_month_SG),5)

y_lab <- "Monthly Unemployment rate in SG" 
frequency<-"months"
plot(SGMUR_Train, type='l')
plot(forecast_raw_SG,xlab = paste ("Time in", frequency ,y_lab , sep=" "), ylab=y_lab)


# with UK data
plot(UKMUR_Train, type='l')
acf(UKMUR_Train)  # check stationarity - not stationary
acf(diff(UKMUR_Train))  # check stationarity of diff data - stationary

arima_model_aic <- auto.arima(UKMUR_Train,ic="aic")
arima_model_bic <- auto.arima(UKMUR_Train,ic="bic")

arima_model_UK <- arima_model_aic
forecast_raw_UK = forecast(arima_model_UK, length(UKMUR_Validate))
forecast_df_UK = as.data.frame(forecast_raw_UK)
forecasted_value_UK = forecast_df_UK$`Point Forecast`
MSE_each_month_UK<-round((UKMUR_Validate-forecasted_value_UK)^2, 7)
UKMSE <- round(mean(MSE_each_month_UK),5)

y_lab <- "Monthly Unemployment rate in UK" 
frequency<-"months"
plot(forecast_raw_UK,xlab = paste ("Time in", frequency ,y_lab , sep=" "), ylab=y_lab)

# with US data
plot(USMUR_Train, type='l')
acf(USMUR_Train)  # check stationarity - not stationary
acf(diff(USMUR_Train))  # check stationarity of diff data - stationary

arima_model_aic <- auto.arima(USMUR_Train,ic="aic")
arima_model_bic <- auto.arima(USMUR_Train,ic="bic")

arima_model_US <- arima_model_aic
forecast_raw_US = forecast(arima_model_US, length(USMUR_Validate))
forecast_df_US = as.data.frame(forecast_raw_US)
forecasted_value_US = forecast_df_US$`Point Forecast`
MSE_each_month_US<-round((USMUR_Validate-forecasted_value_US)^2, 7)
USMSE <- round(mean(MSE_each_month_US),5)

y_lab <- "Monthly Unemployment rate in US" 
frequency<-"months"
plot(forecast_raw_US,xlab = paste ("Time in", frequency ,y_lab , sep=" "), ylab=y_lab)

print(paste('SG MSE: ', SGMSE, 'UK MSE: ', UKMSE, 'US MSE: ', USMSE)) 
# "SG MSE:  0.01474 UK MSE:  0.60953 US MSE:  1.985"

## GLOBAL

# approach 1: add all together and fit a separate TS
Glob_MUR_Train <- (SGMUR_Train + USMUR_Train + UKMUR_Train) / 3
plot(Glob_MUR_Train, type='l')
acf(Glob_MUR_Train)  # check stationarity - not stationary
acf(diff(Glob_MUR_Train))  # check stationarity of diff data - stationary

arima_model_aic <- auto.arima(Glob_MUR_Train,ic="aic")
arima_model_bic <- auto.arima(Glob_MUR_Train,ic="bic")

arima_model_Glob <- arima_model_aic
forecast_raw_Glob = forecast(arima_model_Glob, validation_data_periods)
forecast_df_Glob = as.data.frame(forecast_raw_Glob)
forecasted_value_Glob = forecast_df_Glob$`Point Forecast`

GlobMUR_Validate <- (UKMUR_Validate + USMUR_Validate + SGMUR_Validate) / 3
MSE_each_month_Glob<-round((GlobMUR_Validate-forecasted_value_Glob)^2, 7)
GlobMSE <- round(mean(MSE_each_month_Glob),5)
print(paste('Global MSE: ', Glob_MSE))
# "Global MSE:  1.72798"

y_lab <- "Monthly Unemployment rate GLobally" 
frequency<-"months"
plot(forecast_raw_Glob,xlab = paste ("Time in", frequency ,y_lab , sep=" "), ylab=y_lab)

# approach 2: simply add the 3 together and average
Glob_MUR <- (forecasted_value_US + forecasted_value_UK + forecasted_value_SG) / 3
Glob_Validate <- (UKMUR_Validate + USMUR_Validate + SGMUR_Validate) / 3
MSE_each_month_Glob<-round((Glob_Validate-Glob_MUR)^2, 7)
Glob_MSE <- round(mean(MSE_each_month_Glob),5)
print(paste('Global MSE: ', Glob_MSE))
# "Global MSE:  1.72798" Looks like the same as doing it with another ARIMA on Global

# ==============
# Multivariate ARIMA
# ==============

# On SG

# First remove 'na'
SGGRI <- SGGRI[SGGRI!=-1]
SGCHI <- SGCHI[SGCHI!=-1]
SGESI <- SGESI[SGESI!=-1]
SGSI  <- SGSI[SGSI!=-1]

# Trim the lists to the same length
length_of_period = min(length(SGGRI), length(SGCHI), length(SGESI), length(SGSI), length(SGMUR_Train))
print( paste('Minimum length of vector with all elements eligible is', length_of_period) )
tSGGRI <- SGGRI[ (length(SGGRI) - length_of_period + 1) : length(SGGRI)]
tSGCHI <- SGCHI[ (length(SGCHI) - length_of_period + 1) : length(SGCHI)]
tSGESI <- SGESI[ (length(SGESI) - length_of_period + 1) : length(SGESI)]
tSGSI <- SGSI[ (length(SGSI) - length_of_period + 1) : length(SGSI)]
tSGMUR <- SGMUR_Train[ (length(SGMUR_Train) - length_of_period + 1) : length(SGMUR_Train)]

# Diff the data to make it stationary
dSGGRI <- diff( tSGGRI )
dSGCHI <- diff( tSGCHI )
dSGESI <- diff( tSGESI )
dSGSI <- diff( tSGSI )
dSGMUR <- diff( tSGMUR )

par(mfrow=c(5,1))
acf(dSGGRI)
acf(dSGCHI)
acf(dSGESI)
acf(dSGSI)
acf(dSGMUR)

# Fit the VAR model
training_data = cbind(dSGMUR, dSGESI, dSGSI, dSGCHI, dSGGRI)
MTSplot(training_data)
ccm(training_data)
m1=VAR(training_data,2)
m2=refVAR(m1,thres=1.96)

# Prediction
diff_data_forecast<-VARpred(m1, validation_data_periods)  
dSGMUR_forecast <- diff_data_forecast$pred[,1]

SGMUR_forecast <- c(0,0,0,0)
curr <- tSGMUR[length(tSGMUR)]
idx <- 1
for (i in dSGMUR_forecast) {
  curr = curr + i
  SGMUR_forecast[idx] = curr
  idx = idx + 1
}

MSE_each_month_SG<-round((SGMUR_Validate-SGMUR_forecast)^2, 7)
SGMSE <- round(mean(MSE_each_month_SG),5)


# On UK

# First remove 'na'
UKGRI <- UKGRI[UKGRI!=-1]
UKCHI <- UKCHI[UKCHI!=-1]
UKESI <- UKESI[UKESI!=-1]
UKSI  <- UKSI[UKSI!=-1]

# Trim the lists to the same length
length_of_period = min(length(UKGRI), length(UKCHI), length(UKESI), length(UKSI), length(UKMUR_Train))
print( paste('Minimum length of vector with all elements eligible is', length_of_period) )
tUKGRI <- UKGRI[ (length(UKGRI) - length_of_period + 1) : length(UKGRI)]
tUKCHI <- UKCHI[ (length(UKCHI) - length_of_period + 1) : length(UKCHI)]
tUKESI <- UKESI[ (length(UKESI) - length_of_period + 1) : length(UKESI)]
tUKSI <- UKSI[ (length(UKSI) - length_of_period + 1) : length(UKSI)]
tUKMUR <- UKMUR_Train[ (length(UKMUR_Train) - length_of_period + 1) : length(UKMUR_Train)]

# Diff the data to make it stationary
dUKGRI <- diff( tUKGRI )
dUKCHI <- diff( tUKCHI )
#dUKESI <- diff( tUKESI )
dUKSI <- diff( tUKSI )
dUKMUR <- diff( tUKMUR )

par(mfrow=c(4,1))
acf(dUKGRI)
acf(dUKCHI)
#acf(dUKESI)
acf(dUKSI)
acf(dUKMUR)

# Fit the VAR model
training_data = cbind(dUKMUR, dUKSI, dUKCHI, dUKGRI)
MTSplot(training_data)
ccm(training_data)
m1=VAR(training_data,2)
m2=refVAR(m1,thres=1.96)

# Prediction
diff_data_forecast<-VARpred(m1, validation_data_periods)  
dUKMUR_forecast <- diff_data_forecast$pred[,1]

UKMUR_forecast <- c(0,0,0,0)
curr <- tUKMUR[length(tUKMUR)]
idx <- 1
for (i in dUKMUR_forecast) {
  curr = curr + i
  UKMUR_forecast[idx] = curr
  idx = idx + 1
}

MSE_each_month_UK<-round((UKMUR_Validate-UKMUR_forecast)^2, 7)
UKMSE <- round(mean(MSE_each_month_UK),5)


# On US

# First remove 'na'
USGRI <- USGRI[USGRI!=-1]
USCHI <- USCHI[USCHI!=-1]
USESI <- USESI[USESI!=-1]
USSI  <- USSI[USSI!=-1]

# Trim the lists to the same length
length_of_period = min(length(USGRI), length(USCHI), length(USESI), length(USSI), length(USMUR_Train))
print( paste('Minimum length of vector with all elements eligible is', length_of_period) )
tUSGRI <- USGRI[ (length(USGRI) - length_of_period + 1) : length(USGRI)]
tUSCHI <- USCHI[ (length(USCHI) - length_of_period + 1) : length(USCHI)]
tUSESI <- USESI[ (length(USESI) - length_of_period + 1) : length(USESI)]
tUSSI <- USSI[ (length(USSI) - length_of_period + 1) : length(USSI)]
tUSMUR <- USMUR_Train[ (length(USMUR_Train) - length_of_period + 1) : length(USMUR_Train)]

# Diff the data to make it stationary
dUSGRI <- diff( tUSGRI )
dUSCHI <- diff( tUSCHI )
dUSESI <- diff( tUSESI )
dUSSI <- diff( tUSSI )
dUSMUR <- diff( tUSMUR )

par(mfrow=c(4,1))
acf(dUSGRI)
acf(dUSCHI)
#acf(dUSESI)
#acf(dUSSI)
acf(dUSMUR)

# Fit the VAR model
training_data = cbind(dUSMUR, dUSCHI, dUSSI)
MTSplot(training_data)
ccm(training_data)
m1=VAR(training_data,1)
m2=refVAR(m1,thres=1.96)

# Prediction
diff_data_forecast<-VARpred(m1, validation_data_periods)  
dUSMUR_forecast <- diff_data_forecast$pred[,1]

USMUR_forecast <- c(0,0,0,0)
curr <- tUSMUR[length(tUSMUR)]
idx <- 1
for (i in dUSMUR_forecast) {
  curr = curr + i
  USMUR_forecast[idx] = curr
  idx = idx + 1
}

MSE_each_month_US<-round((USMUR_Validate-USMUR_forecast)^2, 7)
USMSE <- round(mean(MSE_each_month_US),5)

print(paste('SG MSE: ', SGMSE, 'UK MSE: ', UKMSE, 'US MSE: ', USMSE)) 
#"SG MSE:  0.10125 UK MSE:  7.71931 US MSE:  11.87819"

## GLOBAL

Glob_MUR <- (USMUR_forecast + UKMUR_forecast + SGMUR_forecast) / 3
Glob_Validate <- (UKMUR_Validate + USMUR_Validate + SGMUR_Validate) / 3
MSE_each_month_Glob<-round((Glob_Validate-Glob_MUR)^2, 7)
Glob_MSE <- round(mean(MSE_each_month_Glob),5)
print(paste('Global MSE: ', Glob_MSE))
# "Global MSE:  0.17714"

# ==============
# Cross country single variate ARIMA
# ==============

training_data = cbind(diff(SGMUR_Train), diff(UKMUR_Train), diff(USMUR_Train))
MTSplot(training_data)
ccm(training_data)
m1 <- VAR(training_data,1)
m2=refVAR(m1,thres=1.96)
data_forecast<-VARpred(m1, validation_data_periods)$pred  
UKMUR_forecast <- data_forecast[,1]
SGMUR_forecast <- data_forecast[,2]
USMUR_forecast <- data_forecast[,3]

MSE_each_month_UK<-round((UKMUR_Validate-UKMUR_forecast)^2, 7)
UKMSE <- round(mean(MSE_each_month_UK),5)
MSE_each_month_SG<-round((SGMUR_Validate-SGMUR_forecast)^2, 7)
SGMSE <- round(mean(MSE_each_month_SG),5)
MSE_each_month_US<-round((USMUR_Validate-USMUR_forecast)^2, 7)
USMSE <- round(mean(MSE_each_month_US),5)

print(paste('SG MSE: ', SGMSE, 'UK MSE: ', UKMSE, 'US MSE: ', USMSE)) 
# "SG MSE:  0.06297 UK MSE:  6.19581 US MSE:  1.14636"

## GLOBAL

Glob_MUR <- (USMUR_forecast + UKMUR_forecast + SGMUR_forecast) / 3
Glob_Validate <- (UKMUR_Validate + USMUR_Validate + SGMUR_Validate) / 3
MSE_each_month_Glob<-round((Glob_Validate-Glob_MUR)^2, 7)
Glob_MSE <- round(mean(MSE_each_month_Glob),5)
print(paste('Global MSE: ', Glob_MSE))
# "Global MSE:  1.15841"


# ==============
# Linear Regression
# ==============

# On SG

# First remove 'na'
SGGRI <- SGGRI[SGGRI!=-1]
SGCHI <- SGCHI[SGCHI!=-1]
SGESI <- SGESI[SGESI!=-1]
SGSI  <- SGSI[SGSI!=-1]

# Trim the lists to the same length
length_of_period = min(length(SGGRI), length(SGCHI), length(SGESI), length(SGSI), length(SGMUR_Train))
print( paste('Minimum length of vector with all elements eligible is', length_of_period) )
tSGGRI <- SGGRI[ (length(SGGRI) - length_of_period + 1) : length(SGGRI)]
tSGCHI <- SGCHI[ (length(SGCHI) - length_of_period + 1) : length(SGCHI)]
tSGESI <- SGESI[ (length(SGESI) - length_of_period + 1) : length(SGESI)]
tSGSI <- SGSI[ (length(SGSI) - length_of_period + 1) : length(SGSI)]
tSGMUR <- SGMUR_Train[ (length(SGMUR_Train) - length_of_period + 1) : length(SGMUR_Train)]

m1 <- lm(tSGMUR ~ tSGGRI + tSGCHI + tSGESI + tSGSI)
anova(m1)  # ESI not very significant
m2 <- lm(tSGMUR ~ tSGGRI + tSGCHI + tSGSI)  # SI not very significant
anova(m2)
m3 <- lm(tSGMUR ~ tSGGRI + tSGCHI)
anova(m3)

# waiting for data on GRI and CHI from M6 to M9....

####NNAR model for Singapore####
y_lab <- "Monthly Unemployment rate in SG"   # input name of data
training_data<-SGMUR_Train
testing_data_start_index = 18
testing_data<-SGMUR_Validate
original_data <- append(training_data, testing_data)

AD<-read_excel("formated_data.xlsx", sheet = "MUR SG Train")$Date
append(AD, read_excel("formated_data.xlsx", sheet = "MUR Validate")$Date)
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
SGMSE <- MSE_Per_Day

NNAR_forecast_model<-nnetar(original_data, model=model_NNAR)
NNAR_forecast <- forecast(NNAR_forecast_model, h=N_forecasting_months)
print(ascii(data.frame(FD, forecasting_by_NNAR=NNAR_forecast$mean)), type = "rest")
plot(c(original_data, NNAR_forecast$mean), xlab = paste ("Time in", frequency ,y_lab , sep=" "), ylab=y_lab, type='l')

####NNAR model for UK####
y_lab <- "Monthly Unemployment rate in UK"   # input name of data
training_data<-UKMUR_Train
testing_data_start_index = 50
testing_data<-UKMUR_Validate
original_data <- append(training_data, testing_data)

AD<-read_excel("formated_data.xlsx", sheet = "MUR UK Train")$Date
append(AD, read_excel("formated_data.xlsx", sheet = "MUR Validate")$Date)
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
UKMSE <- MSE_Per_Day

NNAR_forecast_model<-nnetar(original_data, model=model_NNAR)
NNAR_forecast <- forecast(NNAR_forecast_model, h=N_forecasting_months)
print(ascii(data.frame(FD, forecasting_by_NNAR=NNAR_forecast$mean)), type = "rest")
plot(c(original_data, NNAR_forecast$mean), xlab = paste ("Time in", frequency ,y_lab , sep=" "), ylab=y_lab, type='l')


####NNAR model for US####
y_lab <- "Monthly Unemployment rate in US"   # input name of data
training_data<-USMUR_Train
testing_data_start_index = 50
testing_data<-original_data[testing_data_start_index:53] #testing data
original_data <- append(training_data, testing_data)

AD<-read_excel("formated_data.xlsx", sheet = "MUR US Train")$Date
append(AD, read_excel("formated_data.xlsx", sheet = "MUR Validate")$Date)
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
USMSE <- MSE_Per_Day

NNAR_forecast_model<-nnetar(original_data, model=model_NNAR)
NNAR_forecast <- forecast(NNAR_forecast_model, h=N_forecasting_months)
print(ascii(data.frame(FD, forecasting_by_NNAR=NNAR_forecast$mean)), type = "rest")
plot(c(original_data, NNAR_forecast$mean), xlab = paste ("Time in", frequency ,y_lab , sep=" "), ylab=y_lab, type='l')

print(paste('SG MSE: ', SGMSE, 'UK MSE: ', UKMSE, 'US MSE: ', USMSE)) 
