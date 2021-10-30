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
library(tseries)
library(forecast)   
require(tseries) 
library(forecast)
library(ggplot2)
library("readxl")
library(moments)
library("MTS")

# ==============
# Loading data
# ==============

#Loading Monthly Unemployment Rate Data
cutoffIndexUnemp <- 17  # to remove the data after May 2021
validation_data_periods <- 4
MURAll <- read_excel("formated_data.xlsx", sheet = "Monthly Unemployment Rate")
MURExt <- read_excel("formated_data.xlsx", sheet = "Extended Unemployment Rate")

SGMURAll<-MURAll$Singapore
UKMURAll<-MURExt$`United Kingdom`[1:49]
USMURAll<-MURExt$`United States`[1:49]

SGMUR_Train<-SGMURAll[1:cutoffIndexUnemp]
UKMUR_Train<-UKMURAll
USMUR_Train<-USMURAll

SGMUR_Validate <- SGMURAll[18:21]   # hardcode indicies, otherwise results wrong
USMUR_Validate <- MURExt$`United Kingdom`[50:53] 
UKMUR_Validate <- MURExt$`United States`[50:53]  

# ==============
# Single variate ARIMA
# ==============

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


# with UK data
plot(UKMUR_Train, type='l')
acf(UKMUR_Train)  # check stationarity - not stationary
acf(diff(UKMUR_Train))  # check stationarity of diff data - stationary with AR(2)

arima_model_UK_try <- auto.arima(UKMUR_Train) # suggested ARIMA(2,1,0), but need to diff
arima_model_UK <- arima(UKMUR_Train, order=c(2,0,1)) # I take what acf told me and do ARIMA(2,0,1)
forecast_raw_UK = forecast(arima_model_UK, length(UKMUR_Validate))
forecast_df_UK = as.data.frame(forecast_raw_UK)
forecasted_value_UK = forecast_df_UK$`Point Forecast`
MSE_each_month_UK<-round((UKMUR_Validate-forecasted_value_UK)^2, 7)
UKMSE <- round(mean(MSE_each_month_UK),5)

y_lab <- "Monthly Unemployment rate in SG" 
plot(forecast_raw_UK,xlab = paste ("Time in", frequency ,y_lab , sep=" "), ylab=y_lab)

# with US data
plot(USMUR_Train, type='l')
plot(diff(USMUR_Train), type='l')
acf(USMUR_Train)  # check stationarity - not stationary
acf(diff(USMUR_Train))  # check stationarity of diff data - stationary, and no AR, white noise.

arima_model_aic <- auto.arima(USMUR_Train,ic="aic")  # suggested ARIMA(0,1,0), but need to diff to be stationary
arima_model_US <- arima(USMUR_Train, order=c(0,0,1)) # I fitted this from my observation in acf plot
forecast_raw_US = forecast(arima_model_US, length(USMUR_Validate))
forecast_df_US = as.data.frame(forecast_raw_US)
forecasted_value_US = forecast_df_US$`Point Forecast`
MSE_each_month_US<-round((USMUR_Validate-forecasted_value_US)^2, 7)
USMSE <- round(mean(MSE_each_month_US),5)

plot(forecast_raw_US)

print(paste('SG MSE: ', SGMSE, 'UK MSE: ', UKMSE, 'US MSE: ', USMSE)) 
# "SG MSE:  0.01474 UK MSE:  0.546 US MSE:  0.62149"

## GLOBAL

# add the 3 together and average
Glob_MUR <- (forecasted_value_US + forecasted_value_UK + forecasted_value_SG) / 3
Glob_Validate <- (UKMUR_Validate + USMUR_Validate + SGMUR_Validate) / 3
MSE_each_month_Glob<-round((Glob_Validate-Glob_MUR)^2, 7)
Glob_MSE <- round(mean(MSE_each_month_Glob),5)
print(paste('Global MSE: ', Glob_MSE))
# "Global MSE:  0.02387" 
