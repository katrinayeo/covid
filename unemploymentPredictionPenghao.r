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

# ==============
# Loading data
# ==============

#Loading Monthly Unemployment Rate Data
cutoffIndexUnemp <- 17  # to remove the data after May 2021
MURAll <- read_excel("formated_data.xlsx", sheet = "Monthly Unemployment Rate")

SGMURAll<-MURAll$Singapore
UKMURAll<-MURAll$`United Kingdom`
USMURAll<-MURAll$`United States`

numDataPoints <- NROW(SGMURAll)

SGMUR_Train<-SGMURAll[1:cutoffIndexUnemp]
UKMUR_Train<-UKMURAll[1:cutoffIndexUnemp]
USMUR_Train<-USMURAll[1:cutoffIndexUnemp]

SGMUR_Validate <- SGMURAll[18:22]   # hardcode indicies, otherwise results wrong
USMUR_Validate <- USMURAll[18:22] 
UKMUR_Validate <- UKMURAll[18:22] 

#Loading Government Response Index Data
GRI <- read_excel("formated_data.xlsx", sheet = "Government Response Index")
SGGRI<-GRI$Singapore
UKGRI<-GRI$`United Kingdom`
USGRI<-GRI$`United States`

#Loading Containment Health Index Data
CHI <- read_excel("formated_data.xlsx", sheet = "Containment Health Index")
SGCHI<-CHI$Singapore
UKCHI<-CHI$`United Kingdom`
USCHI<-CHI$`United States`

#Loading Economic Support Index Data
ESI <- read_excel("formated_data.xlsx", sheet = "Economic Support Index")
SGESI<-ESI$Singapore
UKESI<-ESI$`United Kingdom`
USESI<-ESI$`United States`

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

# ==============
# Multivariate ARIMA
# ==============


# ==============
# Cross country single variate ARIMA
# ==============

