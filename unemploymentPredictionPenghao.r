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

# Start with SGP data
plot(SGMUR_Train, type='l')
acf(SGMUR_Train)  # check stationarity - not stationary
acf(diff(SGMUR_Train))  # check stationarity of diff data - stationary

dSGMUR <- diff(SGMUR_Train)
arima_model_aic <- auto.arima(SGMUR_Train,ic="aic")
arima_model_bic <- auto.arima(SGMUR_Train,ic="bic")

arima_model <- arima_model_aic
forecast(arima_model)

