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

#Loading Daily New Cases Data
DNC <- read.ts("formated_data.xlsx", sheet = "Daily New Cases", header = true)
DNC <- read_excel("formated_data.xlsx", sheet = "Daily New Cases")
SGDNC<-DNC$Singapore
UKDNC<-DNC$`United Kingdom`
USDNC<-DNC$`United States`

#Loading Monthly Unemployment Rate Data
MUR <- read_excel("formated_data.xlsx", sheet = "Monthly Unemployment Rate")
SGMUR<-MUR$Singapore
UKMUR<-MUR$`United Kingdom`
USMUR<-MUR$`United States`

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

#Finding Appropriate model for Singapore
summary(SGMUR)
data.frame(kurtosis=kurtosis(SGMUR))   # calculate Coefficient of kurtosis
data.frame(skewness=skewness(SGMUR))  # calculate Coefficient of skewness
data.frame(Standard.deviation=sd(SGMUR))


# Data Preprocessing
y_lab <- "Monthly Unemployment rate in SG"   # input name of data
NNAR_Model<- TRUE     #create new model (TRUE/FALSE)
validation_data_months = 3
Number_Neural<-5       # Number of Neural For model NNAR Model
#processing on data (input data)
rows <- NROW(SGMUR) # calculate number of rows in time series (number of months)
training_data<-SGMUR[1:(rows-validation_data_months)] # Training data
testing_data<-SGMUR[(rows-validation_data_months+1):rows] #Testing data

frequency<-"months"
Actual_date_interval <- c("2020/01/01","2021/05/31")
Forecast_date_interval <- c("2021/06/01","2021/10/31")

AD<-fulldate<-seq(as.Date(Actual_date_interval[1]),as.Date(Actual_date_interval[2]), frequency)  #input range for actual date
FD<-seq(as.Date(Forecast_date_interval[1]),as.Date(Forecast_date_interval[2]), frequency)  #input range forecasting date

N_forecasting_months<-nrow(data.frame(FD))  #calculate number of months that you want to forecast
validation_dates<-tail(AD,validation_data_months) # select validation_dates
validation_data_by_name<-weekdays(validation_dates) # put names of validation dates
forecasting_data_by_name<-weekdays(FD)  # put names of Forecasting dates


if(NNAR_Model==TRUE){
  data_series<-ts(training_data)
  model_NNAR<-nnetar(data_series, size = Number_Neural)
  saveRDS(model_NNAR, file = "model_NNAR.RDS")
  my_model <- readRDS("model_NNAR.RDS")
  accuracy(model_NNAR)  # accuracy on training data #Print Model Parameters
  model_NNAR
}

forecasting_NNAR <- forecast(model_NNAR, h=N_forecasting_months+validation_data_months)
validation_forecast<-head(forecasting_NNAR$mean,validation_data_months)
MAPE_Per_Day<-round(  abs(((testing_data-validation_forecast)/testing_data)*100)  ,3)
paste ("MAPE % For ",validation_data_months,frequency,"by using NNAR Model for  ==> ",y_lab, sep=" ")
MAPE_Mean_All<-paste(round(mean(MAPE_Per_Day),3),"% MAPE ",validation_data_months,frequency,y_lab,sep=" ")
MAPE_Mean_All_NNAR<-round(mean(MAPE_Per_Day),3)
MAPE_NNAR<-paste(round(MAPE_Per_Day,3),"%")
MAPE_NNAR_Model<-paste(MAPE_Per_Day ,"%")
paste (" MAPE that's Error of Forecasting for ",validation_data_months," months in NNAR Model for  ==> ",y_lab, sep=" ")
paste(MAPE_Mean_All,"%")
paste ("MAPE that's Error of Forecasting month by month for ",validation_data_months," months in NNAR Model for  ==> ",y_lab, sep=" ")
print(ascii(data.frame(date_NNAR=validation_dates,validation_data_by_name,actual_data=testing_data,forecasting_NNAR=validation_forecast,MAPE_NNAR_Model)), type = "rest")
print(ascii(data.frame(FD,forecating_date=forecasting_data_by_name,forecasting_by_NNAR=tail(forecasting_NNAR$mean,N_forecasting_months))), type = "rest")
plot(forecasting_NNAR,xlab = paste ("Time in", frequency ,y_lab , sep=" "), ylab=y_lab)
x1_test <- ts(testing_data, start =(rows-validation_data_months+1) )
lines(x1_test, col='red',lwd=2)
graph1<-autoplot(forecasting_NNAR,xlab = paste ("Time in", frequency ,y_lab , sep=" "), ylab=y_lab)
graph1

