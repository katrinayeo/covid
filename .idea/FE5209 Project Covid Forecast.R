library(forecast)
library(ggplot2)
library("readxl")
library(moments)
library(forecast)
require(forecast)  
require(tseries)
require(markovchain)
require(data.table)
library(pander)
library("MTS")
library(ascii)
#Loading Daily New Cases Data
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
SGdata = cbind(SGDNC, SGGRI, SGCHI, SGESI)
diff_SGdata = diff(SGdata)
dim(diff_SGdata)

# Data Preprocessing
validation_data_days = 30
#processing on data (input data)
rows <- NROW(diff_SGdata) # calculate number of rows in time series (number of days)
training_data<-diff_SGdata[1:(rows-validation_data_days),] # Training data
testing_data<-diff_SGdata[(rows-validation_data_days+1):rows,] #Testing data
frequency<-"days"
y_lab <- "Daily Covid 19 Infection cases in Singapore"   # Input name of data
Actual_date_interval <- c("2020/01/01","2021/05/31")
Forecast_date_interval <- c("2021/06/01","2021/10/31")
AD<-fulldate<-seq(as.Date(Actual_date_interval[1]),as.Date(Actual_date_interval[2]), frequency)  # Input range for actual date
FD<-seq(as.Date(Forecast_date_interval[1]),as.Date(Forecast_date_interval[2]), frequency)  # Input range forecasting date
N_forecasting_days<-nrow(data.frame(FD))  # Calculate number of days that you want to forecasting
validation_dates<-tail(AD,validation_data_days) # Select validation_dates
validation_data_by_name<-weekdays(validation_dates) # Names of validation dates
forecasting_data_by_name<-weekdays(FD)  # Names of Forecasting dates

MTSplot(training_data)
ccm(training_data)
m0=VARorder(training_data)
m0$Mstat
names(m0)
m1=VAR(training_data,8)
m2=refVAR(m1,thres=1.96)
MTSdiag(m1,adj=12)

validation_forecast<-VARpred(m1, validation_data_days)
forecasting_VAR <- VARpred(m1, N_forecasting_days+validation_data_days)
MSE_Per_Day<-round((testing_data[, 1]-validation_forecast$pred[, 1])^2, 3)
paste ("MSE for ",validation_data_days,frequency,"by using VAR Model for  ==> ",y_lab, sep=" ")
MSE_Mean_All<-paste(round(mean(MSE_Per_Day),3)," MSE ",validation_data_days,frequency,y_lab,sep=" ")
MSE_Mean_All_VAR<-round(mean(MSE_Per_Day),3)
MSE_VAR<-paste(round(MSE_Per_Day,3))
MSE_VAR_Model<-paste(MSE_Per_Day )
paste ("MSE Error of Forecasting for ",validation_data_days," days in VAR Model for  ==> ",y_lab, sep=" ")
paste(MSE_Mean_All,"%")
paste ("MSE Error of Forecasting day by day for ",validation_data_days," days in VAR Model for  ==> ",y_lab, sep=" ")

print(ascii(data.frame(date_VAR=validation_dates,validation_data_by_name,actual_data=testing_data[, 1],forecasting_VAR=validation_forecast$pred[, 1], MSE_VAR_Model)), type = "rest")
print(ascii(data.frame(FD, forecating_date=forecasting_data_by_name, forecasting_by_VAR=tail(forecasting_VAR$pred[, 1], N_forecasting_days))), type = "rest")
plot(forecasting_VAR$pred[,1],xlab = paste ("Time in", frequency, y_lab, sep=" "), ylab=y_lab)
x1_test <- ts(testing_data, start =(rows-validation_data_days+1) )
lines(x1_test, col='red',lwd=2)
#graph1<-autoplot(forecasting_VAR,xlab = paste ("Time in", frequency ,y_lab , sep=" "), ylab=y_lab)
#graph1

colMeans(training_data) 
sqrt(apply(training_data,2,var))