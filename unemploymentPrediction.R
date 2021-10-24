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

####NNAR model for Singapore####
plot(SGMUR)
hist(SGMUR)
summary(SGMUR)
data.frame(kurtosis=kurtosis(SGMUR))   # calculate Coefficient of kurtosis
data.frame(skewness=skewness(SGMUR))  # calculate Coefficient of skewness
data.frame(Standard.deviation=sd(SGMUR))

# Data Preprocessing
y_lab <- "Monthly Unemployment rate in SG"   # input name of data
NNAR_Model<- TRUE     #create new model (TRUE/FALSE)
Number_Neural<-5  # Number of Neural For model NNAR Model
#processing on data (input data)
rows <- NROW(SGMUR) # calculate number of rows in time series (number of months)
validation_data_months = 3
training_data<-SGMUR[1:(rows-validation_data_months)] # Training data
testing_data<-SGMUR[(rows-validation_data_months+1):rows] #Testing data

if(NNAR_Model==TRUE){
  data_series<-ts(training_data)
  model_NNAR<-nnetar(data_series, size = Number_Neural)
  saveRDS(model_NNAR, file = "model_NNAR.RDS")
  my_model <- readRDS("model_NNAR.RDS")
  model_NNAR
}

frequency<-"months"
Actual_date_interval <- c("2020/01/01","2021/08/31")
Forecast_date_interval <- c("2021/09/01","2021/10/31")

AD<-fulldate<-seq(as.Date(Actual_date_interval[1]),as.Date(Actual_date_interval[2]), frequency)  #input range for actual date
FD<-seq(as.Date(Forecast_date_interval[1]),as.Date(Forecast_date_interval[2]), frequency)  #input range forecasting date

N_forecasting_months<-nrow(data.frame(FD))  #calculate number of months that you want to forecast
validation_dates<-tail(AD,validation_data_months) # select validation_dates

forecasting_NNAR <- forecast(model_NNAR, h=N_forecasting_months+validation_data_months)
validation_forecast<-head(forecasting_NNAR$mean,validation_data_months)
MSE_Per_Month<-round((testing_data-validation_forecast)^2, 3)

MSE_Mean_All<-paste(round(mean(MSE_Per_Month),3),"% average MSE for",validation_data_months,frequency,y_lab,sep=" ")
print(MSE_Mean_All)

MSE_NNAR_Model<-paste(MSE_Per_Month ,"%")
print(ascii(data.frame(date_NNAR=validation_dates,actual_data=testing_data,forecasting_NNAR=validation_forecast,MSE_NNAR_Model)), type = "rest")
print(ascii(data.frame(FD,forecasting_by_NNAR=tail(forecasting_NNAR$mean,N_forecasting_months))), type = "rest")
plot(forecasting_NNAR,xlab = paste ("Time in", frequency ,y_lab , sep=" "), ylab=y_lab)
x1_test <- ts(testing_data, start =(rows-validation_data_months+1) )
lines(x1_test, col='red',lwd=2)



####NNAR model for UK####
plot(UKMUR)
hist(UKMUR)
summary(UKMUR)
data.frame(kurtosis=kurtosis(UKMUR))   # calculate Coefficient of kurtosis
data.frame(skewness=skewness(UKMUR))  # calculate Coefficient of skewness
data.frame(Standard.deviation=sd(UKMUR))

# Data Preprocessing
y_lab <- "Monthly Unemployment rate in UK"   # input name of data
NNAR_Model<- TRUE     #create new model (TRUE/FALSE)
Number_Neural<-5  # Number of Neural For model NNAR Model
#processing on data (input data)
rows <- NROW(UKMUR) # calculate number of rows in time series (number of months)
validation_data_months = 3
training_data<-UKMUR[1:(rows-validation_data_months)] # Training data
testing_data<-UKMUR[(rows-validation_data_months+1):rows] #Testing data

if(NNAR_Model==TRUE){
  data_series<-ts(training_data)
  model_NNAR<-nnetar(data_series, size = Number_Neural)
  saveRDS(model_NNAR, file = "model_NNAR.RDS")
  my_model <- readRDS("model_NNAR.RDS")
  model_NNAR
}

frequency<-"months"
Actual_date_interval <- c("2020/01/01","2021/08/31")
Forecast_date_interval <- c("2021/09/01","2021/10/31")

AD<-fulldate<-seq(as.Date(Actual_date_interval[1]),as.Date(Actual_date_interval[2]), frequency)  #input range for actual date
FD<-seq(as.Date(Forecast_date_interval[1]),as.Date(Forecast_date_interval[2]), frequency)  #input range forecasting date

N_forecasting_months<-nrow(data.frame(FD))  #calculate number of months that you want to forecast
validation_dates<-tail(AD,validation_data_months) # select validation_dates

forecasting_NNAR <- forecast(model_NNAR, h=N_forecasting_months+validation_data_months)
validation_forecast<-head(forecasting_NNAR$mean,validation_data_months)
MSE_Per_Month<-round((testing_data-validation_forecast)^2, 3)

MSE_Mean_All<-paste(round(mean(MSE_Per_Month),3),"% average MSE for",validation_data_months,frequency,y_lab,sep=" ")
print(MSE_Mean_All)

MSE_NNAR_Model<-paste(MSE_Per_Month ,"%")
print(ascii(data.frame(date_NNAR=validation_dates,actual_data=testing_data,forecasting_NNAR=validation_forecast,MSE_NNAR_Model)), type = "rest")
print(ascii(data.frame(FD,forecasting_by_NNAR=tail(forecasting_NNAR$mean,N_forecasting_months))), type = "rest")
plot(forecasting_NNAR,xlab = paste ("Time in", frequency ,y_lab , sep=" "), ylab=y_lab)
x1_test <- ts(testing_data, start =(rows-validation_data_months+1) )
lines(x1_test, col='red',lwd=2)

####NNAR model for US####
plot(USMUR)
hist(USMUR)
summary(USMUR)
data.frame(kurtosis=kurtosis(USMUR))   # calculate Coefficient of kurtosis
data.frame(skewness=skewness(USMUR))  # calculate Coefficient of skewness
data.frame(Standard.deviation=sd(USMUR))

# Data Preprocessing
y_lab <- "Monthly Unemployment rate in UK"   # input name of data
NNAR_Model<- TRUE     #create new model (TRUE/FALSE)
Number_Neural<-5  # Number of Neural For model NNAR Model
#processing on data (input data)
rows <- NROW(USMUR) # calculate number of rows in time series (number of months)
validation_data_months = 3
training_data<-USMUR[1:(rows-validation_data_months)] # Training data
testing_data<-USMUR[(rows-validation_data_months+1):rows] #Testing data

if(NNAR_Model==TRUE){
  data_series<-ts(training_data)
  model_NNAR<-nnetar(data_series, size = Number_Neural)
  saveRDS(model_NNAR, file = "model_NNAR.RDS")
  my_model <- readRDS("model_NNAR.RDS")
  model_NNAR
}

frequency<-"months"
Actual_date_interval <- c("2020/01/01","2021/08/31")
Forecast_date_interval <- c("2021/09/01","2021/10/31")

AD<-fulldate<-seq(as.Date(Actual_date_interval[1]),as.Date(Actual_date_interval[2]), frequency)  #input range for actual date
FD<-seq(as.Date(Forecast_date_interval[1]),as.Date(Forecast_date_interval[2]), frequency)  #input range forecasting date

N_forecasting_months<-nrow(data.frame(FD))  #calculate number of months that you want to forecast
validation_dates<-tail(AD,validation_data_months) # select validation_dates

forecasting_NNAR <- forecast(model_NNAR, h=N_forecasting_months+validation_data_months)
validation_forecast<-head(forecasting_NNAR$mean,validation_data_months)
MSE_Per_Month<-round((testing_data-validation_forecast)^2, 3)

MSE_Mean_All<-paste(round(mean(MSE_Per_Month),3),"% average MSE for",validation_data_months,frequency,y_lab,sep=" ")
print(MSE_Mean_All)

MSE_NNAR_Model<-paste(MSE_Per_Month ,"%")
print(ascii(data.frame(date_NNAR=validation_dates,actual_data=testing_data,forecasting_NNAR=validation_forecast,MSE_NNAR_Model)), type = "rest")
print(ascii(data.frame(FD,forecasting_by_NNAR=tail(forecasting_NNAR$mean,N_forecasting_months))), type = "rest")
plot(forecasting_NNAR,xlab = paste ("Time in", frequency ,y_lab , sep=" "), ylab=y_lab)
x1_test <- ts(testing_data, start =(rows-validation_data_months+1) )
lines(x1_test, col='red',lwd=2)



