# Adapted and modified from Epidemic.TA in 
# System for Forecasting COVID-19 Cases Using Time-Series and Neural Networks Models 

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

##Global variable##
all_countries_covid_data <- read_excel("formated_data.xlsx", sheet = "Daily New Cases")
original_data<-all_countries_covid_data$`Singapore`

y_lab <- "Daily Covid 19 Infection cases in Singapore"   # input name of data
Actual_date_interval <- c("2020/01/01","2021/08/31")
Forecast_date_interval <- c("2021/10/01","2021/10/31")

frequency<-"days"
country.name <- "Singapore"
# Data Preparation & calculate some of statistics measures
summary(original_data) # Summary your time series
# calculate standard deviation 
data.frame(kurtosis=kurtosis(original_data))   # calculate Cofficient of kurtosis
data.frame(skewness=skewness(original_data))  # calculate Cofficient of skewness
data.frame(Standard.deviation=sd(original_data))

#processing on data (input data)
rows <- NROW(original_data) # calculate number of rows in time series (number of days)
training_data<-original_data[1:517] # Training data
testing_data_start_index = 518
testing_data<-original_data[testing_data_start_index:609] #testing data
sep_data_days <-30

jun_actual_data<-original_data[518:547]
jul_actual_data<-original_data[548:578]
aug_actual_data<-original_data[579:609]
sep_actual_data<-original_data[610:639]

jun_forecast_input<-training_data
jul_forecast_input<-c(jun_forecast_input, jun_actual_data)
aug_forecast_input<-c(jul_forecast_input, jul_actual_data)
sep_forecast_input<-c(aug_forecast_input, aug_actual_data)
oct_forecast_input<-c(sep_forecast_input, sep_actual_data)

forecast_input_list<-list(jun_forecast_input, jul_forecast_input, aug_forecast_input, sep_forecast_input)
actual_data_list<-list(jun_actual_data, jul_actual_data, aug_actual_data, sep_actual_data)

AD<-all_countries_covid_data$Date # Input range for actual date
FD<-seq(as.Date(Forecast_date_interval[1]), as.Date(Forecast_date_interval[2]), frequency)  # Input range forecasting date
N_forecasting_days<-nrow(data.frame(FD))  # Number of days to forecast
validation_dates<-tail(AD, sep_data_days) # Number of validation dates
validation_data_by_name<-weekdays(validation_dates) # Name of validation dates
forecasting_data_by_name<-weekdays(FD)  # Names of forecasting dates
validation_data_days<-NROW(testing_data)


#NNAR Model 
calibrate_NN_size = FALSE


if (calibrate_NN_size){
  neural<-1:50
  mse_result = c()
  for (j in neural){
    data_series<-ts(training_data)
    Number_Neural<-j    # Number of Neural For model NNAR Model
    model_NNAR<-nnetar(data_series, size=Number_Neural)
    accuracy(model_NNAR)  # Accuracy on training data #Print Model Parameters
    model_NNAR
    sse_vec<-c()
    # Testing Data Evaluation
    for (i in 1:4){
      input_data<-forecast_input_list[i][[1]]
      temp_model<-nnetar(input_data, size = Number_Neural, model=model_NNAR)
      actual_data<-actual_data_list[i][[1]]
      one_month_ahead_forecast <- forecast(temp_model, h=NROW(actual_data))
      sse<-(actual_data-one_month_ahead_forecast$mean)^2
      plot(c(input_data, one_month_ahead_forecast$mean),xlab = paste ("Time in", frequency ,y_lab , sep=" "), ylab=y_lab, type='l')
      x1_test <- ts(actual_data, start=NROW(input_data)+1 )
      lines(x1_test, col='red',lwd=2)
      sse_vec<-c(sse_vec, sse)
    }
    
    MSE_Per_Day<-round(mean(sse_vec), 3)
    paste ("MSE % For ",validation_data_days,frequency,"by using NNAR Model for  ==> ",y_lab, sep=" ")
    MSE_Mean_All<-paste(MSE_Per_Day," MSE ",validation_data_days,frequency,y_lab,sep=" ")
    MSE_Mean_All_NNAR<-round(MSE_Per_Day, 3)
    MSE_NNAR<-paste(round(MSE_Per_Day,3))
    MSE_NNAR_Model<-paste(MSE_Per_Day)
    paste (" MSE that's Error of Forecasting for ",validation_data_days," days in NNAR Model for  ==> ",y_lab, sep=" ")
    paste(MSE_Mean_All)
    paste ("MSE that's Error of Forecasting day by day for ",validation_data_days," days in NNAR Model for  ==> ",y_lab, sep=" ")
    print(ascii(data.frame(date_NNAR=validation_dates,validation_data_by_name,actual_data=actual_data,forecasting_NNAR=one_month_ahead_forecast,sse)), type = "rest")
    
    #temp<-data.frame(validation_dates, validation_forecast)
    #write.csv(temp, 'nnar_forecast.csv')
    
    NNAR_forecast_model<-nnetar(oct_forecast_input, size = Number_Neural, model=model_NNAR)
    NNAR_forecast <- forecast(NNAR_forecast_model, h=N_forecasting_days)
    print(ascii(data.frame(FD,forecating_date=forecasting_data_by_name,forecasting_by_NNAR=NNAR_forecast$mean)), type = "rest")
    plot(c(oct_forecast_input, NNAR_forecast$mean),xlab = paste ("Time in", frequency ,y_lab , sep=" "), ylab=y_lab, type='l')
    MSE_Mean_All_NNAR
    
    mse_result<-c(mse_result, MSE_Mean_All_NNAR)
  }
  mse_result
  min(mse_result)
}

data_series<-ts(training_data)
Number_Neural<-3    # Number of Neural For model NNAR Model
model_NNAR<-nnetar(data_series, size=Number_Neural)
accuracy(model_NNAR)  # Accuracy on training data #Print Model Parameters
model_NNAR
sse_vec<-c()
# Testing Data Evaluation
for (i in 1:4){
  input_data<-forecast_input_list[i][[1]]
  temp_model<-nnetar(input_data, size = Number_Neural, model=model_NNAR)
  actual_data<-actual_data_list[i][[1]]
  one_month_ahead_forecast <- forecast(temp_model, h=NROW(actual_data))
  sse<-(actual_data-one_month_ahead_forecast$mean)^2
  plot(c(input_data, one_month_ahead_forecast$mean),xlab = paste ("Time in", frequency ,y_lab , sep=" "), ylab=y_lab, type='l')
  x1_test <- ts(actual_data, start=NROW(input_data)+1 )
  lines(x1_test, col='red',lwd=2)
  sse_vec<-c(sse_vec, sse)
}

MSE_Per_Day<-round(mean(sse_vec), 3)
paste ("MSE % For ",validation_data_days,frequency,"by using NNAR Model for  ==> ",y_lab, sep=" ")
MSE_Mean_All<-paste(MSE_Per_Day," MSE ",validation_data_days,frequency,y_lab,sep=" ")
MSE_Mean_All_NNAR<-round(MSE_Per_Day, 3)
MSE_NNAR<-paste(round(MSE_Per_Day,3))
MSE_NNAR_Model<-paste(MSE_Per_Day)
paste (" MSE that's Error of Forecasting for ",validation_data_days," days in NNAR Model for  ==> ",y_lab, sep=" ")
paste(MSE_Mean_All)
paste ("MSE that's Error of Forecasting day by day for ",validation_data_days," days in NNAR Model for  ==> ",y_lab, sep=" ")
print(ascii(data.frame(date_NNAR=validation_dates,validation_data_by_name,actual_data=actual_data,forecasting_NNAR=one_month_ahead_forecast,sse)), type = "rest")

#temp<-data.frame(validation_dates, validation_forecast)
#write.csv(temp, 'nnar_forecast.csv')

NNAR_forecast_model<-nnetar(oct_forecast_input, size = Number_Neural, model=model_NNAR)
NNAR_forecast <- forecast(NNAR_forecast_model, h=N_forecasting_days)
print(ascii(data.frame(FD,forecating_date=forecasting_data_by_name,forecasting_by_NNAR=NNAR_forecast$mean)), type = "rest")
plot(c(oct_forecast_input, NNAR_forecast$mean),xlab = paste ("Time in", frequency ,y_lab , sep=" "), ylab=y_lab, type='l')
MSE_Mean_All_NNAR


# BATS Model (Box-Cox Transformation, ARMA residuals, Trend and Seasonality)
# Data Modeling
data_series<-ts(training_data) # make your data to time series
autoplot(data_series ,xlab=paste ("Time in", frequency, sep=" "), ylab = y_lab, main=paste ("Actual Data :", y_lab, sep=" "))
model_bats<-bats(data_series)
accuracy(model_bats)  # accuracy on training data
# Print Model Parameters
model_bats
#ploting BATS Model
plot(model_bats,xlab = paste ("Time in", frequency ,y_lab , sep=" "))
# Testing Data Evaluation
sse_vec<-c()
# Testing Data Evaluation
for (i in 1:4){
  input_data<-forecast_input_list[i][[1]]
  temp_model<-bats(input_data, model=model_bats)
  actual_data<-actual_data_list[i][[1]]
  one_month_ahead_forecast <- predict(temp_model, h=NROW(actual_data))
  sse<-(actual_data-one_month_ahead_forecast$mean)^2
  plot(c(input_data, one_month_ahead_forecast$mean),xlab = paste ("Time in", frequency ,y_lab , sep=" "), ylab=y_lab, type='l')
  x1_test <- ts(actual_data, start=NROW(input_data)+1 )
  lines(x1_test, col='red',lwd=2)
  sse_vec<-c(sse_vec, sse)
}

MSE_Per_Day<-round(mean(sse_vec), 3)
paste ("MSE % For ",validation_data_days,frequency,"by using bats Model for  ==> ", y_lab, sep=" ")
MSE_Mean_All.bats_Model<-round(MSE_Per_Day, 3)
MSE_Mean_All.bats<-paste(round(MSE_Per_Day, 3), " MSE ", validation_data_days, frequency, y_lab, sep=" ")
MSE_bats<-paste(round(MSE_Per_Day, 3))
MSE_bats_Model<-paste(MSE_Per_Day)
paste (" MSE that's Error of Forecasting for ", validation_data_days, " days in bats Model for  ==> ", y_lab, sep=" ")
paste(MSE_Mean_All.bats)
paste ("MSE that's Error of Forecasting day by day for ", validation_data_days, " days in bats Model for  ==> ", y_lab, sep=" ")
print(ascii(data.frame(date_bats=validation_dates, validation_data_by_name, actual_data=actual_data, forecasting_bats=one_month_ahead_forecast$mean, sse)), type = "rest")

BATS_forecast_model<-bats(oct_forecast_input, model=model_bats)
BATS_forecast <- forecast(BATS_forecast_model, h=N_forecasting_days)
print(ascii(data.frame(FD,forecating_date=forecasting_data_by_name,forecasting_by_NNAR=BATS_forecast$mean)), type = "rest")
plot(c(oct_forecast_input, BATS_forecast$mean),xlab = paste ("Time in", frequency ,y_lab , sep=" "), ylab=y_lab, type='l')
MSE_Mean_All.bats



# TBATS Model (Trigonometric Seasonal, Box-Cox Transformation, ARMA residuals, Trend and Seasonality)
# Data Modeling
data_series<-ts(training_data)
model_TBATS<-tbats(data_series, use.box.cox=FALSE, seasonal.periods=c(6), use.damped.trend=FALSE)
accuracy(model_TBATS)  # accuracy on training data
# Print Model Parameters
model_TBATS

plot(model_TBATS,xlab = paste ("Time in", frequency, y_lab , sep=" "), ylab=y_lab)
# Testing Data Evaluation
sse_vec<-c()
# Testing Data Evaluation
for (i in 1:4){
  input_data<-forecast_input_list[i][[1]]
  temp_model<-tbats(input_data, model=model_TBATS)
  actual_data<-actual_data_list[i][[1]]
  one_month_ahead_forecast <- predict(temp_model, h=NROW(actual_data))
  sse<-(actual_data-one_month_ahead_forecast$mean)^2
  plot(c(input_data, one_month_ahead_forecast$mean),xlab = paste ("Time in", frequency ,y_lab , sep=" "), ylab=y_lab, type='l')
  x1_test <- ts(actual_data, start=NROW(input_data)+1 )
  lines(x1_test, col='red',lwd=2)
  sse_vec<-c(sse_vec, sse)
}

MSE_Per_Day<-round(mean(sse_vec), 3)
paste ("MSE % For ",validation_data_days,frequency,"by using TBATS Model for  ==> ", y_lab, sep=" ")
MSE_Mean_All.TBATS_Model<-round(MSE_Per_Day, 3)
MSE_Mean_All.TBATS<-paste(round(MSE_Per_Day, 3), "MSE ", validation_data_days, frequency, y_lab, sep=" ")
MSE_TBATS<-paste(round(MSE_Per_Day,3))
MSE_TBATS_Model<-paste(MSE_Per_Day)
paste (" MSE that's Error of Forecasting for ", validation_data_days, " days in TBATS Model for  ==> ", y_lab, sep=" ")
paste(MSE_Mean_All.TBATS, "%")
paste ("MSE that's Error of Forecasting day by day for ", validation_data_days, " days in TBATS Model for  ==> ", y_lab, sep=" ")
print(ascii(data.frame(date_TBATS=validation_dates,validation_data_by_name, actual_data=actual_data, forecasting_TBATS=one_month_ahead_forecast$mean, sse)), type = "rest")

TBATS_forecast_model<-tbats(oct_forecast_input, model=model_TBATS)
TBATS_forecast <- forecast(TBATS_forecast_model, h=N_forecasting_days)
print(ascii(data.frame(FD, forecating_date=forecasting_data_by_name, forecasting_by_TBATS=TBATS_forecast$mean, Lower=TBATS_forecast$lower, Upper=TBATS_forecast$upper)), type = "rest")
plot(c(oct_forecast_input, TBATS_forecast$mean),xlab = paste ("Time in", frequency ,y_lab , sep=" "), ylab=y_lab, type='l')
MSE_Mean_All.TBATS



## Holt's linear trend
# Data Modeling
data_series<-ts(training_data)
model_holt<-holt(data_series, h=1, lambda = "auto")
accuracy(model_holt)  # accuracy on training data
# Print Model Parameters
summary(model_holt$model)
# Testing Data Evaluation
sse_vec<-c()
for (i in 1:4){
  input_data<-forecast_input_list[i][[1]]
  actual_data<-actual_data_list[i][[1]]
  one_month_ahead_forecast<-forecast(input_data, h = NROW(actual_data), model=model_holt, use.initial.values=TRUE)
  sse<-(actual_data-one_month_ahead_forecast$mean)^2
  plot(c(input_data, one_month_ahead_forecast$mean),xlab = paste ("Time in", frequency ,y_lab , sep=" "), ylab=y_lab, type='l')
  x1_test <- ts(actual_data, start=NROW(input_data)+1 )
  lines(x1_test, col='red',lwd=2)
  sse_vec<-c(sse_vec, sse)
}
MSE_Per_Day<-round(mean(sse_vec), 3)
paste ("MSE % For ",validation_data_days,frequency,"by using holt Model for  ==> ", y_lab, sep=" ")
MSE_Mean_All.Holt_Model<-round(MSE_Per_Day, 3)
MSE_Mean_All.Holt<-paste(round(MSE_Per_Day, 3)," MSE ",validation_data_days,frequency,y_lab,sep=" ")
MSE_holt<-paste(round(MSE_Per_Day, 3))
MSE_holt_Model<-paste(MSE_Per_Day)
paste (" MSE that's Error of Forecasting for ", validation_data_days, " days in holt Model for  ==> ", y_lab, sep=" ")
paste(MSE_Mean_All.Holt,"%")
paste ("MSE that's Error of Forecasting day by day for ", validation_data_days, " days in holt Model for  ==> ", y_lab, sep=" ")
print(ascii(data.frame(date_holt=validation_dates, validation_data_by_name, actual_data=actual_data, forecasting_holt=one_month_ahead_forecast$mean, sse)), type = "rest")

HLT_forecast <- forecast(oct_forecast_input, h=N_forecasting_days, model=model_holt, use.initial.values=TRUE)
print(ascii(data.frame(FD, forecating_date=forecasting_data_by_name, forecasting_by_holt=HLT_forecast$mean, Lower=HLT_forecast$lower, Upper=HLT_forecast$upper)), type = "rest")
plot(c(oct_forecast_input, HLT_forecast$mean),xlab = paste ("Time in", frequency ,y_lab , sep=" "), ylab=y_lab, type='l')
MSE_Mean_All.Holt



#Auto arima model
##################
paste ("tests For Check Stationarity in series  ==> ", y_lab, sep=" ")
kpss.test(data_series) # kpss test
pp.test(data_series)   # pp test
adf.test(data_series)  # adf test
ndiffs(data_series)    # Doing first diffrencing on data
#Taking the first difference
diff1_x1<-diff(data_series)
autoplot(diff1_x1, xlab = paste ("Time in", frequency, y_lab, sep=" "), ylab=y_lab, main = "1nd differenced series")
##Testing the stationary of the first differenced series
paste ("tests For Check Stationarity in series after taking first differences in  ==> ", y_lab, sep=" ")
kpss.test(diff1_x1)   # kpss test after taking first differences
pp.test(diff1_x1)     # pp test after taking first differences
adf.test(diff1_x1)    # adf test after taking first differences
#Taking the second difference
diff2_x1=diff(diff1_x1)
autoplot(diff2_x1, xlab = paste ("Time in", frequency, y_lab, sep=" "), ylab=y_lab, main = "2nd differenced series")
##Testing the stationary of the first differenced series
paste ("tests For Check Stationarity in series after taking Second differences in", y_lab, sep=" ")
kpss.test(diff2_x1)   # applay kpss test after taking Second differences
pp.test(diff2_x1)     # applay pp test after taking Second differences
adf.test(diff2_x1)    # applay adf test after taking Second differences
####Fitting an ARIMA Model
#1. Using auto arima function
model1 <- auto.arima(data_series,stepwise=FALSE, approximation=FALSE, trace=T, test = c("kpss", "adf", "pp"))
model1

#2. Using ACF and PACF Function
acf(diff2_x1, xlab = paste ("Time in", frequency, y_lab, sep=" "), ylab=y_lab, main=paste("ACF-2nd differenced series ", y_lab, sep=" ", lag.max=20))    # plot ACF "auto correlation function after taking second diffrences
pacf(diff2_x1, xlab = paste ("Time in", frequency, y_lab, sep=" "), ylab=y_lab, main=paste("PACF-2nd differenced series ", y_lab, sep=" ", lag.max=20))   # plot PACF " Partial auto correlation function after taking second diffrences
           
x1_model1<- model1 # Run Best model of auto arima  for forecasting
x1_model1  # Show result of best model of auto arima 
paste ("accuracy of autoarima Model For  ==> ",y_lab, sep=" ")
accuracy(x1_model1)  # Acuracy of best model from auto arima
x1_model1$x          # show result of best model from auto.arima 
checkresiduals(x1_model1, xlab = paste ("Time in", frequency ,y_lab , sep=" "), ylab=y_lab)  # checkresiduals from best model from using auto arima 
paste("Ljung-Box test for   ==> ", y_lab, sep=" ")
Box.test(x1_model1$residuals^2, lag=20, type="Ljung-Box")   # Ljung-Box test on residuals

jarque.bera.test(x1_model1$residuals)  # Do Jarque-Bera test on residuals 
#Actual Vs Fitted
plot(data_series, col='red', lwd=2, main="Actual vs Fitted Plot", xlab='Time in (days)', ylab=y_lab) # plot actual and Fitted model 
lines(fitted(x1_model1), col='black')
#Test data
sse_vec<-c()
for (i in 1:4){
  input_data<-forecast_input_list[i][[1]]
  temp_model<-Arima(input_data, model=x1_model1)
  actual_data<-actual_data_list[i][[1]]
  one_month_ahead_forecast <- forecast(temp_model, h=NROW(actual_data))
  sse<-(actual_data-one_month_ahead_forecast$mean)^2
  plot(c(input_data, one_month_ahead_forecast$mean),xlab = paste ("Time in", frequency ,y_lab , sep=" "), ylab=y_lab, type='l')
  x1_test <- ts(actual_data, start=NROW(input_data)+1 )
  lines(x1_test, col='red',lwd=2)
  sse_vec<-c(sse_vec, sse)
}

MSE_Per_Day<-round(mean(sse_vec), 3)

paste ("MSE % For ",validation_data_days,frequency,"by using bats Model for  ==> ", y_lab, sep=" ")
MSE_Mean_All.ARIMA_Model<-round(MSE_Per_Day, 3)
MSE_Mean_All.ARIMA<-paste(round(MSE_Per_Day, 3)," MSE ", validation_data_days, frequency, y_lab, sep=" ")
MSE_auto_arima<-paste(round(MSE_Per_Day, 3))
MSE_auto.arima_Model<-paste(MSE_Per_Day)
paste (" MSE that's Error of Forecasting for ", validation_data_days, " days in bats Model for  ==> ",y_lab, sep=" ")
paste(MSE_Mean_All.ARIMA, "%")
paste ("MSE that's Error of Forecasting day by day for ",validation_data_days," days in bats Model for  ==> ",y_lab, sep=" ")
print(ascii(data.frame(date_auto.arima=validation_dates,validation_data_by_name,actual_data=actual_data, forecasting_auto.arima=one_month_ahead_forecast$mean, sse)), type = "rest")

ARIMA_forecast_model<-Arima(input_data, model=x1_model1)
ARIMA_forecast <- forecast(ARIMA_forecast_model, h=N_forecasting_days)
print(ascii(data.frame(FD,forecating_date=forecasting_data_by_name,forecasting_by_auto.arima=ARIMA_forecast$mean,Lower=ARIMA_forecast$lower, Upper=ARIMA_forecast$upper)), type = "rest")
plot(c(oct_forecast_input, ARIMA_forecast$mean),xlab = paste ("Time in", frequency ,y_lab , sep=" "), ylab=y_lab, type='l')
MSE_Mean_All.ARIMA



#VAR model
DNC <- read_excel("formated_data.xlsx", sheet = "Daily New Cases")
SGDNC<-DNC$Singapore[1:517]
UKDNC<-DNC$`United Kingdom`[1:517]
USDNC<-DNC$`United States`[1:517]

#Loading Monthly Unemployment Rate Data
MUR <- read_excel("formated_data.xlsx", sheet = "Monthly Unemployment Rate")
SGMUR<-MUR$Singapore[1:517]
UKMUR<-MUR$`United Kingdom`[1:517]
USMUR<-MUR$`United States`[1:517]

#Loading Government Response Index Data
GRI <- read_excel("formated_data.xlsx", sheet = "Government Response Index")
SGGRI<-GRI$Singapore[1:517]
UKGRI<-GRI$`United Kingdom`[1:517]
USGRI<-GRI$`United States`[1:517]

#Loading Containment Health Index Data
CHI <- read_excel("formated_data.xlsx", sheet = "Containment Health Index")
SGCHI<-CHI$Singapore[1:517]
UKCHI<-CHI$`United Kingdom`[1:517]
USCHI<-CHI$`United States`[1:517]

#Loading Economic Support Index Data
ESI <- read_excel("formated_data.xlsx", sheet = "Economic Support Index")
SGESI<-ESI$Singapore[1:517]
UKESI<-ESI$`United Kingdom`[1:517]
USESI<-ESI$`United States`[1:517]


# Data prep for VAR and VECM models
jun_actual_data<-cbind(DNC$Singapore[518:547], MUR$Singapore[518:547], GRI$Singapore[518:547], CHI$Singapore[518:547], ESI$Singapore[518:547])
jul_actual_data<-cbind(DNC$Singapore[548:578], MUR$Singapore[548:578], GRI$Singapore[548:578], CHI$Singapore[548:578], ESI$Singapore[548:578])
aug_actual_data<-cbind(DNC$Singapore[579:609], MUR$Singapore[579:609], GRI$Singapore[579:609], CHI$Singapore[579:609], ESI$Singapore[579:609])
sep_actual_data<-cbind(DNC$Singapore[610:639], MUR$Singapore[610:639], GRI$Singapore[610:639], CHI$Singapore[610:639], ESI$Singapore[610:639])


jun_forecast_input<-cbind(DNC$Singapore[1:517], MUR$Singapore[1:517], GRI$Singapore[1:517], CHI$Singapore[1:517], ESI$Singapore[1:517])
jul_forecast_input<-c(jun_forecast_input, jun_actual_data)
aug_forecast_input<-c(jul_forecast_input, jul_actual_data)
sep_forecast_input<-c(aug_forecast_input, aug_actual_data)
oct_forecast_input<-c(sep_forecast_input, sep_actual_data)

forecast_input_list<-list(jun_forecast_input, jul_forecast_input, aug_forecast_input, sep_forecast_input)
actual_data_list<-list(jun_actual_data, jul_actual_data, aug_actual_data, sep_actual_data)


SGdata = cbind(SGDNC, SGGRI, SGCHI, SGESI)
diff_SGdata = diff(SGdata)
dim(diff_SGdata)

#processing on data (input data)
rows <- NROW(diff_SGdata) # calculate number of rows in time series (number of days)
training_data<-diff_SGdata
MTSplot(training_data)
ccm(training_data)
m0=VARorder(training_data)
m0$Mstat
names(m0)
m1=VAR(training_data, 8)
m2=refVAR(m1,thres=1.96)
MTSdiag(m1,adj=12)

validation_forecast<-VARpred(m1, validation_data_days)

sse_vec<-c()
# Testing Data Evaluation
for (i in 1:4){
  input_data<-forecast_input_list[i][[1]]
  SGdata.VAR <- VARselect(SGdata) #determining number of lags to be included in VAR model
  nlags <- SGdata.VAR$selection["SC(n)"]
  temp_model<-VAR(input_data, 2) # temp_model<- #Reuse existing var model here
  actual_data<-actual_data_list[i][[1]]
  one_month_ahead_forecast <- predict(temp_model, h=NROW(actual_data))
  sse<-(actual_data-one_month_ahead_forecast$mean)^2
  plot(c(input_data, one_month_ahead_forecast$mean),xlab = paste ("Time in", frequency ,y_lab , sep=" "), ylab=y_lab, type='l')
  x1_test <- ts(actual_data, start=NROW(input_data)+1 )
  lines(x1_test, col='red',lwd=2)
  sse_vec<-c(sse_vec, sse)
}
MSE_Per_Day<-round(mean(sse_vec), 3)



forecasting_VAR <- VARpred(m1, N_forecasting_days+validation_data_days)
MSE_Per_Day<-round((testing_data[, 1]-validation_forecast$pred[, 1])^2, 3)
paste ("MSE for ",validation_data_days,frequency,"by using VAR Model for  ==> ",y_lab, sep=" ")
MSE_Mean_All.VAR<-paste(round(mean(MSE_Per_Day),3)," MSE ",validation_data_days,frequency,y_lab,sep=" ")
MSE_Mean_All.VAR<-round(mean(MSE_Per_Day),3)
MSE_VAR<-paste(round(MSE_Per_Day,3))
MSE_VAR_Model<-paste(MSE_Per_Day )
paste ("MSE Error of Forecasting for ",validation_data_days," days in VAR Model for  ==> ",y_lab, sep=" ")
paste(MSE_Mean_All,"%")
paste ("MSE Error of Forecasting day by day for ",validation_data_days," days in VAR Model for  ==> ",y_lab, sep=" ")

print(ascii(data.frame(date_VAR=validation_dates,validation_data_by_name,actual_data=testing_data[, 1],forecasting_VAR=validation_forecast$pred[, 1], MSE_VAR_Model)), type = "rest")
print(ascii(data.frame(FD, forecating_date=forecasting_data_by_name, forecasting_by_VAR=tail(forecasting_VAR$pred[, 1], N_forecasting_days))), type = "rest")
plot(forecasting_VAR$pred[,1],xlab = paste ("Time in", frequency, y_lab, sep=" "), ylab=y_lab)
x1_test <- ts(testing_data, start = testing_data_start_index )
lines(x1_test, col='red',lwd=1)
graph5<-autoplot(forecasting_VAR, xlab = paste ("Time in", frequency, y_lab, sep=" "), ylab=y_lab)
graph5
MSE_Mean_All.VAR


colMeans(training_data) 
sqrt(apply(training_data,2,var))

#VECM Model
SGdata = cbind(SGDNC, SGGRI, SGCHI, SGESI)
SGdata.VAR <- VARselect(SGdata) #determining number of lags to be included in cointegration test and VCEM model
nlags <- SGdata.VAR$selection["SC(n)"]
nlags

#Perform cointegration test
SGdata.CA <- ca.jo(SGdata, ecdet="const", type="trace", K=nlags, spec="transitory")
summary(SGdata.CA)



#Convert from VCEM to VAR model
SGdata.VAR_convert <- vec2var(SGdata.CA, r=1)

sse_vec<-c()
# Testing Data Evaluation
for (i in 1:4){
  input_data<-forecast_input_list[i][[1]]
  SGdata.VAR <- VARselect(SGdata) #determining number of lags to be included in VAR model
  nlags <- SGdata.VAR$selection["SC(n)"]
  SGdata.reuse <- ca.jo(SGdata, ecdet="const", type="trace", K=nlags, spec="transitory")
  summary(SGdata.reuse)
  temp_model<-vec2var(SGdata.reuse, r=1) # temp_model<- #Reuse existing vecm model here
  actual_data<-actual_data_list[i][[1]]
  one_month_ahead_forecast <- predict(temp_model, h=NROW(actual_data))
  sse<-(actual_data-one_month_ahead_forecast$mean)^2
  plot(c(input_data, one_month_ahead_forecast$mean),xlab = paste ("Time in", frequency ,y_lab , sep=" "), ylab=y_lab, type='l')
  x1_test <- ts(actual_data, start=NROW(input_data)+1 )
  lines(x1_test, col='red',lwd=2)
  sse_vec<-c(sse_vec, sse)
}
MSE_Per_Day<-round(mean(sse_vec), 3)

#Forecasting
validation_forecast<-predict(SGdata.VAR_convert, validation_data_days)
forecasting_VECM <- predict(SGdata.VAR_convert, N_forecasting_days+validation_data_days)
MSE_Per_Day<-round((testing_data[, 1]-validation_forecast$pred[, 1])^2, 3)
paste ("MSE for ",validation_data_days,frequency,"by using VECM Model for  ==> ",y_lab, sep=" ")
MSE_Mean_All.VECM<-paste(round(mean(MSE_Per_Day),3)," MSE ",validation_data_days,frequency,y_lab,sep=" ")
MSE_Mean_All.VECM<-round(mean(MSE_Per_Day),3)
MSE_VECM<-paste(round(MSE_Per_Day,3))
MSE_VECM_Model<-paste(MSE_Per_Day )
paste ("MSE Error of Forecasting for ",validation_data_days," days in VECM Model for  ==> ",y_lab, sep=" ")
paste(MSE_Mean_All,"%")
paste ("MSE Error of Forecasting day by day for ",validation_data_days," days in VECM Model for  ==> ",y_lab, sep=" ")

print(ascii(data.frame(date_VAR=validation_dates,validation_data_by_name,actual_data=testing_data[, 1],forecasting_VAR=validation_forecast$pred[, 1], MSE_VAR_Model)), type = "rest")
print(ascii(data.frame(FD, forecating_date=forecasting_data_by_name, forecasting_by_VAR=tail(forecasting_VAR$pred[, 1], N_forecasting_days))), type = "rest")
plot(forecasting_VECM$pred[,1],xlab = paste ("Time in", frequency, y_lab, sep=" "), ylab=y_lab)
x1_test <- ts(testing_data, start = testing_data_start_index )
lines(x1_test, col='red',lwd=1)
graph6<-autoplot(forecasting_VECM, xlab = paste ("Time in", frequency, y_lab, sep=" "), ylab=y_lab)
graph6
MSE_Mean_All.VECM

1# Summary Table for MSE for all models
best_recommended_model <- min(MSE_Mean_All_NNAR, MSE_Mean_All.bats_Model, MSE_Mean_All.TBATS_Model, MSE_Mean_All.Holt_Model, MSE_Mean_All.ARIMA_Model, MSE_Mean_All.VAR, MSE_Mean_All.VECM)
paste("Choosing the best model based on MSE of forecasts by using NNAR, BATS, TBATS, Holt's Linear Model, ARIMA and VAR for ", y_lab, sep=" ")
best_recommended_model
x1<-if(best_recommended_model >= MSE_Mean_All.bats_Model) {paste("BATS Model")}
x2<-if(best_recommended_model >= MSE_Mean_All.TBATS_Model) {paste("TBATS Model")}
x3<-if(best_recommended_model >= MSE_Mean_All.Holt_Model) {paste("Holt Model")}
x4<-if(best_recommended_model >= MSE_Mean_All.ARIMA_Model) {paste("ARIMA Model")}
x5<-if(best_recommended_model >= MSE_Mean_All_NNAR) {paste("NNAR Model")}
x6<-if(best_recommended_model >= MSE_Mean_All.VAR) {paste("VAR Model")}
x7<-if(best_recommended_model >= MSE_Mean_All.VECM) {paste("VECM Model")}

panderOptions('table.split.table', Inf)
paste("Forecasting by using BATS Model  ==> ", y_lab, sep=" ")
print(ascii(data.frame(FD,forecating_date=forecasting_data_by_name, forecasting_by_bats=tail(BATS_forecast$mean, N_forecasting_days), lower=tail(BATS_forecast$lower,N_forecasting_days), Upper=tail(BATS_forecast$lower,N_forecasting_days))), type = "rest")
paste("Forecasting by using TBATS Model  ==> ", y_lab, sep=" ")
print(ascii(data.frame(FD,forecating_date=forecasting_data_by_name, forecasting_by_TBATS=tail(TBATS_forecast$mean, N_forecasting_days), Lower=tail(TBATS_forecast$lower,N_forecasting_days), Upper=tail(TBATS_forecast$upper,N_forecasting_days))), type = "rest")
paste("Forecasting by using Holt's Linear Trend Model  ==> ", y_lab, sep=" ")
print(ascii(data.frame(FD,forecating_date=forecasting_data_by_name, forecasting_by_holt=tail(HLT_forecast$mean, N_forecasting_days), Lower=tail(HLT_forecast$lower,N_forecasting_days), Upper=tail(HLT_forecast$upper,N_forecasting_days))), type = "rest")
paste("Forecasting by using ARIMA Model  ==> ", y_lab, sep=" ")
print(ascii(data.frame(FD,forecating_date=forecasting_data_by_name, forecasting_by_auto.arima=tail(ARIMA_forecast$mean, N_forecasting_days), Lower=tail(ARIMA_forecast$lower, N_forecasting_days), Upper=tail(ARIMA_forecast$upper, N_forecasting_days))), type = "rest")
paste("Forecasting by using NNAR Model  ==> ", y_lab, sep=" ")
print(ascii(data.frame(FD,forecating_date=forecasting_data_by_name, forecasting_by_NNAR=tail(NNAR_forecast$mean, N_forecasting_days))), type = "rest")
paste("Forecasting by using VAR Model  ==> ", y_lab, sep=" ")
print(ascii(data.frame(FD,forecasting_date=forecasting_data_by_name, forecasting_by_VAR=tail(forecasting_VAR$mean, N_forecasting_days), Lower=tail(forecasting_VAR$lower, N_forecasting_days), Upper=tail(forecasting_VAR$upper, N_forecasting_days))), type = "rest")
paste("Forecasting by using VECM Model  ==> ", y_lab, sep=" ")
print(ascii(data.frame(FD,forecasting_date=forecasting_data_by_name, forecasting_by_VECM=tail(forecasting_VECM$mean, N_forecasting_days), Lower=tail(forecasting_VECM$lower, N_forecasting_days), Upper=tail(forecasting_VECM$upper, N_forecasting_days))), type = "rest")
result<-c(x1, x2, x3, x4, x5, x6, x7)
table.error<-data.frame(country.name, NNAR.model=MSE_Mean_All_NNAR, BATS.Model=MSE_Mean_All.bats_Model, TBATS.Model=MSE_Mean_All.TBATS_Model, Holt.Model=MSE_Mean_All.Holt_Model, ARIMA.Model=MSE_Mean_All.ARIMA_Model,  VAR.Model=MSE_Mean_All.VAR, VECM.Model=MSE_Mean_All.VECM, Best.Model=result)

  
print(ascii(table(table.error)), type = "rest")
MSE.Value<-c(MSE_Mean_All_NNAR, MSE_Mean_All.bats_Model, MSE_Mean_All.TBATS_Model, MSE_Mean_All.Holt_Model, MSE_Mean_All.ARIMA_Model, MSE_Mean_All_VAR)
Model<-c("NNAR.model","BATS.Model","TBATS.Model","Holt.Model","ARIMA.Model" , "VAR Model")
channel_data<-data.frame(Model,MSE.Value)
# Normally, the entire expression below would be assigned to an object, but we're
# going bare bones here.
ggplot(channel_data, aes(x = Model, y = MSE.Value)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = MSE.Value)) +  # x AND y INHERITED. WE JUST NEED TO SPECIFY "label"
  coord_flip() +
  scale_y_continuous(expand = c(0, 0))

message("System finished Modelling and Forecasting  by using BATS, TBATS, Holt's Linear Trend, and ARIMA Model ==>",y_lab, sep=" ")
message(" Thank you for using our System For Modelling and Forecasting ==> ", y_lab, sep=" ")

