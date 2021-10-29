# Adapted and modified from Epidemic.TA in 
# System for Forecasting COVID-19 Cases Using Time-Series and Neural Networks Models 
library(fGarch)
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
set.seed(1)
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



#NNAR Model 
data_series<-ts(training_data)
#Number_Neural<-3    # Number of Neural For model NNAR Model
model_NNAR<-nnetar(data_series)
accuracy(model_NNAR)  # Accuracy on training data #Print Model Parameters
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
lines(x1_test, col='red',lwd=2)

MSE_Per_Day<-round(mean(se_vec), 3)
paste ("MSE % For ",validation_data_days,frequency,"by using NNAR Model for  ==> ",y_lab, sep=" ")
MSE_Mean_All<-paste(MSE_Per_Day," MSE ",validation_data_days,frequency,y_lab,sep=" ")
MSE_Mean_All_NNAR<-round(MSE_Per_Day, 3)
MSE_NNAR<-paste(round(MSE_Per_Day,3))
MSE_NNAR_Model<-paste(MSE_Per_Day)
paste (" MSE that's Error of Forecasting for ",validation_data_days," days in NNAR Model for  ==> ",y_lab, sep=" ")
paste(MSE_Mean_All)

#temp<-data.frame(validation_dates, validation_forecast)
#write.csv(temp, 'nnar_forecast.csv')

NNAR_forecast_model<-nnetar(original_data, model=model_NNAR)
NNAR_forecast <- forecast(NNAR_forecast_model, h=N_forecasting_days)
print(ascii(data.frame(FD,forecating_date=forecasting_data_by_name, forecasting_by_NNAR=NNAR_forecast$mean)), type = "rest")
plot(c(original_data, NNAR_forecast$mean), xlab = paste ("Time in", frequency ,y_lab , sep=" "), ylab=y_lab, type='l')
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
one_day_ahead_forecast <- predict(model_bats, h=1)
se_1<-(testing_data[1]-one_day_ahead_forecast$mean)^2
se_vec<-c(se_1)
forecast_vec = c(one_day_ahead_forecast$mean)
for (i in 1:(NROW(testing_data)-1)){
  input_data<-c(training_data, testing_data[1:i])
  temp_model<-bats(input_data, model=model_bats)
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
paste ("MSE % For ",validation_data_days,frequency,"by using bats Model for  ==> ", y_lab, sep=" ")
MSE_Mean_All.bats_Model<-round(MSE_Per_Day, 3)
MSE_Mean_All.bats<-paste(round(MSE_Per_Day, 3), " MSE ", validation_data_days, frequency, y_lab, sep=" ")
MSE_bats<-paste(round(MSE_Per_Day, 3))
MSE_bats_Model<-paste(MSE_Per_Day)
paste (" MSE that's Error of Forecasting for ", validation_data_days, " days in bats Model for  ==> ", y_lab, sep=" ")
paste(MSE_Mean_All.bats)
paste ("MSE that's Error of Forecasting day by day for ", validation_data_days, " days in bats Model for  ==> ", y_lab, sep=" ")

BATS_forecast_model<-bats(original_data, model=model_bats)
BATS_forecast <- forecast(BATS_forecast_model, h=N_forecasting_days)
print(ascii(data.frame(FD,forecating_date=forecasting_data_by_name,forecasting_by_NNAR=BATS_forecast$mean)), type = "rest")
plot(c(original_data, BATS_forecast$mean),xlab = paste ("Time in", frequency ,y_lab , sep=" "), ylab=y_lab, type='l')
MSE_Mean_All.bats




# TBATS Model (Trigonometric Seasonal, Box-Cox Transformation, ARMA residuals, Trend and Seasonality)
# Data Modeling
data_series<-ts(training_data)
model_TBATS<-tbats(data_series, use.box.cox=FALSE, seasonal.periods=c(1), use.damped.trend=FALSE)
accuracy(model_TBATS)  # accuracy on training data
# Print Model Parameters
model_TBATS
plot(model_TBATS,xlab = paste ("Time in", frequency, y_lab , sep=" "), ylab=y_lab)

# Testing Data Evaluation
one_day_ahead_forecast <- predict(model_TBATS, h=1)
se_1<-(testing_data[1]-one_day_ahead_forecast$mean)^2
se_vec<-c(se_1)
forecast_vec = c(one_day_ahead_forecast$mean)
for (i in 1:(NROW(testing_data)-1)){
  input_data<-c(training_data, testing_data[1:i])
  temp_model<-tbats(input_data, model=model_TBATS)
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
paste ("MSE % For ",validation_data_days,frequency,"by using TBATS Model for  ==> ", y_lab, sep=" ")
MSE_Mean_All.TBATS_Model<-round(MSE_Per_Day, 3)
MSE_Mean_All.TBATS<-paste(round(MSE_Per_Day, 3), "MSE ", validation_data_days, frequency, y_lab, sep=" ")
MSE_TBATS<-paste(round(MSE_Per_Day,3))
MSE_TBATS_Model<-paste(MSE_Per_Day)
paste (" MSE that's Error of Forecasting for ", validation_data_days, " days in TBATS Model for  ==> ", y_lab, sep=" ")
paste(MSE_Mean_All.TBATS, "%")
paste ("MSE that's Error of Forecasting day by day for ", validation_data_days, " days in TBATS Model for  ==> ", y_lab, sep=" ")

TBATS_forecast_model<-tbats(original_data, model=model_TBATS)
TBATS_forecast <- forecast(TBATS_forecast_model, h=N_forecasting_days)
print(ascii(data.frame(FD, forecating_date=forecasting_data_by_name, forecasting_by_TBATS=TBATS_forecast$mean, Lower=TBATS_forecast$lower, Upper=TBATS_forecast$upper)), type = "rest")
plot(c(original_data, TBATS_forecast$mean),xlab = paste ("Time in", frequency ,y_lab , sep=" "), ylab=y_lab, type='l')
MSE_Mean_All.TBATS



## Holt's linear trend
# Data Modeling
data_series<-ts(training_data)
model_holt<-holt(data_series, h=1, lambda = "auto")
accuracy(model_holt)  # accuracy on training data
# Print Model Parameters
summary(model_holt$model)

# Testing Data Evaluation
one_day_ahead_forecast <- forecast(data_series, h=1, model=model_holt, use.initial.values=TRUE)
se_1<-(testing_data[1]-one_day_ahead_forecast$mean)^2
se_vec<-c(se_1)
forecast_vec = c(one_day_ahead_forecast$mean)
for (i in 1:(NROW(testing_data)-1)){
  input_data<-c(training_data, testing_data[1:i])
  actual_data<-testing_data[i+1]
  one_day_ahead_forecast <- forecast(input_data, h=1, model=model_holt, use.initial.values=TRUE)
  forecast_vec<-c(forecast_vec, one_day_ahead_forecast$mean)
  se<-(actual_data-one_day_ahead_forecast$mean)^2
  se_vec<-c(se_vec, se)
}
plot(c(training_data, forecast_vec),xlab = paste ("Time in", frequency ,y_lab , sep=" "), ylab=y_lab, type='l')
x1_test <- ts(testing_data, start=NROW(training_data)+1 )
lines(x1_test, col='red',lwd=2)
MSE_Per_Day<-round(mean(se_vec), 3)
paste ("MSE % For ",validation_data_days,frequency,"by using holt Model for  ==> ", y_lab, sep=" ")
MSE_Mean_All.Holt_Model<-round(MSE_Per_Day, 3)
MSE_Mean_All.Holt<-paste(round(MSE_Per_Day, 3)," MSE ",validation_data_days,frequency,y_lab,sep=" ")
MSE_holt<-paste(round(MSE_Per_Day, 3))
MSE_holt_Model<-paste(MSE_Per_Day)
paste (" MSE that's Error of Forecasting for ", validation_data_days, " days in holt Model for  ==> ", y_lab, sep=" ")
paste(MSE_Mean_All.Holt,"%")
paste ("MSE that's Error of Forecasting day by day for ", validation_data_days, " days in holt Model for  ==> ", y_lab, sep=" ")

HLT_forecast <- forecast(original_data, h=N_forecasting_days, model=model_holt, use.initial.values=TRUE)
print(ascii(data.frame(FD, forecating_date=forecasting_data_by_name, forecasting_by_holt=HLT_forecast$mean, Lower=HLT_forecast$lower, Upper=HLT_forecast$upper)), type = "rest")
plot(c(original_data, HLT_forecast$mean),xlab = paste ("Time in", frequency ,y_lab , sep=" "), ylab=y_lab, type='l')
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
Box.test(x1_model1$residuals, lag=10, type="Ljung-Box")   # Ljung-Box test on residuals
Box.test(x1_model1$residuals^2, lag=10, type="Ljung-Box")   # Ljung-Box test on residuals-squared

jarque.bera.test(x1_model1$residuals)  # Do Jarque-Bera test on residuals 
#Actual Vs Fitted
plot(data_series, col='red', lwd=2, main="Actual vs Fitted Plot", xlab='Time in (days)', ylab=y_lab) # plot actual and Fitted model 
lines(fitted(x1_model1), col='black')

# Testing Data Evaluation
one_day_ahead_forecast <- forecast(x1_model1, h=1)
se_1<-(testing_data[1]-one_day_ahead_forecast$mean)^2
se_vec<-c(se_1)
forecast_vec = c(one_day_ahead_forecast$mean)
for (i in 1:(NROW(testing_data)-1)){
  input_data<-c(training_data, testing_data[1:i])
  temp_model<-Arima(input_data, model=x1_model1)
  actual_data<-testing_data[i+1]
  one_day_ahead_forecast <- forecast(temp_model, h=1)
  forecast_vec<-c(forecast_vec, one_day_ahead_forecast$mean)
  se<-(actual_data-one_day_ahead_forecast$mean)^2
  se_vec<-c(se_vec, se)
}

plot(c(training_data, forecast_vec),xlab = paste ("Time in", frequency ,y_lab , sep=" "), ylab=y_lab, type='l')
x1_test <- ts(testing_data, start=NROW(training_data)+1 )
lines(x1_test, col='red',lwd=2)
MSE_Per_Day<-round(mean(se_vec), 3)

paste ("MSE % For ",validation_data_days,frequency,"by using bats Model for  ==> ", y_lab, sep=" ")
MSE_Mean_All.ARIMA_Model<-round(MSE_Per_Day, 3)
MSE_Mean_All.ARIMA<-paste(round(MSE_Per_Day, 3)," MSE ", validation_data_days, frequency, y_lab, sep=" ")
MSE_auto_arima<-paste(round(MSE_Per_Day, 3))
MSE_auto.arima_Model<-paste(MSE_Per_Day)
paste (" MSE that's Error of Forecasting for ", validation_data_days, " days in bats Model for  ==> ",y_lab, sep=" ")
paste(MSE_Mean_All.ARIMA, "%")
paste ("MSE that's Error of Forecasting day by day for ",validation_data_days," days in bats Model for  ==> ",y_lab, sep=" ")

ARIMA_forecast_model<-Arima(original_data, model=x1_model1)
ARIMA_forecast <- forecast(ARIMA_forecast_model, h=N_forecasting_days)
print(ascii(data.frame(FD,forecating_date=forecasting_data_by_name,forecasting_by_auto.arima=ARIMA_forecast$mean,Lower=ARIMA_forecast$lower, Upper=ARIMA_forecast$upper)), type = "rest")
plot(c(original_data, ARIMA_forecast$mean),xlab = paste ("Time in", frequency ,y_lab , sep=" "), ylab=y_lab, type='l')
MSE_Mean_All.ARIMA



# GARCH
data_series<-ts(training_data)
auto.arima(data_series, approximation=FALSE, trace=T)
data_series_diff<-diff(data_series)
garch_model<-garchFit(formula= ~arma(3,0) + garch(1, 1), data_series_diff)
summary(garch_model)
res = residuals(garch_model)
res_std = res / garch_model@sigma.t
Box.test(res_std^2, lag=10, type="Ljung-Box")
acf(res_std^2)
# Testing Data Evaluation
one_day_ahead_forecast_diff <- predict(garch_model, 1)
one_day_ahead_forecast<-tail(training_data, n=1) + one_day_ahead_forecast_diff$meanForecast
se_1<-(testing_data[1]-one_day_ahead_forecast)^2
se_vec<-c(se_1)
forecast_vec = c(one_day_ahead_forecast$mean)
for (i in 1:(NROW(testing_data)-1)){
  input_data<-c(training_data, testing_data[1:i])
  actual_data<-testing_data[i+1]
  one_day_ahead_forecast <- predict(garch_model, input_data, h=1)
  forecast_vec<-c(forecast_vec, one_day_ahead_forecast$mean)
  se<-(actual_data-one_day_ahead_forecast$mean)^2
  se_vec<-c(se_vec, se)
}

plot(c(training_data, forecast_vec),xlab = paste ("Time in", frequency ,y_lab , sep=" "), ylab=y_lab, type='l')
x1_test <- ts(testing_data, start=NROW(training_data)+1 )
lines(x1_test, col='red',lwd=2)


# Summary Table for MSE for all models
best_recommended_model <- min(MSE_Mean_All_NNAR, MSE_Mean_All.bats_Model, MSE_Mean_All.TBATS_Model, MSE_Mean_All.Holt_Model, MSE_Mean_All.ARIMA_Model)
paste("Choosing the best model based on MSE of forecasts by using NNAR, BATS, TBATS, Holt's Linear Model, ARIMA", y_lab, sep=" ")
best_recommended_model
x1<-if(best_recommended_model >= MSE_Mean_All.bats_Model) {paste("BATS Model")}
x2<-if(best_recommended_model >= MSE_Mean_All.TBATS_Model) {paste("TBATS Model")}
x3<-if(best_recommended_model >= MSE_Mean_All.Holt_Model) {paste("Holt Model")}
x4<-if(best_recommended_model >= MSE_Mean_All.ARIMA_Model) {paste("ARIMA Model")}
x5<-if(best_recommended_model >= MSE_Mean_All_NNAR) {paste("NNAR Model")}

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
result<-c(x1, x2, x3, x4, x5)
table.error<-data.frame(country.name, NNAR.model=MSE_Mean_All_NNAR, BATS.Model=MSE_Mean_All.bats_Model, TBATS.Model=MSE_Mean_All.TBATS_Model, Holt.Model=MSE_Mean_All.Holt_Model, ARIMA.Model=MSE_Mean_All.ARIMA_Model, Best.Model=result)

print(ascii(table.error), type = "rest")



#
#==========================================================
#

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

validation_forecast<-VARpred(m1, NROW(testing_data))
se_vec<-(testing_data-validation_forecast$pred)^2

MSE_Per_Day<-round(mean(se_vec), 3)
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
