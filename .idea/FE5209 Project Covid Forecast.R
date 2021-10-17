library(forecast)
library(ggplot2)
library("readxl")
install.packages("moments", repos="https://cran.r-project.org")
library(moments)
library(forecast)
require(forecast)  
require(tseries)
require(markovchain)
require(data.table)
library(pander)
install.packages("MTS")
library("MTS")

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

MTSplot(diff_SGdata)
ccm(diff_SGdata)
m0=VARorder(diff_SGdata)
m0$Mstat
names(m0)
m1=VAR(diff_SGdata,13) 
m2=refVAR(m1,thres=1.96)
MTSdiag(m1,adj=12)
VARpred(m1,8)
colMeans(diff_SGdata) 
sqrt(apply(diff_SGdata,2,var))
