



#### Energy consumption

#### facts list ####
# same minute observations = 1441, 1442
# 1 day = 1440 observations
# 1 week = 10080 observations
# 1 30-day-month = 43200 observations
# 2006 = 15 days almost 7 hours
# 2007 = 365 days = 525600 observations (should be)
# 2008 = 366 days = 527040 observations (should be)
# 2009 = 365 days = 525600 observations (should be)
# 2010 = 329.87 days = 475023 observations
# whole dataset = 2075259 obs. = 1441.x days
# canvi d'hora = ...

# calendari francès de festes
# per verificar (https://www.calendrier-365.fr/jours-feries/2007.html)
# 1 gener = cap d'any
# diumenge i dilluns de pasqua (verificar data segons any)
# 1 maig = fête du travail
# 8 maig = victòria de 1945
# 17 maig = ascensió
# diumenge i dilluns de pentecosta (verificar data segons any)
# 14 juliol = festa nacional
# 15 agost = assumpció
# 1 novembre = tots sants
# 11 novembre = armistici de 1918
# Nadal
#
# Dates canvi d'hora
# 2007 (estiu):   a partir de 26/3  
# 2007 (hivern):  a partir de 29/10
# 2008 (estiu):   .
# 2008 (hivern):  .
# 2009 (estiu):   .
# 2009 (hivern):  .
# 2010 (estiu):   .
# 2010 (hivern):  .
#
#
# Calendari hores de sol a París: https://www.kalendrier.com/heure-lever-coucher-soleil/2008/janvier/paris.html
# Dades meteorològiques París-Montsouris: https://www.infoclimat.fr/climatologie-mensuelle/07156/fevrier/2007/paris-montsouris.html


setwd("C:/Users/User/Desktop/DataAnalytics/41DefineDSProcess")

install.packages("forecast")
install.packages("ggpmisc")
install.packages("TTR")
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(forecast)
library(ggpmisc)
library(stats)
library(tseries)






#### 0. Loading and exploring dataset ----

df <- read_delim("household_power_consumption.txt", 
                 ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%d/%m/%Y"), 
                                                              Time = col_time(format = "%H:%M:%S")), 
                 trim_ws = TRUE)


df<- read.table("household_power_consumption.txt", sep = ";", dec = ".", header = TRUE, na.strings = c("NA","?"))
df[1:6,]
head(df)


df <-cbind(df,paste(df$Date,df$Time), stringsAsFactors=FALSE)
colnames(df)[10] <-"DateTime"
df <- df[,c(ncol(df), 1:(ncol(df)-1))]
df$DateTime <- as.POSIXct(df$DateTime)



#### 1. Data preprocessing
### 1.1. feature engineering

## 1.1.2. Converting variables

df$Global_active_power <- (df$Global_active_power*1000)/60
df$Global_reactive_power <- (df$Global_reactive_power*1000)/60

## 1.1.3. New features to create: 
# 1.1.3.1. Total Consumption and Total Submetered Consumption

df$TotalConsumption <- df$Global_active_power+df$Global_reactive_power
df$TotalSubCons <- df$Sub_metering_1+df$Sub_metering_2+df$Sub_metering_3
df$NoSubCons <- df$Global_active_power-df$TotalSubCons


# 1.1.3.2. Year, Month.
df$Year <- substr(df$DateTime,1,4)
df$Year <- as.numeric(df$Year)

df$Month <- substr(df$DateTime,6,7)
df$Month <- as.numeric(df$Month)

df$Season$Winter
df$Season$Spring
df$Season$


energy_ts <- df$Global_active_power  
plot(energy_ts)  
df$gap_clean <- tsclean(energy_ts)
plot(df$gap_clean)

## 1.1.4. Replacing NA's (when Quim's sentence "less is more" has full of sense)

df[is.na(df)] <- 0

## 1.1.5. Subsampling Data

AnnualConsumption <- df %>% group_by(Year) %>% summarise("ActivePower"=sum(Global_active_power),
                                                               "ReactivePower"=sum(Global_reactive_power),
                                                               "Kitchen"=sum(Sub_metering_1),
                                                               "Laundry"=sum(Sub_metering_2),
                                                               "Climatization"=sum(Sub_metering_3),
                                                               "TotalSubCons"=sum(TotalSubCons),
                                                               "TotalConsumption"=sum(TotalConsumption))


MonthlyConsumption <- df %>% group_by(Year, Month) %>% summarise("ActivePower"=sum(Global_active_power),
                                                                "ReactivePower"=sum(Global_reactive_power),
                                                                "Kitchen"=sum(Sub_metering_1),
                                                                "Laundry"=sum(Sub_metering_2),
                                                                "Climatization"=sum(Sub_metering_3),
                                                                "TotalSubCons"=sum(TotalSubCons),
                                                                "TotalConsumption"=sum(TotalConsumption))


# as we realize on 2.2. while decomposing, August'08 is outlying the model,
# so we'll adjust it by replacing its value for the mean all the rest of August
# observations.

MonthlyConsumption[21,3] <- sum(MonthlyConsumption[9,3], MonthlyConsumption[33,3],
     MonthlyConsumption[45,3])/3

MonthlyConsumption[21,4] <- sum(MonthlyConsumption[9,4], MonthlyConsumption[33,4],
                                MonthlyConsumption[45,4])/3

MonthlyConsumption[21,5] <- sum(MonthlyConsumption[9,5], MonthlyConsumption[33,5],
                                MonthlyConsumption[45,5])/3

MonthlyConsumption[21,6] <- sum(MonthlyConsumption[9,6], MonthlyConsumption[33,6],
                                MonthlyConsumption[45,6])/3

MonthlyConsumption[21,7] <- sum(MonthlyConsumption[9,7], MonthlyConsumption[33,7],
                                MonthlyConsumption[45,7])/3

MonthlyConsumption[21,8] <- sum(MonthlyConsumption[9,8], MonthlyConsumption[33,8],
                                MonthlyConsumption[45,8])/3

MonthlyConsumption[21,9] <- sum(MonthlyConsumption[9,9], MonthlyConsumption[33,9],
                                MonthlyConsumption[45,9])/3

# Given the fact that both Dec-06 and Nov-10 the monthly observations are
# incomplete, I'll filter both before training the model.

MonthlyConsumption <- MonthlyConsumption %>% filter(!(Year == 2006))
MonthlyConsumption <- MonthlyConsumption %>% filter(!(Year == 2010 & Month == 11))


## 1.1.6. Correlation Matrix

# create df only with numeric values to do so
numeric_df <- cbind.data.frame(df$Global_active_power,df$Global_reactive_power,
                               df$Voltage,df$Global_intensity,df$Sub_metering_1,
                               df$Sub_metering_2,df$Sub_metering_3,df$TotalConsumption,
                               df$TotalSubCons,df$NoSubCons)

#run correlation matrix
cor(numeric_df, use = "pairwise.complete.obs")     #use this argument to deal with NA

# Correlations with Total Consumption
# 1. Global Intensity           0.99
# 2. Submetered Consumption     0.84
# 3. No Submetered Consumption  0.70
# 4. Submeter 3                 0.63
# Voltage     *inverse correlation with the rest of variables
#             *the more correlated with TotalConsumption, the more inversely
#              correlated with Voltage

# with GAP
# 1. Global Intensity           0.99
# 2. Submetered Consumption     0.85
# 3. No Submetered Consumption  0.71
# 4. Submeter 3                 0.64

# with GRP
# 1. Total Consumption          0.35
# 2. Global Intensity           0.28
# 3. GAP                        0.26
# 4. No Submetered Consumption  0.22

## Data Visualization

df %>% select(Date,DateTime,TotalConsumption) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=TotalConsumption,col="Consumption"))

df %>% group_by(year(DateTime)) %>% select(Date,DateTime,TotalConsumption) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=TotalConsumption,col="Consumption"))


#### 2. Building Models

## 2.1. Converting to Time Series

ts_annual <- ts(AnnualConsumption, frequency = 1)
ts_monthly <- ts(MonthlyConsumption, frequency = 12)

# outliers

boxplot(ts_annual ~ cycle(ts_annual))
boxplot(ts_monthly ~ cycle(ts_monthly))
boxplot(ts_monthly ~ cycle(ts_monthly))


# Splitting into train and test sets

ts_monthly_train <- forecast:::subset.ts(ts_monthly, end = 34)
ts_monthly_test <- forecast:::subset.ts(ts_monthly, start = 35)


# log (useless, we are dealing an additive model, not multiplicative)
log.ts_annual <- log(ts_annual)
log.ts_monthly <- log(ts_monthly)

plot.ts(log.ts_annual)
plot.ts(log.ts_monthly)

### 2.2. Decomposing Time Series

## 2.2.1. Decompose monthly GAP
GAP.Month.TS <- (ts_monthly[,3])
GAP.Month.TS.Decomposed <-decompose(GAP.Month.TS)
plot(GAP.Month.TS.Decomposed)
autoplot(GAP.Month.TS.Decomposed)

GAP.Month.TS.Decomposed$seasonal
GAP.Month.TS.Decomposed$trend
GAP.Month.TS.Decomposed$random

boxplot(GAP.Month.TS ~ cycle(GAP.Month.TS))

#2.2.1.1. Decompose monthly GAP to train and test

GAP.Month.TS.Train <- (ts_monthly_train[,3])
GAP.Month.TS.Train.Decomposed <-decompose(GAP.Month.TS.Train)
plot(GAP.Month.TS.Train.Decomposed)
autoplot(GAP.Month.TS.Train.Decomposed)

GAP.Month.TS.Test <- (ts_monthly_test[,3])
decompose(GAP.Month.TS.Test)


## 2.2.2. Decompose monthly GRP
GRP.Month.TS <- (ts_monthly[,4])
GRP.Month.TS.Decomposed <- decompose(GRP.Month.TS)
plot(GRP.Month.TS.Decomposed)
autoplot(GRP.Month.TS.Decomposed)

boxplot(GRP.Month.TS ~ cycle(GRP.Month.TS))

#2.2.2.1. Decompose monthly GRP to train and test
GRP.Month.TS.Train <- ts_monthly_train[,4]
GRP.Month.TS.Train.Decomposed <- decompose(GRP.Month.TS.Train)
plot(GRP.Month.TS.Train.Decomposed)

GRP.Month.TS.Test <- ts_monthly_test[,4]

## 2.2.3. Decompose Total Consumption
TC.Month.TS <- (ts_monthly[,9])
TC.Month.TS.Decomposed <- decompose(TC.Month.TS)
plot(TC.Month.TS.Decomposed)

boxplot(TC.Month.TS ~ cycle(TC.Month.TS))

#2.2.3.1. Decompose TotC to train and test
TC.Month.TS.Train <- ts_monthly_train[,9]
TC.Month.TS.Train.Decomposed <- decompose(TC.Month.TS.Train)
plot(TC.Month.TS.Train.Decomposed)

TC.Month.TS.Test <- ts_monthly_test[,9]

## 2.2.4. Outliers

tsclean(GAP.Month.TS)   ## NO ENCARA!!!

### 2.3. Adjusting

## 2.3.1. Adjusting monthly GAP
GAP.Month.TS.Adjusted <- GAP.Month.TS - GAP.Month.TS.Decomposed$random
plot(GAP.Month.TS.Adjusted)       


## 2.3.2. Adjusting monthly GRP
GRP.Month.TS.Adjusted <- GRP.Month.TS - GRP.Month.TS.Decomposed$random
plot(GRP.Month.TS.Adjusted)

## 2.3.3. Adjusting monthly Total Consumption
TC.Month.TS.Adjusted <- TC.Month.TS - TC.Month.TS.Decomposed$random
plot(TC.Month.TS.Adjusted)

### 2.4. Training forecasts
## 2.4.1. HoltWinters
## 2.4.1.1 HoltWinters with monthly GAP
GAP.Month.TS.Forecast.Train <- HoltWinters(GAP.Month.TS.Train, alpha = NULL)
stats:::plot.HoltWinters(GAP.Month.TS.Forecast.Train, col = 1,
                         col.predicted = "green")

GAP.Month.TS.Forecast.Train$fitted

GAP.Month.TS.HWForecast.Train <- forecast:::forecast.HoltWinters(GAP.Month.TS.Forecast.Train, h = 12)
GAP.Month.TS.HWForecast.Train
forecast:::plot.forecast(GAP.Month.TS.HWForecast.Train)

accuracy(GAP.Month.TS.HWForecast.Train,GAP.Month.TS.Test)

GAP.Month.TS.Test

## 2.4.1.2 HoltWinters with monthly GRP
GRP.Month.TS.Forecast.Train <- HoltWinters(GRP.Month.TS.Train, alpha = NULL)
plot(GRP.Month.TS.Forecast.Train)

GRP.Month.TS.HWForecast.Train <- forecast:::forecast.HoltWinters(GRP.Month.TS.Forecast.Train, h = 12)
GRP.Month.TS.HWForecast.Train
forecast:::plot.forecast(GRP.Month.TS.HWForecast.Train)

accuracy(GRP.Month.TS.HWForecast.Train, GRP.Month.TS.Test)

## 2.4.1.3. HoltWinters with Total Consumption
TC.Month.TS.Forecast.Train <- HoltWinters(TC.Month.TS.Train, alpha = NULL)
plot(TC.Month.TS.Forecast.Train)

TC.Month.TS.HWForecast.Train <- forecast:::forecast.HoltWinters(TC.Month.TS.Forecast.Train, h = 12)
TC.Month.TS.HWForecast.Train
forecast:::plot.forecast(TC.Month.TS.HWForecast.Train)

accuracy(TC.Month.TS.HWForecast.Train, TC.Month.TS.Test)

### 2.4.2.1. ARIMA
## 2.4.2.2. Training monthly GAP with ARIMA

GAP.Month.TS.Train.Arima <- auto.arima(GAP.Month.TS.Train)
GAP.Month.TS.Train.ArimaForecast <- forecast:::forecast.Arima(GAP.Month.TS.Train.Arima, h = 12)
plot(GAP.Month.TS.Train.ArimaForecast)

accuracy(GAP.Month.TS.Train.ArimaForecast, GAP.Month.TS.Test)

## 2.4.2.3. Training monthly GRP with ARIMA

GRP.Month.TS.Train.Arima <- auto.arima(GRP.Month.TS.Train)
GRP.Month.TS.Train.ArimaForecast <- forecast:::forecast.Arima(GRP.Month.TS.Train.Arima, h = 12)
plot(GRP.Month.TS.Train.ArimaForecast)

accuracy(GRP.Month.TS.Train.ArimaForecast, GRP.Month.TS.Test)

## 2.4.2.4. Training monthly TC with ARIMA

TC.Month.TS.Train.Arima <- auto.arima(TC.Month.TS.Train)
TC.Month.TS.Train.ArimaForecast <- forecast:::forecast.Arima(TC.Month.TS.Train.Arima, h = 12)
plot(TC.Month.TS.Train.ArimaForecast)

accuracy(TC.Month.TS.Train.ArimaForecast, TC.Month.TS.Test)

### 2.4.3. Plotting trained models

## 2.4.3.1. GAP
autoplot(GAP.Month.TS.Test) +
  autolayer(GAP.Month.TS.HWForecast.Train,col="red", series = "HoltWinters", PI = FALSE) +
  autolayer(GAP.Month.TS.Train.ArimaForecast,col="blue", series = "ARIMA", PI = FALSE) +
  scale_color_manual(labels("HoltWinters","ARIMA"))

## 2.4.3.2. GRP
autoplot(GRP.Month.TS.Test) + 
  autolayer(GRP.Month.TS.HWForecast.Train,col="red", PI = FALSE) +
  autolayer(GRP.Month.TS.Train.ArimaForecast,col="blue", PI = FALSE)

## 2.4.3.3. Total Consumption
autoplot(TC.Month.TS.Test) +
  autolayer(TC.Month.TS.HWForecast.Train, col="red", PI = FALSE) +
  autolayer(TC.Month.TS.Train.ArimaForecast, col="blue", PI = FALSE)

#### 2.5. Applying Model

### 2.5.1. HoltWinters
## 2.5.1.1. HoltWinters to GAP
GAP.Month.TS.HoltWinters <- HoltWinters(GAP.Month.TS, alpha = NULL)
stats:::plot.HoltWinters(GAP.Month.TS.HoltWinters, col = 1, 
                         col.predicted = "green")

GAP.Month.TS.Forecast.Train <- HoltWinters(GAP.Month.TS.Train, alpha = NULL)
stats:::plot.HoltWinters(GAP.Month.TS.Forecast.Train, col = 1,
                         col.predicted = "green")

GAP.Month.TS.Forecast.Train$fitted

GAP.Month.TS.HWForecast.Train <- forecast:::forecast.HoltWinters(GAP.Month.TS.Forecast.Train, h = 12)
GAP.Month.TS.HWForecast.Train
forecast:::plot.forecast(GAP.Month.TS.HWForecast.Train)

accuracy(GAP.Month.TS.HWForecast.Train,GAP.Month.TS.Test)

## 2.5.1.2. HoltWinters to GRP

## 2.5.1.3. HoltWinters to Total Consumption

### 2.5.2. Arima

## 2.5.2.1. Arima to GAP

## 2.5.2.2. Arima to GRP

## 2.5.2.3. Arima to Total Consumption

