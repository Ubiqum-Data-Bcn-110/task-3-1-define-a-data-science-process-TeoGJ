



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
install.packages("bizdays")
install.packages("marima")
install.packages("forecastHybrid")
install.packages("vars")
install.packages("opera")
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(forecast)
library(ggpmisc)
library(stats)
library(tseries)
library(bizdays)
library(marima)
library(forecastHybrid)
library(vars)
library(opera)
library(rvest)





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

# adding weather data
meteo <- readxl::read_xlsx("meteodata.xlsx")
meteo <- as.data.frame(meteo)

meteo$Ensoleillement <- meteo$Ensoleillement*1440

#### 1. Data preprocessing
### 1.1. feature engineering

## 1.1.2. Converting variables

df$Global_active_power <- (df$Global_active_power*1000)/60
df$Global_reactive_power <- (df$Global_reactive_power*1000)/60

DailyConsumption$ActivePower <- (DailyConsumption$ActivePower*60)/1000
DailyConsumption$ReactivePower <- (DailyConsumption$ReactivePower*60)/1000

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

df$Day <- substr(df$DateTime,9,10)
df$Day <- as.numeric(df$Day)

energy_ts <- df$Global_active_power  
plot(energy_ts)  
df$gap_clean <- tsclean(energy_ts)
plot(df$gap_clean)

## 1.1.4. Replacing NA's (when Quim's sentence "less is more" has full of sense)

df[is.na(df)] <- 0
meteo[is.na(meteo)] <- 0


df %>% 

## 1.1.5. Subsampling Data

AnnualConsumption <- df %>% group_by(Year) %>% summarise("ActivePower"=sum(Global_active_power),
                                                               "ReactivePower"=sum(Global_reactive_power),
                                                               "Kitchen"=sum(Sub_metering_1),
                                                               "Laundry"=sum(Sub_metering_2),
                                                               "Climatization"=sum(Sub_metering_3),
                                                               "TotalSubCons"=sum(TotalSubCons),
                                                               "TotalConsumption"=sum(TotalConsumption))


DailyConsumption <- df %>% group_by(Date) %>% summarise("ActivePower"=sum(Global_active_power),
                                                        "ReactivePower"=sum(Global_active_power),
                                                        "Kitchen"=sum(Sub_metering_1),
                                                        "Laundry"=sum(Sub_metering_2),
                                                        "Climatization"=sum(Sub_metering_3),
                                                        "NoSubmetered"=sum(TotalSubCons),
                                                        "TotalConsumption"=sum(TotalConsumption),
                                                        "Voltage"=mean(Voltage),
                                                        "Intensity"=sum(Global_intensity))

meteo2 <- meteo %>% group_by(Date1) %>% summarise("TMin"=`Temp. Minime`,
                                                 "TMax" =`Temp. Maximale`,
                                                 "Sun"=Ensoleillement,
                                                 "Rain"=Précipitations)

meteo$Date1 <- ymd(meteo$Date1)
str(meteo)
meteo$Year <- substr(meteo$Date1,1,4)
meteo$Year <- as.numeric(meteo$Year)
  
meteo$Month <- substr(meteo$Date1,6,7)
meteo$Month <- as.numeric(meteo$Month)

meteo_month <- meteo%>% group_by(Year,Month) %>% summarise("TMin"=mean(`Temp. Minime`),
                                                      "TMax"=mean(`Temp. Maximale`),
                                                      "Sun"=sum(Ensoleillement),
                                                      "Rain"=sum(Précipitations))


DailyConsumption <- cbind(DailyConsumption, meteo)
DailyConsumption[is.na(DailyConsumption)] <- 0
DailyConsumption$Date1 = NULL
DailyConsumption$TMin = as.numeric(DailyConsumption$TMin)
DailyConsumption$TMax = as.numeric(DailyConsumption$TMax)
DailyConsumption$Sun = as.numeric(DailyConsumption$Sun)

DailyNumeric <- DailyConsumption
DailyNumeric$Date = NULL
                                            


MonthlyConsumption <- df %>% group_by(Year, Month) %>% summarise("ActivePower"=sum(Global_active_power),
                                                                "ReactivePower"=sum(Global_reactive_power),
                                                                "Kitchen"=sum(Sub_metering_1),
                                                                "Laundry"=sum(Sub_metering_2),
                                                                "Climatization"=sum(Sub_metering_3),
                                                                "TotalSubCons"=sum(TotalSubCons),
                                                                "TotalConsumption"=sum(TotalConsumption))

MonthlyConsumption <- cbind(MonthlyConsumption,meteo_month)

MonthlyNumeric <- cbind(MonthlyConsumption,meteo_month)
str(MonthlyNumeric)

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


MonthlyConsumption %>% dplyr::select(Year,Month,ActivePower,ReactivePower,TMin,TMax) %>%
  ggplot(aes(x=Month)) +
  geom_line(aes(y=ActivePower,col="ActivePower")) +
  geom_line(aes(y=ReactivePower,col="ReactivePower")) +
  geom_line(aes(y=TMax,col="TMax")) +
  geom_line(aes(y=TMin,col="TMin"))


DailyConsumption %>% dplyr::select(Date,ActivePower,ReactivePower,Kitchen,Laundry,
                                   Climatization,TotalConsumption,TMin,TMax,Sun,
                                   Rain) %>%
  ggplot(aes(x=Date)) +
  geom_line(aes(y=TMax,col="TMax"))

DailyConsumption %>% dplyr::select(Date,ActivePower,ReactivePower,Kitchen,Laundry,
                                   Climatization,TotalConsumption,TMin,TMax,Sun,
                                   Rain) %>%
  ggplot(aes(x=Date)) +
  geom_line(aes(y=TMax,col="TMax")) +
  geom_line(aes(y=TMin,col="TMin"))

  
DailyConsumption %>% dplyr::select(Date,ActivePower,ReactivePower,Kitchen,Laundry,
                                     Climatization,TotalConsumption,TMin,TMax,Sun,
                                     Rain) %>%
  ggplot(aes(x=Date)) +

DailyConsumption$TMax <- DailyConsumption$`Temp. Maximale`
DailyConsumption$TMin <- DailyConsumption$`Temp. Minime`
DailyConsumption$Rain <- DailyConsumption$Précipitations
DailyConsumption$Sun <- DailyConsumption$Ensoleillement

plot(df$Global_intensity, df$Voltage)

# Given the fact that both Dec-06 and Nov-10 the monthly observations are
# incomplete, I'll filter both before training the model.

MonthlyConsumption2 <- MonthlyConsumption %>% filter(!(Year == 2006))
MonthlyConsumption2 <- MonthlyConsumption2 %>% filter(!(Year == 2010 & Month == 11))

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

cor(DailyNumeric, use = "pairwise.complete.obs")     #use this argument to deal with NA
cor(MonthlyNumeric, use = "pairwise.complete.obs")

## Data Visualization

df %>% select(Date,DateTime,TotalConsumption) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=TotalConsumption,col="Consumption"))

df %>% group_by(year(DateTime)) %>% select(Date,DateTime,TotalConsumption) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=TotalConsumption,col="Consumption"))


#### 2. Building Models

## 2.1. Converting to Time Series

ts_annual <- ts(AnnualConsumption, frequency = 1)
ts_monthly <- ts(MonthlyConsumption, frequency = 12)
ts_monthly2 <- ts(MonthlyConsumption2, frequency = 12)

plot.ts(ts_monthly)

# outliers

boxplot(ts_annual ~ cycle(ts_annual))
boxplot(ts_monthly ~ cycle(ts_monthly))
boxplot(ts_monthly ~ cycle(ts_monthly))

boxplot(GAP.Month.TS ~ cycle(GAP.Month.TS))
boxplot(GRP.Month.TS ~ cycle(GRP.Month.TS))
boxplot(TC.Month.TS ~ cycle(TC.Month.TS))

# Splitting into train and test sets

ts_monthly_train <- forecast:::subset.ts(ts_monthly, end = 34)
ts_monthly_test <- forecast:::subset.ts(ts_monthly, start = 35)

ts_monthly2_train <- forecast:::subset.ts(ts_monthly2, end = 34)
ts_monthly2_test <- forecast:::subset.ts(ts_monthly2, start = 35)


# log (useless, we are dealing an additive model, not multiplicative)
log.ts_annual <- log(ts_annual)
log.ts_monthly <- log(ts_monthly)

plot.ts(log.ts_annual)
plot.ts(log.ts_monthly)

acf(diff(log.ts_monthly))
pacf(diff(log.ts_monthly))

### 2.2. Decomposing Time Series
ts_monthly.decomposed <- decompose(ts_monthly)
ts_monthly.train.decomposed <- decompose(ts_monthly_train)

ts_monthly.train.decomposed

## 2.2.1. Decompose monthly GAP
GAP.Month.TS <- (ts_monthly[,3])
GAP.Month.TS.Decomposed <-decompose(GAP.Month.TS)
plot(GAP.Month.TS.Decomposed)
autoplot(GAP.Month.TS.Decomposed)

GAP.Month.TS.2 <- (ts_monthly2[,3])


stl_gap7 <- stl(GAP.Month.TS, s.window = 7)
plot(stl_gap7)

stl_gap9 <- stl(GAP.Month.TS, s.window = 9)
plot(stl_gap9)

stl_gap11 <- stl(GAP.Month.TS, s.window = 11)
plot(stl_gap11)

stl_grp7 <- stl(GRP.Month.TS, s.window = 7)
plot(stl_grp7)

stl_grp9 <- stl(GRP.Month.TS, s.window = 9)
plot(stl_grp9)

stl_grp11 <- stl(GRP.Month.TS, s.window = 11)
plot(stl_grp11)

#2.2.1.1. Decompose monthly GAP to train and test
GAP.Month.TS.Train <- (ts_monthly_train[,3])
GAP.Month.TS.Train.Decomposed <-decompose(GAP.Month.TS.Train)
plot(GAP.Month.TS.Train.Decomposed)
autoplot(GAP.Month.TS.Train.Decomposed)

GAP.Month.TS.Test <- (ts_monthly_test[,3])
decompose(GAP.Month.TS.Test)

GAP.Month.TS.Train.2 <- (ts_monthly2_train[,3])
GAP.Month.TS.Test.2 <- (ts_monthly2_test[,3])

## 2.2.2. Decompose monthly GRP
GRP.Month.TS <- (ts_monthly[,4])
GRP.Month.TS.Decomposed <- decompose(GRP.Month.TS)
plot(GRP.Month.TS.Decomposed)
autoplot(GRP.Month.TS.Decomposed)

boxplot(GRP.Month.TS ~ cycle(GRP.Month.TS))


GRP.Month.TS.2 <- ts_monthly2[,4]

#2.2.2.1. Decompose monthly GRP to train and test
GRP.Month.TS.Train <- ts_monthly_train[,4]
GRP.Month.TS.Train.Decomposed <- decompose(GRP.Month.TS.Train)
plot(GRP.Month.TS.Train.Decomposed)

GRP.Month.TS.Test <- ts_monthly_test[,4]

GRP.Month.TS.Train.2 <- ts_monthly2_train[,4]
GRP.Month.TS.Test.2 <- ts_monthly2_test[,4]

## 2.2.3. Decompose Total Consumption
TC.Month.TS <- (ts_monthly[,9])
TC.Month.TS.Decomposed <- decompose(TC.Month.TS)
plot(TC.Month.TS.Decomposed)

boxplot(TC.Month.TS ~ cycle(TC.Month.TS))

TC.Month.TS.2 <- (ts_monthly2[,9])

#2.2.3.1. Decompose TotC to train and test
TC.Month.TS.Train <- ts_monthly_train[,9]
TC.Month.TS.Train.Decomposed <- decompose(TC.Month.TS.Train)
plot(TC.Month.TS.Train.Decomposed)

TC.Month.TS.Test <- ts_monthly_test[,9]

TC.Month.TS.Train.2 <- ts_monthly2_train[,9]
TC.Month.TS.Test.2 <- ts_monthly2_test[,9]

## 2.2.4. Outliers

tsclean(GAP.Month.TS)   ## NO ENCARA!!!

### 2.3. Adjusting

## 2.3.1. Adjusting monthly GAP
GAP.Month.TS.Adjusted <- GAP.Month.TS - GAP.Month.TS.Decomposed$random
plot(GAP.Month.TS.Adjusted)

GAP.Month.TS.Train.Adjusted <- GAP.Month.TS.Train - GAP.Month.TS.Train.Decomposed$random
plot(GAP.Month.TS.Train.Adjusted)
plot(GAP.Month.TS.Train)

## 2.3.2. Adjusting monthly GRP
GRP.Month.TS.Adjusted <- GRP.Month.TS - GRP.Month.TS.Decomposed$random
plot(GRP.Month.TS.Adjusted)

GRP.Month.TS.Train.Adjusted <- GRP.Month.TS.Train - GRP.Month.TS.Train.Decomposed$random

## 2.3.3. Adjusting monthly Total Consumption
TC.Month.TS.Adjusted <- TC.Month.TS - TC.Month.TS.Decomposed$random
plot(TC.Month.TS.Adjusted)

TC.Month.TS.Train.Adjusted <- TC.Month.TS.Train - TC.Month.TS.Train.Decomposed$random

#### 2.4. Training forecasts
### 2.4.1. HoltWinters
## 2.4.1.1 HoltWinters with monthly GAP
# 2.4.1.1.1. Removing randomness

GAP.Month.TS.HW.Train <- HoltWinters(GAP.Month.TS.Train.Adjusted)
stats:::plot.HoltWinters(GAP.Month.TS.HW.Train, col = 1,
                         col.predicted = "green")

GAP.Month.TS.HWForecast.Train <- forecast:::forecast.HoltWinters(GAP.Month.TS.HW.Train, h = 12)
GAP.Month.TS.HWForecast.Train
forecast:::plot.forecast(GAP.Month.TS.HWForecast.Train)

accuracy(GAP.Month.TS.HWForecast.Train,GAP.Month.TS.Test)

# 2.4.1.1.2 Without removing randomness

GAP.Month.TS.HW.Train.R <- HoltWinters(GAP.Month.TS.Train)
stats:::plot.HoltWinters(GAP.Month.TS.HW.Train.2, col = 1,
                         col.predicted = "green")

GAP.Month.TS.HWForecast.Train.R <- forecast:::forecast.HoltWinters(GAP.Month.TS.HW.Train.R, h = 12)
GAP.Month.TS.HWForecast.Train.R
forecast:::plot.forecast(GAP.Month.TS.HWForecast.Train.2)

accuracy(GAP.Month.TS.HWForecast.Train.2,GAP.Month.TS.Test)

#2.4.1.1.3 Only using complete months

GAP.Month.TS.HW.Train.2 <- HoltWinters(GAP.Month.TS.Train.2)
stats:::plot.HoltWinters(GAP.Month.TS.HW.Train.2, col = 1,
                         col.predicted = "green")

GAP.Month.TS.HWForecast.Train.2 <- forecast:::forecast.HoltWinters(GAP.Month.TS.HW.Train.2, h = 12)
GAP.Month.TS.HWForecast.Train.2
forecast:::plot.forecast(GAP.Month.TS.HWForecast.Train.2)

accuracy(GAP.Month.TS.HWForecast.Train.2,GAP.Month.TS.Test.2)

## 2.4.1.2 HoltWinters with monthly GRP
# 2.4.1.2.1. Removing randomness
GRP.Month.TS.HW.Train <- HoltWinters(GRP.Month.TS.Train, alpha = NULL)
plot(GRP.Month.TS.Forecast.Train)

GRP.Month.TS.HWForecast.Train <- forecast:::forecast.HoltWinters(GRP.Month.TS.HW.Train, h = 12)
GRP.Month.TS.HWForecast.Train
forecast:::plot.forecast(GRP.Month.TS.HWForecast.Train)

accuracy(GRP.Month.TS.HWForecast.Train, GRP.Month.TS.Test)

# 2.4.1.2.2 Without removing randomness
GRP.Month.TS.HW.Train.R <- HoltWinters(GRP.Month.TS.Train)
plot(GRP.Month.TS.HW.Train.R)

GRP.Month.TS.HWForecast.Train.R <- forecast:::forecast.HoltWinters(GRP.Month.TS.HW.Train.R, h = 12)
GRP.Month.TS.HWForecast.Train.R
forecast:::plot.forecast(GRP.Month.TS.HWForecast.Train.R)

accuracy(GRP.Month.TS.HWForecast.Train.R , GRP.Month.TS.Test)

#2.4.1.2.3 Only with complete months
GRP.Month.TS.HW.Train.2 <- HoltWinters(GRP.Month.TS.Train.2)
plot(GRP.Month.TS.HW.Train.2)

GRP.Month.TS.HWForecast.Train.2 <- forecast:::forecast.HoltWinters(GRP.Month.TS.HW.Train.2, h = 12)
GRP.Month.TS.HWForecast.Train.2
forecast:::plot.forecast(GRP.Month.TS.HWForecast.Train.2)

accuracy(GRP.Month.TS.HWForecast.Train.2, GRP.Month.TS.Test.2)

## 2.4.1.3. HoltWinters with Total Consumption
# 2.4.1.3.1. Removing randomness
TC.Month.TS.HW.Train <- HoltWinters(TC.Month.TS.Train, alpha = NULL)
plot(TC.Month.TS.HW.Train)

TC.Month.TS.HWForecast.Train <- forecast:::forecast.HoltWinters(TC.Month.TS.HW.Train, h = 12)
TC.Month.TS.HWForecast.Train
forecast:::plot.forecast(TC.Month.TS.HWForecast.Train)

accuracy(TC.Month.TS.HWForecast.Train, TC.Month.TS.Test)

# 2.4.1.3.2. Without removing randomness
TC.Month.TS.HW.Train.R <- HoltWinters(TC.Month.TS.Train)
plot(TC.Month.TS.HW.Train.R)

TC.Month.TS.HWForecast.Train.R <- forecast:::forecast.HoltWinters(TC.Month.TS.HW.Train.R, h = 12)
TC.Month.TS.HWForecast.Train.R
forecast:::plot.forecast(TC.Month.TS.HWForecast.Train.R)

accuracy(TC.Month.TS.HWForecast.Train.R, TC.Month.TS.Test)

# 2.4.1.3.3. Only with complete months
TC.Month.TS.HW.Train.2 <- HoltWinters(TC.Month.TS.Train.2)
plot(TC.Month.TS.HW.Train.2)

TC.Month.TS.HWForecast.Train.2 <- forecast:::forecast.HoltWinters(TC.Month.TS.HW.Train.2, h = 12)
TC.Month.TS.HWForecast.Train.2
forecast:::plot.forecast(TC.Month.TS.HWForecast.Train.2)

accuracy(TC.Month.TS.HWForecast.Train.2, TC.Month.TS.Test.2)


### 2.4.2.1. ARIMA
## 2.4.2.2. Training monthly GAP with ARIMA
# 2.4.2.2.1.

GAP.Month.TS.Train.Arima <- auto.arima(GAP.Month.TS.Train)
GAP.Month.TS.Train.ArimaForecast <- forecast:::forecast.Arima(GAP.Month.TS.Train.Arima, h = 12)
autoplot(GAP.Month.TS.Train.ArimaForecast)
GAP.Month.TS.Train.Arima

accuracy(GAP.Month.TS.Train.ArimaForecast, GAP.Month.TS.Test)

#2.4.2.2.2 Only with complete months
GAP.Month.TS.Train.Arima.2 <- auto.arima(GAP.Month.TS.Train.2)
GAP.Month.TS.Train.ArimaForecast.2 <- forecast:::forecast.Arima(GAP.Month.TS.Train.Arima.2, h = 12)
autoplot(GAP.Month.TS.Train.ArimaForecast.2)

accuracy(GAP.Month.TS.Train.ArimaForecast.2, GAP.Month.TS.Test.2)


## 2.4.2.3. Training monthly GRP with ARIMA
# 2.4.2.3.1

GRP.Month.TS.Train.Arima <- auto.arima(GRP.Month.TS.Train)
GRP.Month.TS.Train.ArimaForecast <- forecast:::forecast.Arima(GRP.Month.TS.Train.Arima, h = 12)
plot(GRP.Month.TS.Train.ArimaForecast)

accuracy(GRP.Month.TS.Train.ArimaForecast, GRP.Month.TS.Test)

#2.4.2.3.2 Only with complete months
GRP.Month.TS.Train.Arima.2 <- auto.arima(GRP.Month.TS.Train.2)
GRP.Month.TS.Train.ArimaForecast.2 <- forecast:::forecast.Arima(GRP.Month.TS.Train.Arima.2, h = 12)
autoplot(GRP.Month.TS.Train.ArimaForecast.2)

accuracy(GRP.Month.TS.Train.ArimaForecast.2, GRP.Month.TS.Test.2)


## 2.4.2.4. Training monthly TC with ARIMA
# 2.4.2.4.1

TC.Month.TS.Train.Arima <- auto.arima(TC.Month.TS.Train)
TC.Month.TS.Train.ArimaForecast <- forecast:::forecast.Arima(TC.Month.TS.Train.Arima, h = 12)
plot(TC.Month.TS.Train.ArimaForecast)

accuracy(TC.Month.TS.Train.ArimaForecast, TC.Month.TS.Test)

# 2.4.2.4.2 Only with complete months
TC.Month.TS.Train.Arima.2 <- auto.arima(TC.Month.TS.Train.2)
TC.Month.TS.Train.ArimaForecast.2 <- forecast:::forecast.Arima(TC.Month.TS.Train.Arima.2, h = 12)
autoplot(TC.Month.TS.Train.ArimaForecast.2)

accuracy(TC.Month.TS.Train.ArimaForecast.2, TC.Month.TS.Test.2)

## 2.4.3. Hybrid modelling
# 2.4.3.1. Training hybrid modelling to GAP

GAP.Month.TS.Hybrid.Train <- hybridModel(GAP.Month.TS.Train, models = "aefnst", lambda = NULL, a.args = NULL,
            e.args = NULL, n.args = NULL, s.args = NULL, t.args = NULL,
            z.args = NULL, weights = c("equal", "insample.errors", "cv.errors"),
            errorMethod = c("RMSE", "MAE", "MASE"), cvHorizon = frequency(y),
            windowSize = 84, horizonAverage = FALSE, parallel = FALSE,
            num.cores = 2L, verbose = TRUE)

GAP.Month.TS.HybridForecast.Train <- forecastHybrid:::forecast.hybridModel(GAP.Month.TS.Hybrid.Train, h = 12)
forecastHybrid:::plot.hybridModel(GAP.Month.TS.HybridForecast.Train)
forecast:::plot.forecast(GAP.Month.TS.HybridForecast.Train)

accuracy(GAP.Month.TS.HybridForecast.Train, GAP.Month.TS.Test.2)
accuracy(GAP.Month.TS.HWForecast.Train, GAP.Month.TS.Test.2)
accuracy(GAP.Month.TS.Train.ArimaForecast, GAP.Month.TS.Test.2)

# 2.4.3.2. Training hybrid modelling to GRP

GRP.Month.TS.Hybrid.Train <- hybridModel(GRP.Month.TS.Train, models = "aefnst", lambda = NULL, a.args = NULL,
                                         e.args = NULL, n.args = NULL, s.args = NULL, t.args = NULL,
                                         z.args = NULL, weights = c("equal", "insample.errors", "cv.errors"),
                                         errorMethod = c("RMSE", "MAE", "MASE"), cvHorizon = frequency(y),
                                         windowSize = 84, horizonAverage = FALSE, parallel = FALSE,
                                         num.cores = 2L, verbose = TRUE)

GRP.Month.TS.HybridForecast.Train <- forecastHybrid:::forecast.hybridModel(GRP.Month.TS.Hybrid.Train, h = 12)
forecastHybrid:::plot.hybridModel(GRP.Month.TS.HybridForecast.Train)
forecast:::plot.forecast(GRP.Month.TS.HybridForecast.Train)

accuracy(GRP.Month.TS.HybridForecast.Train, GRP.Month.TS.Test.2)
accuracy(GRP.Month.TS.HWForecast.Train, GRP.Month.TS.Test.2)
accuracy(GRP.Month.TS.Train.ArimaForecast, GRP.Month.TS.Test.2)

### 2.5. Plotting trained models

## 2.5.1. GAP
autoplot(GAP.Month.TS.Test) +
  autolayer(GAP.Month.TS.HWForecast.Train,col="red", series = "HoltWinters", PI = FALSE) +
  autolayer(GAP.Month.TS.Train.ArimaForecast,col="blue", series = "ARIMA", PI = FALSE) +
  autolayer(GAP.Month.TS.HybridForecast.Train, col = "green", series = "Hybrid", PI = FALSE) +
    scale_color_manual(labels("HoltWinters","ARIMA"))

autoplot(GAP.Month.TS.Test) +
  autolayer(GAP.Month.TS.Train.ArimaForecast,col="blue", series = "ARIMA", PI = FALSE)

    autolayer(GAP.Month.TS.HybridForecast, col = "green", series = "Hybrid", PI = FALSE) +
  scale_color_manual(labels("HoltWinters","ARIMA"))


## 2.5.2. GRP
autoplot(GRP.Month.TS.Test) + 
  autolayer(GRP.Month.TS.HWForecast.Train,col="red", PI = FALSE) +
  autolayer(GRP.Month.TS.Train.ArimaForecast,col="blue", PI = FALSE) +
  autolayer(GRP.Month.TS.HybridForecast.Train, col ="green", series = "Hybrid", PI = FALSE)

## 2.5.3. Total Consumption
autoplot(TC.Month.TS.Test) +
  autolayer(TC.Month.TS.HWForecast.Train, col="red", PI = FALSE) +
  autolayer(TC.Month.TS.Train.ArimaForecast, col="blue", PI = FALSE)


## 2.5.4. Checking trained models performance
# 2.5.4.1. GAP
GAPHybridForecastTrain <- as.data.frame(GAP.Month.TS.HybridForecast.Train)
GAPHWForecastTrain <- as.data.frame(GAP.Month.TS.HWForecast.Train)
GAPArimaForecastTrain <- as.data.frame(GAP.Month.TS.Train.ArimaForecast)

GAPHybridForecastTrain$mae.95 <- GAPHybridForecastTrain$`Hi 95` - GAPHybridForecastTrain$`Lo 95`
GAPHWForecastTrain$mae.95 <- GAPHWForecastTrain$`Hi 95` - GAPHWForecastTrain$`Lo 95`
GAPArimaForecastTrain$mae.95 <- GAPArimaForecastTrain$`Hi 95`- GAPArimaForecastTrain$`Lo 95`

GAPHybridForecastTrain$mae.80 <- GAPHybridForecastTrain$`Hi 80` - GAPHybridForecastTrain$`Lo 80`
GAPHWForecastTrain$mae.80 <- GAPHWForecastTrain$`Hi 80` - GAPHWForecastTrain$`Lo 80`
GAPArimaForecastTrain$mae.80 <- GAPArimaForecastTrain$`Hi 80`- GAPArimaForecastTrain$`Lo 80`

GAPHybridForecastTrain$mpe.95 <- GAPHybridForecastTrain$mae.95 / GAPHybridForecastTrain$`Hi 95`
GAPHWForecastTrain$mpe.95 <- GAPHWForecastTrain$mae.95 / GAPHWForecastTrain$`Hi 95`
GAPArimaForecastTrain$mpe.95 <- GAPArimaForecastTrain$mae.95 / GAPArimaForecastTrain$`Hi 95`

GAPHybridForecastTrain$mpe.80 <- GAPHybridForecastTrain$mae.80 / GAPHybridForecastTrain$`Hi 80`
GAPHWForecastTrain$mpe.80 <- GAPHWForecastTrain$mae.80 / GAPHWForecastTrain$`Hi 80`
GAPArimaForecastTrain$mpe.80 <- GAPArimaForecastTrain$mae.80 / GAPArimaForecastTrain$`Hi 80`

summary(GAPHybridForecastTrain$mae.95)
summary(GAPHWForecastTrain$mae.95)
summary(GAPArimaForecastTrain$mae.95)

summary(GAPHybridForecastTrain$mae.80)
summary(GAPHWForecastTrain$mae.80)
summary(GAPArimaForecastTrain$mae.80)

summary(GAPHybridForecastTrain$mpe.95)
summary(GAPHWForecastTrain$mpe.95)
summary(GAPArimaForecastTrain$mpe.95)

summary(GAPHybridForecastTrain$mpe.80)
summary(GAPHWForecastTrain$mpe.80)
summary(GAPArimaForecastTrain$mpe.80)

# 2.5.4.2. GRP
GRPHybridForecastTrain <- as.data.frame(GRP.Month.TS.HybridForecast.Train)
GRPHWForecastTrain <- as.data.frame(GRP.Month.TS.HWForecast.Train)
GRPArimaForecastTrain <- as.data.frame(GRP.Month.TS.Train.ArimaForecast)

GRPHybridForecastTrain$mae.95 <- GRPHybridForecastTrain$`Hi 95` - GRPHybridForecastTrain$`Lo 95`
GRPHWForecastTrain$mae.95 <- GRPHWForecastTrain$`Hi 95` - GRPHWForecastTrain$`Lo 95`
GRPArimaForecastTrain$mae.95 <- GRPArimaForecastTrain$`Hi 95`- GRPArimaForecastTrain$`Lo 95`

GRPHybridForecastTrain$mae.80 <- GRPHybridForecastTrain$`Hi 80` - GRPHybridForecastTrain$`Lo 80`
GRPHWForecastTrain$mae.80 <- GRPHWForecastTrain$`Hi 80` - GRPHWForecastTrain$`Lo 80`
GRPArimaForecastTrain$mae.80 <- GRPArimaForecastTrain$`Hi 80`- GRPArimaForecastTrain$`Lo 80`

GRPHybridForecastTrain$mpe.95 <- GRPHybridForecastTrain$mae.95 / GRPHybridForecastTrain$`Hi 95`
GRPHWForecastTrain$mpe.95 <- GRPHWForecastTrain$mae.95 / GRPHWForecastTrain$`Hi 95`
GRPArimaForecastTrain$mpe.95 <- GRPArimaForecastTrain$mae.95 / GRPArimaForecastTrain$`Hi 95`

GRPHybridForecastTrain$mpe.80 <- GRPHybridForecastTrain$mae.80 / GRPHybridForecastTrain$`Hi 80`
GRPHWForecastTrain$mpe.80 <- GRPHWForecastTrain$mae.80 / GRPHWForecastTrain$`Hi 80`
GRPArimaForecastTrain$mpe.80 <- GRPArimaForecastTrain$mae.80 / GRPArimaForecastTrain$`Hi 80`

summary(GRPHybridForecastTrain$mae.95)
summary(GRPHWForecastTrain$mae.95)
summary(GRPArimaForecastTrain$mae.95)

summary(GRPHybridForecastTrain$mae.80)
summary(GRPHWForecastTrain$mae.80)
summary(GRPArimaForecastTrain$mae.80)

summary(GRPHybridForecastTrain$mpe.95)
summary(GRPHWForecastTrain$mpe.95)
summary(GRPArimaForecastTrain$mpe.95)

summary(GRPHybridForecastTrain$mpe.80)
summary(GRPHWForecastTrain$mpe.80)
summary(GRPArimaForecastTrain$mpe.80)


#### 2.6. Applying Model

### 2.6.1. HoltWinters
## 2.6.1.1. HoltWinters to GAP
GAP.Month.TS.HoltWinters <- HoltWinters(GAP.Month.TS, alpha = NULL)
stats:::plot.HoltWinters(GAP.Month.TS.HoltWinters, col = 1, 
                         col.predicted = "green")

GAP.Month.TS.HWForecast <- forecast:::forecast.HoltWinters(GAP.Month.TS.HoltWinters, h = 12)
forecast:::plot.forecast(GAP.Month.TS.HWForecast)

accuracy(GAP.Month.TS.HWForecast)
GAP.Month.TS.HWForecast

## 2.6.1.2. HoltWinters to GRP
GRP.Month.TS.HoltWinters <- HoltWinters(GRP.Month.TS, alpha = NULL)
stats:::plot.HoltWinters(GRP.Month.TS.HoltWinters, col = 1, 
                         col.predicted = "green")

GRP.Month.TS.HWForecast <- forecast:::forecast.HoltWinters(GRP.Month.TS.HoltWinters)
forecast:::plot.forecast(GRP.Month.TS.HWForecast)

accuracy(GRP.Month.TS.HWForecast)


## 2.6.1.3. HoltWinters to Total Consumption
TC.Month.TS.HoltWinters <- HoltWinters(TC.Month.TS, alpha = NULL)
stats:::plot.HoltWinters(TC.Month.TS.HoltWinters, col = 1, 
                         col.predicted = "green")

TC.Month.TS.HWForecast <- forecast:::forecast.HoltWinters(TC.Month.TS.HoltWinters)
forecast:::plot.forecast(TC.Month.TS.HWForecast)

accuracy(TC.Month.TS.HWForecast)

### 2.6.2. Arima

## 2.6.2.1. Arima to GAP
GAP.Month.TS.Arima <- auto.arima(GAP.Month.TS)
plot(GAP.Month.TS.Arima)
GAP.Month.TS.ArimaForecast <- forecast:::forecast.Arima(GAP.Month.TS.Arima, h = 12)
plot(GAP.Month.TS.ArimaForecast)
forecast:::plot.forecast(GAP.Month.TS.ArimaForecast)

accuracy(GAP.Month.TS.ArimaForecast)

## 2.6.2.2. Arima to GRP
GRP.Month.TS.Arima <- auto.arima(GRP.Month.TS)
plot(GRP.Month.TS.Arima)
GRP.Month.TS.ArimaForecast <- forecast:::forecast.Arima(GRP.Month.TS.Arima, h = 12)
plot(GRP.Month.TS.ArimaForecast)

accuracy(GRP.Month.TS.ArimaForecast)

## 2.6.2.3. Arima to Total Consumption
TC.Month.TS.Arima <- auto.arima(TC.Month.TS)
plot(TC.Month.TS.Arima)
TC.Month.TS.ArimaForecast <- forecast:::forecast.Arima(TC.Month.TS.Arima, h = 12)
plot(TC.Month.TS.ArimaForecast)

accuracy(TC.Month.TS.ArimaForecast)

autoplot(GAP.Month.TS) +
  autolayer(GAP.Month.TS.ArimaForecast, PI = FALSE) +
  autolayer(GAP.Month.TS.HWForecast, PI = FALSE) +
  autolayer(GAP.Month.TS.HybridForecast, PI = FALSE)


#2.6.3 Hybrid
#2.6.3.1. Hybrid to GAP
GAP.Month.TS.Hybrid <- hybridModel(GAP.Month.TS, models = "aefnst", lambda = NULL, a.args = NULL,
                                   e.args = NULL, n.args = NULL, s.args = NULL, t.args = NULL,
                                   z.args = NULL, weights = c("equal", "insample.errors", "cv.errors"),
                                   errorMethod = c("RMSE", "MAE", "MASE"), cvHorizon = frequency(y),
                                   windowSize = 84, horizonAverage = FALSE, parallel = FALSE,
                                   num.cores = 2L, verbose = TRUE)

GAP.Month.TS.HybridForecast <- forecastHybrid:::forecast.hybridModel(GAP.Month.TS.Hybrid, h = 12)

#2.6.3.2. Hybrid to GRP
GRP.Month.TS.Hybrid <- hybridModel(GRP.Month.TS, models = "aefnst", lambda = NULL, a.args = NULL,
                                   e.args = NULL, n.args = NULL, s.args = NULL, t.args = NULL,
                                   z.args = NULL, weights = c("equal", "insample.errors", "cv.errors"),
                                   errorMethod = c("RMSE", "MAE", "MASE"), cvHorizon = frequency(y),
                                   windowSize = 84, horizonAverage = FALSE, parallel = FALSE,
                                   num.cores = 2L, verbose = TRUE)

GRP.Month.TS.HybridForecast <- forecastHybrid:::forecast.hybridModel(GRP.Month.TS.Hybrid, h = 12)


## 2.6.4. Opera
# 2.6.4.1. Opera to GAP

h <- length(GAP.Month.TS.Test)
Arima.GAP <- GAP.Month.TS.Train.ArimaForecast
HW.GAP <- GAP.Month.TS.HWForecast.Train
Hybrid.GAP <- GAP.Month.TS.HybridForecast.Train
X <- cbind(Arima.GAP=Arima.GAP$mean, HW.GAP=HW.GAP$mean, Hybrid.GAP=Hybrid.GAP$mean)
monthlyGAP <- cbind(GAP.Month.TS, X)
colnames(monthlyGAP) <- c("Data", "Arima", "HoltWinters", "Hybrid")

autoplot(monthlyGAP) +
  xlab("Year") + ylab("Global Active Power")

MLpol1 <- mixture(model = "MLpol", loss.type = "square")
weights <- predict(MLpol1, X, GAP.Month.TS.Test, type="weights")
head(weights)
tail(weights)

z <- ts(predict(MLpol1, X, GAP.Month.TS.Test, type='response'), start=c(3.85), freq=12)
monthlyGAP2 <- cbind(GAP.Month.TS, z)
colnames(monthlyGAP2) <- c("Data","Mixture")
autoplot(monthlyGAP2) +
  xlab("Year") + ylab(expression("Global Active Power"))



mse <- c(Opera = mean((GAP.Month.TS.Test-z)^2),
         Hybrid = mean((GAP.Month.TS.Test - GAPHybridForecast$mean)^2),
         HW = mean((GAP.Month.TS.Test - GAPHWForecast$mean)^2),
         Arima = mean((GAP.Month.TS.Test - GAPHybridForecast$mean)^2))

# Comparing error metrics
# GAP
GAPHybridForecast <- as.data.frame(GAP.Month.TS.HybridForecast)
GAPHWForecast <- as.data.frame(GAP.Month.TS.HWForecast)
GAPArimaForecast <- as.data.frame(GAP.Month.TS.ArimaForecast)

GAPHybridForecast$mae.95 <- GAPHybridForecast$`Hi 95` - GAPHybridForecast$`Lo 95`
GAPHWForecast$mae.95 <- GAPHWForecast$`Hi 95` - GAPHWForecast$`Lo 95`
GAPArimaForecast$mae.95 <- GAPArimaForecast$`Hi 95`- GAPArimaForecast$`Lo 95`

GAPHybridForecast$mae.80 <- GAPHybridForecast$`Hi 80` - GAPHybridForecast$`Lo 80`
GAPHWForecast$mae.80 <- GAPHWForecast$`Hi 80` - GAPHWForecast$`Lo 80`
GAPArimaForecast$mae.80 <- GAPArimaForecast$`Hi 80`- GAPArimaForecast$`Lo 80`

GAPHybridForecast$mpe.95 <- GAPHybridForecast$mae.95 / GAPHybridForecast$`Hi 95`
GAPHWForecast$mpe.95 <- GAPHWForecast$mae.95 / GAPHWForecast$`Hi 95`
GAPArimaForecast$mpe.95 <- GAPArimaForecast$mae.95 / GAPArimaForecast$`Hi 95`

GAPHybridForecast$mpe.80 <- GAPHybridForecast$mae.80 / GAPHybridForecast$`Hi 80`
GAPHWForecast$mpe.80 <- GAPHWForecast$mae.80 / GAPHWForecast$`Hi 80`
GAPArimaForecast$mpe.80 <- GAPArimaForecast$mae.80 / GAPArimaForecast$`Hi 80`

summary(GAPHybridForecast$mae.95)
summary(GAPHWForecast$mae.95)
summary(GAPArimaForecast$mae.95)

summary(GAPHybridForecast$mae.80)
summary(GAPHWForecast$mae.80)
summary(GAPArimaForecast$mae.80)

summary(GAPHybridForecast$mpe.95)
summary(GAPHWForecast$mpe.95)
summary(GAPArimaForecast$mpe.95)

summary(GAPHybridForecast$mpe.80)
summary(GAPHWForecast$mpe.80)
summary(GAPArimaForecast$mpe.80)

#GRP
GRPHybridForecast <- as.data.frame(GRP.Month.TS.HybridForecast)
GRPHWForecast <- as.data.frame(GRP.Month.TS.HWForecast)
GRPArimaForecast <- as.data.frame(GRP.Month.TS.ArimaForecast)

GRPHybridForecast$mae.95 <- GRPHybridForecast$`Hi 95` - GRPHybridForecast$`Lo 95`
GRPHWForecast$mae.95 <- GRPHWForecast$`Hi 95` - GRPHWForecast$`Lo 95`
GRPArimaForecast$mae.95 <- GRPArimaForecast$`Hi 95`- GRPArimaForecast$`Lo 95`

GRPHybridForecast$mae.80 <- GRPHybridForecast$`Hi 80` - GRPHybridForecast$`Lo 80`
GRPHWForecast$mae.80 <- GRPHWForecast$`Hi 80` - GRPHWForecast$`Lo 80`
GRPArimaForecast$mae.80 <- GRPArimaForecast$`Hi 80`- GRPArimaForecast$`Lo 80`

GRPHybridForecast$mpe.95 <- GRPHybridForecast$mae.95 / GRPHybridForecast$`Hi 95`
GRPHWForecast$mpe.95 <- GRPHWForecast$mae.95 / GRPHWForecast$`Hi 95`
GRPArimaForecast$mpe.95 <- GRPArimaForecast$mae.95 / GRPArimaForecast$`Hi 95`

GRPHybridForecast$mpe.80 <- GRPHybridForecast$mae.80 / GRPHybridForecast$`Hi 80`
GRPHWForecast$mpe.80 <- GRPHWForecast$mae.80 / GRPHWForecast$`Hi 80`
GRPArimaForecast$mpe.80 <- GRPArimaForecast$mae.80 / GRPArimaForecast$`Hi 80`

summary(GRPHybridForecast$mae.95)
summary(GRPHWForecast$mae.95)
summary(GRPArimaForecast$mae.95)

summary(GRPHybridForecast$mae.80)
summary(GRPHWForecast$mae.80)
summary(GRPArimaForecast$mae.80)

summary(GRPHybridForecast$mpe.95)
summary(GRPHWForecast$mpe.95)
summary(GRPArimaForecast$mpe.95)

summary(GRPHybridForecast$mpe.80)
summary(GRPHWForecast$mpe.80)
summary(GRPArimaForecast$mpe.80)



####2.7. Multivariate forecasting

MultiFC <- forecast:::forecast.mts(ts_monthly, h = 12, level = c(80,95))

forecast:::plot.mforecast(MultiFC)






