



#### Energy consumption

#### facts list
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


setwd("C:/Users/User/Desktop/DataAnalytics/41DefineDSProcess")

install.packages("forecast")
install.packages("ggpmisc")
install.packages("TTR")
library(dplyr)
library(lubridate)
library(ggplot2)
library(forecast)
library(ggpmisc)
library(stats)






#### Loading and exploring dataset

library(readr)
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



#### Data preprocessing
### feature engineering

## Converting variables

df$Global_active_power <- (df$Global_active_power*1000)/60
df$Global_reactive_power <- (df$Global_reactive_power*1000)/60

## New features to create: Total Consumption and Total Submetered Consumption

df$TotalConsumption <- df$Global_active_power+df$Global_reactive_power
df$TotalSubCons <- df$Sub_metering_1+df$Sub_metering_2+df$Sub_metering_3
df$NoSubCons <- df$Global_active_power-df$TotalSubCons

df$Year <- substr(df$DateTime,1,4)
df$Year <- as.numeric(df$Year)

df$Month <- substr(df$DateTime,6,7)
df$Month <- as.numeric(df$Month)

## Replacing NA's (when Quim's sentence "less is more" has full of sense)

df[is.na(df)] <- 0

## Subsampling Data

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


MonthlyConsumption <- MonthlyConsumption %>% filter(Year > 2006)

  
  
## Correlation Matrix

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

## Data Visualization

df %>% select(Date,DateTime,TotalConsumption) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=TotalConsumption,col="Consumption"))

df %>% group_by(year(DateTime)) %>% select(Date,DateTime,TotalConsumption) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=TotalConsumption,col="Consumption"))



## Converting to Time Series

library(zoo)    #to deal with NA's when decomposing


ts_annual <- ts(AnnualConsumption, frequency = 5)
ts_monthly <- ts(MonthlyConsumption, frequency = 12)

plot.ts(ts_annual)
plot.ts(ts_monthly)

## outliers

boxplot(ts_annual ~ cycle(ts_annual))
boxplot(ts_monthly ~ cycle(ts_monthly))
tsoutliers

## log
log.ts_annual <- log(ts_annual)
log.ts_monthly <- log(ts_monthly)

plot.ts(log.ts_annual)
plot.ts(log.ts_monthly)

#### Building Models

##Decomposing

GAP.Month.TS <- (ts_monthly[,3])
decompose(GAP.Month.TS)
GAP.Month.TS.Decomposed <-decompose(GAP.Month.TS)
plot(GAP.Month.TS.Decomposed)
autoplot(GAP.Month.TS.Decomposed)

GAP.Month.TS.Decomposed$seasonal
GAP.Month.TS.Decomposed$trend
GAP.Month.TS.Decomposed$random

Active_power_ts_annual <- ts_annual[,2]
decompose(Active_power_ts_annual)
Active_power_ts_decompose_annual<- decompose(Active_power_ts_annual)
plot(Active_power_ts_decompose_annual)


##Adjusting

GAP.Month.TS.Adjusted <- GAP.Month.TS.Decomposed$seasonal + GAP.Month.TS.Decomposed$trend
plot(GAP.Month.TS.Adjusted)       ##NA's fins mitjans de 2007 i a partir
                                  ##de mitjans de 2010. Passa alguna cosa.

##HoltWinters
library("forecast")
month_forecasts <- HoltWinters(ts_monthly)
month_forecasts
month_forecasts$SSE

forecast(month_forecasts)

month_forecasts2 <- forecast(month_forecasts)

plot(month_forecasts)

acf(month_forecasts2)



annual_forecasts <- HoltWinters(ts_annual)
annual_forecasts

plot(annual_forecasts)

#### Applying Models

