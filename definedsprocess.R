

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
library(dplyr)
library(lubridate)
library(ggplot2)
library(forecast)


#### Loading and exploring dataset

df<- read.table("household_power_consumption.txt", sep = ";", dec = ".", header = TRUE, na.strings = c("NA","?"))

df[1:6,]
head(df)


df <-cbind(df,paste(df$Date,df$Time), stringsAsFactors=FALSE)
colnames(df)[10] <-"DateTime"
df <- df[,c(ncol(df), 1:(ncol(df)-1))]

df$DateTime <- strptime(df$DateTime, "%d/%m/%Y %H:%M:%S")
df$Date <- as.Date(df$Date, "%d/%m/%Y")
df$DateTime <- as.POSIXct(df$DateTime)
str(df)

df$DateTime<- dmy_hms(paste(df$Date,df$Time))
df$Date<- dmy(df$Date)
str(df)

##si toco més avall d'aquí comencen els problemes:
df$Time<- hms(df$Time)
df$Date <- as_date(df$Date)
df$DateTime <- as_datetime(df$DateTime)

df$Global_active_power <- as.numeric(df$Global_active_power)
df$Global_intensity <- as.numeric(df$Global_intensity)
df$Voltage <- as.numeric(df$Voltage)
df$Global_intensity <- as.numeric(df$Global_intensity)
df$Sub_metering_1 <- as.numeric(df$Sub_metering_1)
df$Sub_metering_2 <- as.numeric(df$Sub_metering_2)
df$Sub_metering_3 <- as.numeric(df$Sub_metering_3)

#### Data exploration
summary(df)
summary(df$Global_active_power)
summary(df$Global_intensity)
summary(df$Voltage)
summary(df$Global_intensity)
summary(df$Sub_metering_1)
summary(df$Sub_metering_2)
summary(df$Sub_metering_3)

#### Data preprocessing
## Subsetting DF on monthly and annually datasets

df.2006.12 <- subset(df, Date>"2006-12-15" & Date<"2007-01-01")
df.2007.01 <- subset(df, Date>"2006-12-31" & Date<"2007-02-01")
df.2007.02 <- subset(df, Date>"2007-01-31" & Date<"2007-03-01")
df.2007.03 <- subset(df, Date>"2007-02-28" & Date<"2007-04-01")
df.2007.04 <- subset(df, Date>"2007-03-31" & Date<"2007-05-01")
df.2007.05 <- subset(df, Date>"2007-04-30" & Date<"2007-06-01")
df.2007.06 <- subset(df, Date>"2007-05-31" & Date<"2007-07-01")
df.2007.07 <- subset(df, Date>"2007-06-30" & Date<"2007-08-01")
df.2007.08 <- subset(df, Date>"2007-07-31" & Date<"2007-09-01")
df.2007.09 <- subset(df, Date>"2007-08-31" & Date<"2007-10-01")
df.2007.10 <- subset(df, Date>"2007-09-30" & Date<"2007-11-01")
df.2007.11 <- subset(df, Date>"2007-10-31" & Date<"2007-12-01")
df.2007.12 <- subset(df, Date>"2007-11-30" & Date<"2008-01-01")
df.2007 <- subset(df, Date>"2006-12-31" & Date<"2008-01-01")

df.2008.01 <- subset(df, Date>"2007-12-31" & Date<"2008-02-01")
df.2008.02 <- subset(df, Date>"2008-01-31" & Date<"2008-03-01")
df.2008.03 <- subset(df, Date>"2008-02-29" & Date<"2008-04-01")
df.2008.04 <- subset(df, Date>"2008-03-31" & Date<"2008-05-01")
df.2008.05 <- subset(df, Date>"2008-04-30" & Date<"2008-06-01")
df.2008.06 <- subset(df, Date>"2008-05-31" & Date<"2008-07-01")
df.2008.07 <- subset(df, Date>"2008-06-30" & Date<"2008-08-01")
df.2008.08 <- subset(df, Date>"2008-07-31" & Date<"2008-09-01")
df.2008.09 <- subset(df, Date>"2008-08-31" & Date<"2008-10-01")
df.2008.10 <- subset(df, Date>"2008-09-30" & Date<"2008-11-01")
df.2008.11 <- subset(df, Date>"2008-10-31" & Date<"2008-12-01")
df.2008.12 <- subset(df, Date>"2008-11-30" & Date<"2009-01-01")
df.2008 <- subset(df, Date>"2007-12-31" & Date<"2009-01-01")

df.2009.01 <- subset(df, Date>"2008-12-31" & Date<"2009-02-01")
df.2009.02 <- subset(df, Date>"2009-01-31" & Date<"2009-03-01")
df.2009.03 <- subset(df, Date>"2009-02-28" & Date<"2009-04-01")
df.2009.04 <- subset(df, Date>"2009-03-31" & Date<"2009-05-01")
df.2009.05 <- subset(df, Date>"2009-04-30" & Date<"2009-06-01")
df.2009.06 <- subset(df, Date>"2009-05-31" & Date<"2009-07-01")
df.2009.07 <- subset(df, Date>"2009-06-30" & Date<"2009-08-01")
df.2009.08 <- subset(df, Date>"2009-07-31" & Date<"2009-09-01")
df.2009.09 <- subset(df, Date>"2009-08-31" & Date<"2009-10-01")
df.2009.10 <- subset(df, Date>"2009-09-30" & Date<"2009-11-01")
df.2009.11 <- subset(df, Date>"2009-10-31" & Date<"2009-12-01")
df.2009.12 <- subset(df, Date>"2009-11-30" & Date<"2010-01-01")
df.2009 <- subset(df, Date>"2008-12-31" & Date<"2010-01-01")

df.2010.01 <- subset(df, Date>"2009-12-31" & Date<"2010-02-01")
df.2010.02 <- subset(df, Date>"2010-01-31" & Date<"2010-03-01")
df.2010.03 <- subset(df, Date>"2010-02-28" & Date<"2010-04-01")
df.2010.04 <- subset(df, Date>"2010-03-31" & Date<"2010-05-01")
df.2010.05 <- subset(df, Date>"2010-04-30" & Date<"2010-06-01")
df.2010.06 <- subset(df, Date>"2010-05-31" & Date<"2010-07-01")
df.2010.07 <- subset(df, Date>"2010-06-30" & Date<"2010-08-01")
df.2010.08 <- subset(df, Date>"2010-07-31" & Date<"2010-09-01")
df.2010.09 <- subset(df, Date>"2010-08-31" & Date<"2010-10-01")
df.2010.10 <- subset(df, Date>"2010-09-30" & Date<"2010-11-01")
df.2010.11 <- subset(df, Date>"2010-10-31" & Date<"2010-12-01")
df.2010 <- subset(df, Date>"2009-12-31" & Date<"2011-01-01")


## missing values location
#whole observations
df %>% select(Date, DateTime, Sub_metering_1) %>% 
  filter(Date > dmy("15-12-2006")) %>% filter(Date < dmy("31-12-2010")) %>% summary()

#1q 2007 (5 observations)
df %>% select(Date, DateTime, Sub_metering_1)%>% filter(is.na(Sub_metering_1)) %>% filter(Date > dmy("31-12-2006")) %>% 
  filter(Date < dmy("01-04-2007")) %>% summary()

#2Q 2007 (3766 observations)
df %>% select(Date, DateTime, Sub_metering_1)%>% filter(is.na(Sub_metering_1)) %>% filter(Date > dmy("31-03-2007")) %>% 
  filter(Date < dmy("01-07-2007")) %>% summary()

#2q 2007 / april (3723 observations)
df %>% select(Date, DateTime, Sub_metering_1)%>% filter(is.na(Sub_metering_1)) %>% filter(Date > dmy("31-03-2007")) %>% 
  filter(Date < dmy("01-05-2007")) %>% summary()

#2q 2007 / may (no observations)
df %>% select(Date, DateTime, Sub_metering_1)%>% filter(is.na(Sub_metering_1)) %>% filter(Date > dmy("31-04-2007")) %>% 
  filter(Date < dmy("01-06-2007")) %>% summary()

#2q 2007 / june (43 observations)
df %>% select(Date, DateTime, Sub_metering_1)%>% filter(is.na(Sub_metering_1)) %>% filter(Date > dmy("31-05-2007")) %>% 
  filter(Date < dmy("01-07-2007")) %>% summary()

#3q 2007 (155 observations)
df %>% select(Date, DateTime, Sub_metering_1)%>% filter(is.na(Sub_metering_1)) %>% filter(Date > dmy("30-06-2007")) %>% 
  filter(Date < dmy("01-10-2007")) %>% summary()

#4q 2007 (5 observations)
df %>% select(Date, DateTime, Sub_metering_1)%>% filter(is.na(Sub_metering_1)) %>% filter(Date > dmy("30-09-2007")) %>% 
  filter(Date < dmy("01-01-2008")) %>% summary()

#1q 2008 (5 observations)
df %>% select(Date, DateTime, Sub_metering_1)%>% filter(is.na(Sub_metering_1)) %>% filter(Date > dmy("31-12-2007")) %>% 
  filter(Date < dmy("01-04-2008")) %>% summary()

#2q 2008 (3 observations)
df %>% select(Date, DateTime, Sub_metering_1)%>% filter(is.na(Sub_metering_1)) %>% filter(Date > dmy("31-03-2008")) %>% 
  filter(Date < dmy("01-07-2008")) %>% summary()

#3q 2008 (4 observations)
df %>% select(Date, DateTime, Sub_metering_1)%>% filter(is.na(Sub_metering_1)) %>% filter(Date > dmy("30-06-2008")) %>% 
  filter(Date < dmy("01-10-2008")) %>% summary()

#4q 2008 (123 observations)
df %>% select(Date, DateTime, Sub_metering_1)%>% filter(is.na(Sub_metering_1)) %>% filter(Date > dmy("30-09-2008")) %>% 
  filter(Date < dmy("01-01-2009")) %>% summary()

#4q 2008 / october (43 observations, all on 25/10)
df %>% select(Date, DateTime, Sub_metering_1)%>% filter(is.na(Sub_metering_1)) %>% filter(Date > dmy("30-09-2008")) %>% 
  filter(Date < dmy("01-11-2008")) %>% summary()

#4q 2008 / november (9 observations)
df %>% select(Date, DateTime, Sub_metering_1)%>% filter(is.na(Sub_metering_1)) %>% filter(Date > dmy("31-10-2008")) %>% 
  filter(Date < dmy("01-12-2008")) %>% summary()

#4q 2008 / december (71 observations, between 10/12 and 20/12)
df %>% select(Date, DateTime, Sub_metering_1)%>% filter(is.na(Sub_metering_1)) %>% filter(Date > dmy("30-11-2008")) %>% 
  filter(Date < dmy("01-01-2009")) %>% summary()

#1q 2009 (67 observations)
df %>% select(Date, DateTime, Sub_metering_1)%>% filter(is.na(Sub_metering_1)) %>% filter(Date > dmy("31-12-2008")) %>% 
  filter(Date < dmy("01-04-2009")) %>% summary()

#2q 2009 (3311 observations)
df %>% select(Date, DateTime, Sub_metering_1)%>% filter(is.na(Sub_metering_1)) %>% filter(Date > dmy("31-03-2009")) %>% 
  filter(Date < dmy("01-07-2009")) %>% summary()

#2q 2009 / april (2 observations, 13/4)
df %>% select(Date, DateTime, Sub_metering_1)%>% filter(is.na(Sub_metering_1)) %>% filter(Date > dmy("31-03-2009")) %>% 
  filter(Date < dmy("01-05-2009")) %>% summary()

#2q 2009 / may (4 observations, between 10/5 and 26/5)
df %>% select(Date, DateTime, Sub_metering_1)%>% filter(is.na(Sub_metering_1)) %>% filter(Date > dmy("30-04-2009")) %>% 
  filter(Date < dmy("01-06-2009")) %>% summary()

#2q 2009 / june (3305 observations, between 13/6 and 15/6)
df %>% select(Date, DateTime, Sub_metering_1)%>% filter(is.na(Sub_metering_1)) %>% filter(Date > dmy("31-05-2009")) %>% 
  filter(Date < dmy("01-07-2009")) %>% summary()

#3q 2009 (898 observations)
df %>% select(Date, DateTime, Sub_metering_1)%>% filter(is.na(Sub_metering_1)) %>% filter(Date > dmy("30-06-2009")) %>% 
  filter(Date < dmy("01-10-2009")) %>% summary()

#3q 2009 / july (4 observations, all on 10/7)
df %>% select(Date, DateTime, Sub_metering_1)%>% filter(is.na(Sub_metering_1)) %>% filter(Date > dmy("30-06-2009")) %>% 
  filter(Date < dmy("01-08-2009")) %>% summary()

#3q 2009 / august (891 observations, all on 13/8)
df %>% select(Date, DateTime, Sub_metering_1)%>% filter(is.na(Sub_metering_1)) %>% filter(Date > dmy("31-07-2009")) %>% 
  filter(Date < dmy("01-09-2009")) %>% summary()

#3q 2009 / september (3 observations)
df %>% select(Date, DateTime, Sub_metering_1)%>% filter(is.na(Sub_metering_1)) %>% filter(Date > dmy("31-08-2009")) %>% 
  filter(Date < dmy("01-10-2009")) %>% summary()

#4q 2009 (4 observations)
df %>% select(Date, DateTime, Sub_metering_1)%>% filter(is.na(Sub_metering_1)) %>% filter(Date > dmy("30-09-2009")) %>% 
  filter(Date < dmy("01-01-2010")) %>% summary()

#1q 2010 (5160 observations)
df %>% select(Date, DateTime, Sub_metering_1)%>% filter(is.na(Sub_metering_1)) %>% filter(Date > dmy("31-12-2009")) %>% 
  filter(Date < dmy("01-04-2010")) %>% summary()

#1q 2010 / january (3131 observations, between 2/1 and 23/1)
df %>% select(Date, DateTime, Sub_metering_1)%>% filter(is.na(Sub_metering_1)) %>% filter(Date > dmy("31-12-2009")) %>% 
  filter(Date < dmy("01-02-2010")) %>% summary()

#1q 2010 / february (2 observations)
df %>% select(Date, DateTime, Sub_metering_1)%>% filter(is.na(Sub_metering_1)) %>% filter(Date > dmy("31-01-2010")) %>% 
  filter(Date < dmy("01-03-2010")) %>% summary()

#1q 2010 / march (2027 observations, between 20/3 and 21/3)
df %>% select(Date, DateTime, Sub_metering_1)%>% filter(is.na(Sub_metering_1)) %>% filter(Date > dmy("28-02-2010")) %>% 
  filter(Date < dmy("01-04-2010")) %>% summary()

#2q 2010 (4 observations)
df %>% select(Date, DateTime, Sub_metering_1)%>% filter(is.na(Sub_metering_1)) %>% filter(Date > dmy("31-03-2010")) %>% 
  filter(Date < dmy("01-07-2010")) %>% summary()

#3q 2010 (12464 observations)
df %>% select(Date, DateTime, Sub_metering_1)%>% filter(is.na(Sub_metering_1)) %>% filter(Date > dmy("30-06-2010")) %>% 
  filter(Date < dmy("01-10-2010")) %>% summary()

#3q 2010 / august (7226 observations, between 17/8 and 22/8)
df %>% select(Date, DateTime, Sub_metering_1)%>% filter(is.na(Sub_metering_1)) %>% filter(Date > dmy("30-07-2010")) %>% 
  filter(Date < dmy("01-09-2010")) %>% summary()

#3q 2010 / september (5237 observations, between 25/9 and 28/9)
df %>% select(Date, DateTime, Sub_metering_1)%>% filter(is.na(Sub_metering_1)) %>% filter(Date > dmy("31-08-2010")) %>% 
  filter(Date < dmy("01-10-2010")) %>% summary()

#4q 2010 (1 observation, 24/10 at 15:35)
df %>% select(Date, DateTime, Sub_metering_1)%>% filter(is.na(Sub_metering_1)) %>% filter(Date > dmy("30-09-2010")) %>% 
  filter(Date < dmy("01-01-2011")) %>% summary()

## features seasonal behaviour, visualization

#Global Active Power
#Whole observations
df %>% select(Date, Time, Global_active_power) %>% 
  filter(Date > dmy("15-12-2006")) %>% filter(Date < dmy("31-12-2010")) %>% summary()

df %>% group_by(year(Date), month(Date)) %>% summarise_all(mean,na.rm = TRUE)


#2006
df %>% select(Date, Time, Global_active_power) %>% filter(Date > dmy("15-12-2006")) %>% filter(Date < dmy("01-01-2007")) %>% summary()

#2007
df %>% filter(Date > dmy("31-12-2006")) %>% filter(Date < dmy("01-01-2008")) %>% 
  select(Global_active_power) %>% summary(Global_active_power)

#2008
df %>% filter(Date > dmy("31-12-2007")) %>% filter(Date < dmy("01-01-2009")) %>% 
  select(Global_active_power) %>% summary(Global_active_power)

#2009
df %>% filter(Date > dmy("31-12-2008")) %>% filter(Date < dmy("01-01-2010")) %>% 
  select(Global_active_power) %>% summary(Global_active_power)

#2010
df %>% filter(Date > dmy("31-12-2009")) %>% filter(Date < dmy("01-01-2011")) %>% 
  select(Global_active_power) %>% summary(Global_active_power)

#Global Reactive Power
#Whole observations
df %>% filter(Date > dmy("15-12-2006")) %>% filter(Date < dmy("31-12-2010")) %>% 
  select(Global_reactive_power) %>% summary(Global_reactive_power)

#2006
df %>% filter(Date > dmy("15-12-2006")) %>% filter(Date < dmy("01-01-2007")) %>% 
  select(Global_reactive_power) %>% summary(Global_reactive_power)

#2007
df %>% filter(Date > dmy("31-12-2006")) %>% filter(Date < dmy("01-01-2008")) %>% 
  select(Global_reactive_power) %>% summary(Global_reactive_power)

#2008
df %>% filter(Date > dmy("31-12-2007")) %>% filter(Date < dmy("01-01-2009")) %>% 
  select(Global_reactive_power) %>% summary(Global_reactive_power)

#2009
df %>% filter(Date > dmy("31-12-2008")) %>% filter(Date < dmy("01-01-2010")) %>% 
  select(Global_reactive_power) %>% summary(Global_reactive_power)

#2010
df %>% filter(Date > dmy("31-12-2009")) %>% filter(Date < dmy("01-01-2011")) %>% 
  select(Global_reactive_power) %>% summary(Global_reactive_power)


#Voltage
#Whole observations
df %>% filter(Date > dmy("15-12-2006")) %>% filter(Date < dmy("31-12-2010")) %>% 
  select(Voltage) %>% summary(Voltage)

#2006
df %>% filter(Date > dmy("15-12-2006")) %>% filter(Date < dmy("01-01-2007")) %>% 
  select(Voltage) %>% summary(Voltage)

#2007
df %>% filter(Date > dmy("31-12-2006")) %>% filter(Date < dmy("01-01-2008")) %>% 
  select(Voltage) %>% summary(Voltage)

#2008
df %>% filter(Date > dmy("31-12-2007")) %>% filter(Date < dmy("01-01-2009")) %>% 
  select(Voltage) %>% summary(Voltage)

#2009
df %>% filter(Date > dmy("31-12-2008")) %>% filter(Date < dmy("01-01-2010")) %>% 
  select(Voltage) %>% summary(Voltage)

#2010
df %>% filter(Date > dmy("31-12-2009")) %>% filter(Date < dmy("01-01-2011")) %>% 
  select(Voltage) %>% summary(Voltage)

#Global Intensity
#Whole observations
df %>% filter(Date > dmy("15-12-2006")) %>% filter(Date < dmy("31-12-2010")) %>% 
  select(Global_intensity) %>% summary(Global_intensity)

#2006
df %>% filter(Date > dmy("15-12-2006")) %>% filter(Date < dmy("01-01-2007")) %>% 
  select(Global_intensity) %>% summary(Global_intensity)

#2007
df %>% filter(Date > dmy("31-12-2006")) %>% filter(Date < dmy("01-01-2008")) %>% 
  select(Global_intensity) %>% summary(Global_intensity)

#2008
df %>% filter(Date > dmy("31-12-2007")) %>% filter(Date < dmy("01-01-2009")) %>% 
  select(Global_intensity) %>% summary(Global_intensity)

#2009
df %>% filter(Date > dmy("31-12-2008")) %>% filter(Date < dmy("01-01-2010")) %>% 
  select(Global_intensity) %>% summary(Global_intensity)

#2010
df %>% filter(Date > dmy("31-12-2009")) %>% filter(Date < dmy("01-01-2011")) %>% 
  select(Global_intensity) %>% summary(Global_intensity)

#Submeter 1 (Kitchen)
#Whole observations
df %>% filter(Date > dmy("15-12-2006")) %>% filter(Date < dmy("31-12-2010")) %>% 
  select(Sub_metering_1) %>% summary(Sub_metering_1)

#2006
df %>% filter(Date > dmy("15-12-2006")) %>% filter(Date < dmy("01-01-2007")) %>% 
  select(Sub_metering_1) %>% summary(Sub_metering_1)

#2007
df %>% filter(Date > dmy("31-12-2006")) %>% filter(Date < dmy("01-01-2008")) %>% 
  select(Sub_metering_1) %>% summary(Sub_metering_1)

#2008
df %>% filter(Date > dmy("31-12-2007")) %>% filter(Date < dmy("01-01-2009")) %>% 
  select(Sub_metering_1) %>% summary(Sub_metering_1)

#2009
df %>% filter(Date > dmy("31-12-2008")) %>% filter(Date < dmy("01-01-2010")) %>% 
  select(Sub_metering_1) %>% summary(Sub_metering_1)

#2010
df %>% filter(Date > dmy("31-12-2009")) %>% filter(Date < dmy("01-01-2011")) %>% 
  select(Sub_metering_1) %>% summary(Sub_metering_1)


#Submeter 2 (Laundry)
#Whole observations
df %>% filter(Date > dmy("15-12-2006")) %>% filter(Date < dmy("31-12-2010")) %>% 
  select(Sub_metering_2) %>% summary(Sub_metering_2)

#2006
df %>% filter(Date > dmy("15-12-2006")) %>% filter(Date < dmy("01-01-2007")) %>% 
  select(Sub_metering_2) %>% summary(Sub_metering_2)

#2007
df %>% filter(Date > dmy("31-12-2006")) %>% filter(Date < dmy("01-01-2008")) %>% 
  select(Sub_metering_2) %>% summary(Sub_metering_2)

#2008
df %>% filter(Date > dmy("31-12-2007")) %>% filter(Date < dmy("01-01-2009")) %>% 
  select(Sub_metering_2) %>% summary(Sub_metering_2)

#2009
df %>% filter(Date > dmy("31-12-2008")) %>% filter(Date < dmy("01-01-2010")) %>% 
  select(Sub_metering_2) %>% summary(Sub_metering_2)

#2010
df %>% filter(Date > dmy("31-12-2009")) %>% filter(Date < dmy("01-01-2011")) %>% 
  select(Sub_metering_2) %>% summary(Sub_metering_2)


#Submeter 3 (Climatization)
#Whole observations
df %>% filter(Date > dmy("15-12-2006")) %>% filter(Date < dmy("31-12-2010")) %>% 
  select(Sub_metering_3) %>% summary(Sub_metering_3)

#2006
df %>% filter(Date > dmy("15-12-2006")) %>% filter(Date < dmy("01-01-2007")) %>% 
  select(Sub_metering_3) %>% summary(Sub_metering_3)

#2007
df %>% filter(Date > dmy("31-12-2006")) %>% filter(Date < dmy("01-01-2008")) %>% 
  select(Sub_metering_3) %>% summary(Sub_metering_3)

#2008
df %>% filter(Date > dmy("31-12-2007")) %>% filter(Date < dmy("01-01-2009")) %>% 
  select(Sub_metering_3) %>% summary(Sub_metering_3)

#2009
df %>% filter(Date > dmy("31-12-2008")) %>% filter(Date < dmy("01-01-2010")) %>% 
  select(Sub_metering_3) %>% summary(Sub_metering_3)

#2010
df %>% filter(Date > dmy("31-12-2009")) %>% filter(Date < dmy("01-01-2011")) %>% 
  select(Sub_metering_3) %>% summary(Sub_metering_3)

####Feature relationships

### S1 (Kitchen) vs. S2 (Laundry)
##2007
df.2007 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry"))

#2007.01
df.2007.01 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry"))

#2007.02
df.2007.02 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry"))

#2007.03
df.2007.03 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry"))

#2007.04
df.2007.04 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry"))

#2007.05
df.2007.05 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry"))

#2007.06
df.2007.06 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry"))

#2007.07
df.2007.07 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry"))

#2007.08
df.2007.08 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry"))

#2007.09
df.2007.09 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry"))

#2007.10
df.2007.10 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry"))

#2007.11
df.2007.11 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry"))

#2007.12
df.2007.12 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry"))

#2007.1q

#2007.2q

#2007.3q

#2007.4q

#2008
df.2008 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry"))

#2008.01
df.2008.01 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry"))


#2008.02
df.2008.02 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry"))

#2008.03
df.2008.03 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry"))

#2008.04
df.2008.04 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry"))

#2008.05
df.2008.05 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry"))

#2008.06
df.2008.06 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry"))

#2008.07
df.2008.07 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry"))

#2008.08
df.2008.08 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry"))

#2008.09
df.2008.09 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry"))

#2008.10
df.2008.10 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry"))

#2008.11
df.2008.11 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry"))

#2008.12
df.2008.12 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry"))

#2009
df.2009 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry"))

#2009.01
df.2008.01 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry"))

df.2009.01 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry"))

#2009.02
df.2009.02 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry"))

#2009.03
df.2009.03 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry"))

#2009.04
df.2009.04 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry"))

#2009.05
df.2009.05 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry"))

#2009.06
df.2009.06 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry"))

#2009.07
df.2009.07 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry"))

#2009.08
df.2009.08 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry"))

#2009.09
df.2009.09 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry"))

#2009.10
df.2009.10 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry"))

#2009.11
df.2009.11 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry"))

#2009.12
df.2009.12 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry"))

#2010
df.2010 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry"))

#2010.01
df.2010.01 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry"))

#2010.02
df.2007.02 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry"))

df.2008.02 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry"))

df.2009.02 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry"))

df.2010.02 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry"))

#2010.03
df.2010.03 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry"))

#2010.04
df.2010.04 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry"))

#2010.05
df.2010.05 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry"))

#2010.06
df.2010.06 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry"))

#2010.07
df.2010.07 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry"))

#2010.08
df.2010.08 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry"))

#2010.09
df.2010.09 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry"))

#2010.10
df.2010.10 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry"))

#2010.11
df.2010.11 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry"))


## S1 (Kitchen) vs. S3 (Climatization)
##2007
df.2007 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2007.01
df.2007.01 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2007.02
df.2007.02 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2007.03
df.2007.03 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2007.04
df.2007.04 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2007.05
df.2007.05 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2007.06
df.2007.06 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2007.07
df.2007.07 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2007.08
df.2007.08 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2007.09
df.2007.09 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2007.10
df.2007.10 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2007.11
df.2007.11 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2007.12
df.2007.12 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2007.1q

#2007.2q

#2007.3q

#2007.4q

#2008
df.2008 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2008.01
df.2008.01 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))


#2008.02
df.2008.02 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2008.03
df.2008.03 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2008.04
df.2008.04 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2008.05
df.2008.05 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2008.06
df.2008.06 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2008.07
df.2008.07 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2008.08
df.2008.08 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2008.09
df.2008.09 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2008.10
df.2008.10 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2008.11
df.2008.11 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2008.12
df.2008.12 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2009
df.2009 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2009.01
df.2008.01 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

df.2009.01 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2009.02
df.2009.02 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2009.03
df.2009.03 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2009.04
df.2009.04 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2009.05
df.2009.05 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2009.06
df.2009.06 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2009.07
df.2009.07 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2009.08
df.2009.08 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2009.09
df.2009.09 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2009.10
df.2009.10 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2009.11
df.2009.11 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2009.12
df.2009.12 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2010
df.2010 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2010.01
df.2007.01 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

df.2008.01 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

df.2009.01 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

df.2010.01 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2010.02
df.2007.02 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

df.2008.02 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

df.2009.02 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

df.2010.02 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2010.03
df.2010.03 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2010.04
df.2010.04 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2010.05
df.2010.05 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2010.06
df.2010.06 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2010.07
df.2010.07 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2010.08
df.2010.08 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2010.09
df.2010.09 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2010.10
df.2010.10 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2010.11
df.2010.11 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))


## S2 (Laundry) vs. S3 (Climatization)
##2007
df.2007 %>% select(Date,DateTime,Sub_metering_2,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2007.01
df.2007.01 %>% select(Date,DateTime,Sub_metering_2,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2007.02
df.2007.02 %>% select(Date,DateTime,Sub_metering_2,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2007.03
df.2007.03 %>% select(Date,DateTime,Sub_metering_2,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2007.04
df.2007.04 %>% select(Date,DateTime,Sub_metering_2,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2007.05
df.2007.05 %>% select(Date,DateTime,Sub_metering_2,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2007.06
df.2007.06 %>% select(Date,DateTime,Sub_metering_2,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2007.07
df.2007.07 %>% select(Date,DateTime,Sub_metering_2,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2007.08
df.2007.08 %>% select(Date,DateTime,Sub_metering_2,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2007.09
df.2007.09 %>% select(Date,DateTime,Sub_metering_2,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2007.10
df.2007.10 %>% select(Date,DateTime,Sub_metering_2,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2007.11
df.2007.11 %>% select(Date,DateTime,Sub_metering_2,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2007.12
df.2007.12 %>% select(Date,DateTime,Sub_metering_2,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2007.1q

#2007.2q

#2007.3q

#2007.4q

#2008
df.2008 %>% select(Date,DateTime,Sub_metering_2,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2008.01
df.2008.01 %>% select(Date,DateTime,Sub_metering_2,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))


#2008.02
df.2008.02 %>% select(Date,DateTime,Sub_metering_2,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2008.03
df.2008.03 %>% select(Date,DateTime,Sub_metering_2,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2008.04
df.2008.04 %>% select(Date,DateTime,Sub_metering_2,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2008.05
df.2008.05 %>% select(Date,DateTime,Sub_metering_2,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2008.06
df.2008.06 %>% select(Date,DateTime,Sub_metering_2,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2008.07
df.2008.07 %>% select(Date,DateTime,Sub_metering_2,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2008.08
df.2008.08 %>% select(Date,DateTime,Sub_metering_2,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2008.09
df.2008.09 %>% select(Date,DateTime,Sub_metering_2,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2008.10
df.2008.10 %>% select(Date,DateTime,Sub_metering_2,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2008.11
df.2008.11 %>% select(Date,DateTime,Sub_metering_2,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2008.12
df.2008.12 %>% select(Date,DateTime,Sub_metering_2,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2009
df.2009 %>% select(Date,DateTime,Sub_metering_2,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2009.01
df.2008.01 %>% select(Date,DateTime,Sub_metering_2,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

df.2009.01 %>% select(Date,DateTime,Sub_metering_2,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2009.02
df.2009.02 %>% select(Date,DateTime,Sub_metering_2,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2009.03
df.2009.03 %>% select(Date,DateTime,Sub_metering_2,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2009.04
df.2009.04 %>% select(Date,DateTime,Sub_metering_2,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2009.05
df.2009.05 %>% select(Date,DateTime,Sub_metering_2,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2009.06
df.2009.06 %>% select(Date,DateTime,Sub_metering_2,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2009.07
df.2009.07 %>% select(Date,DateTime,Sub_metering_2,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2009.08
df.2009.08 %>% select(Date,DateTime,Sub_metering_2,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2009.09
df.2009.09 %>% select(Date,DateTime,Sub_metering_2,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2009.10
df.2009.10 %>% select(Date,DateTime,Sub_metering_2,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2009.11
df.2009.11 %>% select(Date,DateTime,Sub_metering_2,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2009.12
df.2009.12 %>% select(Date,DateTime,Sub_metering_2,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2010
df.2010 %>% select(Date,DateTime,Sub_metering_2,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2010.01
df.2007.01 %>% select(Date,DateTime,Sub_metering_2,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

df.2008.01 %>% select(Date,DateTime,Sub_metering_2,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

df.2009.01 %>% select(Date,DateTime,Sub_metering_2,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

df.2010.01 %>% select(Date,DateTime,Sub_metering_2,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2010.02
df.2007.02 %>% select(Date,DateTime,Sub_metering_2,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

df.2008.02 %>% select(Date,DateTime,Sub_metering_2,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

df.2009.02 %>% select(Date,DateTime,Sub_metering_2,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

df.2010.02 %>% select(Date,DateTime,Sub_metering_2,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2010.03
df.2010.03 %>% select(Date,DateTime,Sub_metering_2,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2010.04
df.2010.04 %>% select(Date,DateTime,Sub_metering_2,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2010.05
df.2010.05 %>% select(Date,DateTime,Sub_metering_2,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2010.06
df.2010.06 %>% select(Date,DateTime,Sub_metering_2,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2010.07
df.2010.07 %>% select(Date,DateTime,Sub_metering_2,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2010.08
df.2010.08 %>% select(Date,DateTime,Sub_metering_2,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2010.09
df.2010.09 %>% select(Date,DateTime,Sub_metering_2,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2010.10
df.2010.10 %>% select(Date,DateTime,Sub_metering_2,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2010.11
df.2010.11 %>% select(Date,DateTime,Sub_metering_2,Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

## All Submeters (S1-S2-S3)
##2007
df.2007 %>% select(Date,DateTime,Sub_metering_1, Sub_metering_2, Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) + 
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2007.01
df.2007.01 %>% select(Date,DateTime,Sub_metering_1, Sub_metering_2, Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) + 
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2007.02
df.2007.02 %>% select(Date,DateTime,Sub_metering_1, Sub_metering_2, Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) + 
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2007.03
df.2007.03 %>% select(Date,DateTime,Sub_metering_1, Sub_metering_2, Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) + 
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2007.04
df.2007.04 %>% select(Date,DateTime,Sub_metering_1, Sub_metering_2, Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) + 
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2007.05
df.2007.05 %>% select(Date,DateTime,Sub_metering_1, Sub_metering_2, Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) + 
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2007.06
df.2007.06 %>% select(Date,DateTime,Sub_metering_1, Sub_metering_2, Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) + 
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2007.07
df.2007.07 %>% select(Date,DateTime,Sub_metering_1, Sub_metering_2, Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) + 
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2007.08
df.2007.08 %>% select(Date,DateTime,Sub_metering_1, Sub_metering_2, Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) + 
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2007.09
df.2007.09 %>% select(Date,DateTime,Sub_metering_1, Sub_metering_2, Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) + 
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2007.10
df.2007.10 %>% select(Date,DateTime,Sub_metering_1, Sub_metering_2, Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) + 
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2007.11
df.2007.11 %>% select(Date,DateTime,Sub_metering_1, Sub_metering_2, Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) + 
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2007.12
df.2007.12 %>% select(Date,DateTime,Sub_metering_1, Sub_metering_2, Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) + 
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2007.1q

#2007.2q

#2007.3q

#2007.4q

#2008
df.2008 %>% select(Date,DateTime,Sub_metering_1, Sub_metering_2, Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) + 
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2008.01
df.2008.01 %>% select(Date,DateTime,Sub_metering_1, Sub_metering_2, Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) + 
  geom_line(aes(y=Sub_metering_3,col="Climatization"))


#2008.02
df.2008.02 %>% select(Date,DateTime,Sub_metering_1, Sub_metering_2, Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) + 
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2008.03
df.2008.03 %>% select(Date,DateTime,Sub_metering_1, Sub_metering_2, Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) + 
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2008.04
df.2008.04 %>% select(Date,DateTime,Sub_metering_1, Sub_metering_2, Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) + 
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2008.05
df.2008.05 %>% select(Date,DateTime,Sub_metering_1, Sub_metering_2, Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) + 
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2008.06
df.2008.06 %>% select(Date,DateTime,Sub_metering_1, Sub_metering_2, Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) + 
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2008.07
df.2008.07 %>% select(Date,DateTime,Sub_metering_1, Sub_metering_2, Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) + 
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2008.08
df.2008.08 %>% select(Date,DateTime,Sub_metering_1, Sub_metering_2, Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) + 
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2008.09
df.2008.09 %>% select(Date,DateTime,Sub_metering_1, Sub_metering_2, Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) + 
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2008.10
df.2008.10 %>% select(Date,DateTime,Sub_metering_1, Sub_metering_2, Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) + 
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2008.11
df.2008.11 %>% select(Date,DateTime,Sub_metering_1, Sub_metering_2, Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) + 
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2008.12
df.2008.12 %>% select(Date,DateTime,Sub_metering_1, Sub_metering_2, Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) + 
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2009
df.2009 %>% select(Date,DateTime,Sub_metering_1, Sub_metering_2, Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) + 
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2009.01
df.2008.01 %>% select(Date,DateTime,Sub_metering_1, Sub_metering_2, Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) + 
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

df.2009.01 %>% select(Date,DateTime,Sub_metering_1, Sub_metering_2, Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) + 
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2009.02
df.2009.02 %>% select(Date,DateTime,Sub_metering_1, Sub_metering_2, Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) + 
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2009.03
df.2009.03 %>% select(Date,DateTime,Sub_metering_1, Sub_metering_2, Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) + 
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2009.04
df.2009.04 %>% select(Date,DateTime,Sub_metering_1, Sub_metering_2, Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) + 
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2009.05
df.2009.05 %>% select(Date,DateTime,Sub_metering_1, Sub_metering_2, Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) + 
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2009.06
df.2009.06 %>% select(Date,DateTime,Sub_metering_1, Sub_metering_2, Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) + 
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2009.07
df.2009.07 %>% select(Date,DateTime,Sub_metering_1, Sub_metering_2, Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) + 
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2009.08
df.2009.08 %>% select(Date,DateTime,Sub_metering_1, Sub_metering_2, Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) + 
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2009.09
df.2009.09 %>% select(Date,DateTime,Sub_metering_1, Sub_metering_2, Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) + 
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2009.10
df.2009.10 %>% select(Date,DateTime,Sub_metering_1, Sub_metering_2, Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) + 
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2009.11
df.2009.11 %>% select(Date,DateTime,Sub_metering_1, Sub_metering_2, Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) + 
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2009.12
df.2009.12 %>% select(Date,DateTime,Sub_metering_1, Sub_metering_2, Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) + 
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2010
df.2010 %>% select(Date,DateTime,Sub_metering_1, Sub_metering_2, Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) + 
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2010.01
df.2007.01 %>% select(Date,DateTime,Sub_metering_1, Sub_metering_2, Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) + 
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

df.2008.01 %>% select(Date,DateTime,Sub_metering_1, Sub_metering_2, Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) + 
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

df.2009.01 %>% select(Date,DateTime,Sub_metering_1, Sub_metering_2, Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) + 
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

df.2010.01 %>% select(Date,DateTime,Sub_metering_1, Sub_metering_2, Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) + 
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2010.02
df.2007.02 %>% select(Date,DateTime,Sub_metering_1, Sub_metering_2, Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) + 
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

df.2008.02 %>% select(Date,DateTime,Sub_metering_1, Sub_metering_2, Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) + 
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

df.2009.02 %>% select(Date,DateTime,Sub_metering_1, Sub_metering_2, Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) + 
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

df.2010.02 %>% select(Date,DateTime,Sub_metering_1, Sub_metering_2, Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) + 
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2010.03
df.2010.03 %>% select(Date,DateTime,Sub_metering_1, Sub_metering_2, Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) + 
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2010.04
df.2010.04 %>% select(Date,DateTime,Sub_metering_1, Sub_metering_2, Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) + 
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2010.05
df.2010.05 %>% select(Date,DateTime,Sub_metering_1, Sub_metering_2, Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) + 
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2010.06
df.2010.06 %>% select(Date,DateTime,Sub_metering_1, Sub_metering_2, Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) + 
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2010.07
df.2010.07 %>% select(Date,DateTime,Sub_metering_1, Sub_metering_2, Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) + 
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2010.08
df.2010.08 %>% select(Date,DateTime,Sub_metering_1, Sub_metering_2, Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) + 
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2010.09
df.2010.09 %>% select(Date,DateTime,Sub_metering_1, Sub_metering_2, Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) + 
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2010.10
df.2010.10 %>% select(Date,DateTime,Sub_metering_1, Sub_metering_2, Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) + 
  geom_line(aes(y=Sub_metering_3,col="Climatization"))

#2010.11
df.2010.11 %>% select(Date,DateTime,Sub_metering_1, Sub_metering_2, Sub_metering_3) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) + 
  geom_line(aes(y=Sub_metering_3,col="Climatization"))


## GAP vs. S1
##2007
df.2007 %>% select(Date,DateTime,Sub_metering_1,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.01
df.2007.01 %>% select(Date,DateTime,Sub_metering_1,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.02
df.2007.02 %>% select(Date,DateTime,Sub_metering_1,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.03
df.2007.03 %>% select(Date,DateTime,Sub_metering_1,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.04
df.2007.04 %>% select(Date,DateTime,Sub_metering_1,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.05
df.2007.05 %>% select(Date,DateTime,Sub_metering_1,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.06
df.2007.06 %>% select(Date,DateTime,Sub_metering_1,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.07
df.2007.07 %>% select(Date,DateTime,Sub_metering_1,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.08
df.2007.08 %>% select(Date,DateTime,Sub_metering_1,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.09
df.2007.09 %>% select(Date,DateTime,Sub_metering_1,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.10
df.2007.10 %>% select(Date,DateTime,Sub_metering_1,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.11
df.2007.11 %>% select(Date,DateTime,Sub_metering_1,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.12
df.2007.12 %>% select(Date,DateTime,Sub_metering_1,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.1q

#2007.2q

#2007.3q

#2007.4q

#2008
df.2008 %>% select(Date,DateTime,Sub_metering_1,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2008.01
df.2008.01 %>% select(Date,DateTime,Sub_metering_1,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))


#2008.02
df.2008.02 %>% select(Date,DateTime,Sub_metering_1,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2008.03
df.2008.03 %>% select(Date,DateTime,Sub_metering_1,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2008.04
df.2008.04 %>% select(Date,DateTime,Sub_metering_1,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2008.05
df.2008.05 %>% select(Date,DateTime,Sub_metering_1,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2008.06
df.2008.06 %>% select(Date,DateTime,Sub_metering_1,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2008.07
df.2008.07 %>% select(Date,DateTime,Sub_metering_1,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2008.08
df.2008.08 %>% select(Date,DateTime,Sub_metering_1,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2008.09
df.2008.09 %>% select(Date,DateTime,Sub_metering_1,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2008.10
df.2008.10 %>% select(Date,DateTime,Sub_metering_1,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2008.11
df.2008.11 %>% select(Date,DateTime,Sub_metering_1,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2008.12
df.2008.12 %>% select(Date,DateTime,Sub_metering_1,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009
df.2009 %>% select(Date,DateTime,Sub_metering_1,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009.01
df.2008.01 %>% select(Date,DateTime,Sub_metering_1,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

df.2009.01 %>% select(Date,DateTime,Sub_metering_1,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009.02
df.2009.02 %>% select(Date,DateTime,Sub_metering_1,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009.03
df.2009.03 %>% select(Date,DateTime,Sub_metering_1,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009.04
df.2009.04 %>% select(Date,DateTime,Sub_metering_1,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009.05
df.2009.05 %>% select(Date,DateTime,Sub_metering_1,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009.06
df.2009.06 %>% select(Date,DateTime,Sub_metering_1,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009.07
df.2009.07 %>% select(Date,DateTime,Sub_metering_1,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009.08
df.2009.08 %>% select(Date,DateTime,Sub_metering_1,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009.09
df.2009.09 %>% select(Date,DateTime,Sub_metering_1,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009.10
df.2009.10 %>% select(Date,DateTime,Sub_metering_1,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009.11
df.2009.11 %>% select(Date,DateTime,Sub_metering_1,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009.12
df.2009.12 %>% select(Date,DateTime,Sub_metering_1,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2010
df.2010 %>% select(Date,DateTime,Sub_metering_1,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2010.01
df.2007.01 %>% select(Date,DateTime,Sub_metering_1,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

df.2008.01 %>% select(Date,DateTime,Sub_metering_1,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

df.2009.01 %>% select(Date,DateTime,Sub_metering_1,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

df.2010.01 %>% select(Date,DateTime,Sub_metering_1,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2010.02
df.2007.02 %>% select(Date,DateTime,Sub_metering_1,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

df.2008.02 %>% select(Date,DateTime,Sub_metering_1,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

df.2009.02 %>% select(Date,DateTime,Sub_metering_1,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

df.2010.02 %>% select(Date,DateTime,Sub_metering_1,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2010.03
df.2010.03 %>% select(Date,DateTime,Sub_metering_1,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2010.04
df.2010.04 %>% select(Date,DateTime,Sub_metering_1,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2010.05
df.2010.05 %>% select(Date,DateTime,Sub_metering_1,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2010.06
df.2010.06 %>% select(Date,DateTime,Sub_metering_1,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2010.07
df.2010.07 %>% select(Date,DateTime,Sub_metering_1,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2010.08
df.2010.08 %>% select(Date,DateTime,Sub_metering_1,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2010.09
df.2010.09 %>% select(Date,DateTime,Sub_metering_1,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2010.10
df.2010.10 %>% select(Date,DateTime,Sub_metering_1,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2010.11
df.2010.11 %>% select(Date,DateTime,Sub_metering_1,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))


## GAP vs. S2
##2007
df.2007 %>% select(Date,DateTime,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.01
df.2007.01 %>% select(Date,DateTime,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.02
df.2007.02 %>% select(Date,DateTime,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.03
df.2007.03 %>% select(Date,DateTime,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.04
df.2007.04 %>% select(Date,DateTime,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.05
df.2007.05 %>% select(Date,DateTime,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.06
df.2007.06 %>% select(Date,DateTime,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.07
df.2007.07 %>% select(Date,DateTime,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.08
df.2007.08 %>% select(Date,DateTime,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.09
df.2007.09 %>% select(Date,DateTime,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.10
df.2007.10 %>% select(Date,DateTime,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.11
df.2007.11 %>% select(Date,DateTime,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.12
df.2007.12 %>% select(Date,DateTime,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.1q

#2007.2q

#2007.3q

#2007.4q

#2008
df.2008 %>% select(Date,DateTime,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2008.01
df.2008.01 %>% select(Date,DateTime,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))


#2008.02
df.2008.02 %>% select(Date,DateTime,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2008.03
df.2008.03 %>% select(Date,DateTime,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2008.04
df.2008.04 %>% select(Date,DateTime,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2008.05
df.2008.05 %>% select(Date,DateTime,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2008.06
df.2008.06 %>% select(Date,DateTime,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2008.07
df.2008.07 %>% select(Date,DateTime,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2008.08
df.2008.08 %>% select(Date,DateTime,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2008.09
df.2008.09 %>% select(Date,DateTime,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2008.10
df.2008.10 %>% select(Date,DateTime,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2008.11
df.2008.11 %>% select(Date,DateTime,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2008.12
df.2008.12 %>% select(Date,DateTime,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009
df.2009 %>% select(Date,DateTime,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009.01
df.2008.01 %>% select(Date,DateTime,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

df.2009.01 %>% select(Date,DateTime,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009.02
df.2009.02 %>% select(Date,DateTime,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009.03
df.2009.03 %>% select(Date,DateTime,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009.04
df.2009.04 %>% select(Date,DateTime,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009.05
df.2009.05 %>% select(Date,DateTime,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009.06
df.2009.06 %>% select(Date,DateTime,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009.07
df.2009.07 %>% select(Date,DateTime,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009.08
df.2009.08 %>% select(Date,DateTime,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009.09
df.2009.09 %>% select(Date,DateTime,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009.10
df.2009.10 %>% select(Date,DateTime,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009.11
df.2009.11 %>% select(Date,DateTime,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009.12
df.2009.12 %>% select(Date,DateTime,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2010
df.2010 %>% select(Date,DateTime,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2010.01
df.2007.01 %>% select(Date,DateTime,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

df.2008.01 %>% select(Date,DateTime,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

df.2009.01 %>% select(Date,DateTime,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

df.2010.01 %>% select(Date,DateTime,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2010.02
df.2007.02 %>% select(Date,DateTime,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

df.2008.02 %>% select(Date,DateTime,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

df.2009.02 %>% select(Date,DateTime,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

df.2010.02 %>% select(Date,DateTime,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2010.03
df.2010.03 %>% select(Date,DateTime,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2010.04
df.2010.04 %>% select(Date,DateTime,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2010.05
df.2010.05 %>% select(Date,DateTime,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2010.06
df.2010.06 %>% select(Date,DateTime,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2010.07
df.2010.07 %>% select(Date,DateTime,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2010.08
df.2010.08 %>% select(Date,DateTime,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2010.09
df.2010.09 %>% select(Date,DateTime,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2010.10
df.2010.10 %>% select(Date,DateTime,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2010.11
df.2010.11 %>% select(Date,DateTime,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))


## GAP vs. S3
##2007
df.2007 %>% select(Date,DateTime,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.01
df.2007.01 %>% select(Date,DateTime,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.02
df.2007.02 %>% select(Date,DateTime,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.03
df.2007.03 %>% select(Date,DateTime,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.04
df.2007.04 %>% select(Date,DateTime,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.05
df.2007.05 %>% select(Date,DateTime,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.06
df.2007.06 %>% select(Date,DateTime,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.07
df.2007.07 %>% select(Date,DateTime,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.08
df.2007.08 %>% select(Date,DateTime,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.09
df.2007.09 %>% select(Date,DateTime,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.10
df.2007.10 %>% select(Date,DateTime,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.11
df.2007.11 %>% select(Date,DateTime,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.12
df.2007.12 %>% select(Date,DateTime,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.1q

#2007.2q

#2007.3q

#2007.4q

#2008
df.2008 %>% select(Date,DateTime,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2008.01
df.2008.01 %>% select(Date,DateTime,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))


#2008.02
df.2008.02 %>% select(Date,DateTime,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2008.03
df.2008.03 %>% select(Date,DateTime,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2008.04
df.2008.04 %>% select(Date,DateTime,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2008.05
df.2008.05 %>% select(Date,DateTime,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2008.06
df.2008.06 %>% select(Date,DateTime,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2008.07
df.2008.07 %>% select(Date,DateTime,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2008.08
df.2008.08 %>% select(Date,DateTime,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2008.09
df.2008.09 %>% select(Date,DateTime,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2008.10
df.2008.10 %>% select(Date,DateTime,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2008.11
df.2008.11 %>% select(Date,DateTime,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2008.12
df.2008.12 %>% select(Date,DateTime,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009
df.2009 %>% select(Date,DateTime,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009.01
df.2008.01 %>% select(Date,DateTime,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

df.2009.01 %>% select(Date,DateTime,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009.02
df.2009.02 %>% select(Date,DateTime,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009.03
df.2009.03 %>% select(Date,DateTime,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009.04
df.2009.04 %>% select(Date,DateTime,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009.05
df.2009.05 %>% select(Date,DateTime,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009.06
df.2009.06 %>% select(Date,DateTime,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009.07
df.2009.07 %>% select(Date,DateTime,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009.08
df.2009.08 %>% select(Date,DateTime,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009.09
df.2009.09 %>% select(Date,DateTime,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009.10
df.2009.10 %>% select(Date,DateTime,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009.11
df.2009.11 %>% select(Date,DateTime,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009.12
df.2009.12 %>% select(Date,DateTime,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2010
df.2010 %>% select(Date,DateTime,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2010.01
df.2007.01 %>% select(Date,DateTime,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

df.2008.01 %>% select(Date,DateTime,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

df.2009.01 %>% select(Date,DateTime,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

df.2010.01 %>% select(Date,DateTime,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2010.02
df.2007.02 %>% select(Date,DateTime,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

df.2008.02 %>% select(Date,DateTime,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

df.2009.02 %>% select(Date,DateTime,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

df.2010.02 %>% select(Date,DateTime,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2010.03
df.2010.03 %>% select(Date,DateTime,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2010.04
df.2010.04 %>% select(Date,DateTime,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2010.05
df.2010.05 %>% select(Date,DateTime,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2010.06
df.2010.06 %>% select(Date,DateTime,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2010.07
df.2010.07 %>% select(Date,DateTime,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2010.08
df.2010.08 %>% select(Date,DateTime,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2010.09
df.2010.09 %>% select(Date,DateTime,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2010.10
df.2010.10 %>% select(Date,DateTime,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2010.11
df.2010.11 %>% select(Date,DateTime,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

## GAP vs. S1-S2
##2007
df.2007 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.01
df.2007.01 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.02
df.2007.02 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.03
df.2007.03 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.04
df.2007.04 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.05
df.2007.05 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.06
df.2007.06 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.07
df.2007.07 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.08
df.2007.08 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.09
df.2007.09 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.10
df.2007.10 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.11
df.2007.11 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.12
df.2007.12 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.1q

#2007.2q

#2007.3q

#2007.4q

#2008
df.2008 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2008.01
df.2008.01 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))


#2008.02
df.2008.02 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2008.03
df.2008.03 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2008.04
df.2008.04 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2008.05
df.2008.05 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2008.06
df.2008.06 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2008.07
df.2008.07 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2008.08
df.2008.08 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2008.09
df.2008.09 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2008.10
df.2008.10 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2008.11
df.2008.11 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2008.12
df.2008.12 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009
df.2009 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009.01
df.2008.01 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

df.2009.01 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009.02
df.2009.02 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009.03
df.2009.03 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009.04
df.2009.04 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009.05
df.2009.05 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009.06
df.2009.06 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009.07
df.2009.07 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009.08
df.2009.08 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009.09
df.2009.09 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009.10
df.2009.10 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009.11
df.2009.11 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009.12
df.2009.12 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2010
df.2010 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2010.01
df.2007.01 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

df.2008.01 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

df.2009.01 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

df.2010.01 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2010.02
df.2007.02 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

df.2008.02 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

df.2009.02 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

df.2010.02 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2010.03
df.2010.03 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2010.04
df.2010.04 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2010.05
df.2010.05 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2010.06
df.2010.06 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2010.07
df.2010.07 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2010.08
df.2010.08 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2010.09
df.2010.09 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2010.10
df.2010.10 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2010.11
df.2010.11 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

## GAP vs. S1-S3
##2007
df.2007 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.01
df.2007.01 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.02
df.2007.02 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.03
df.2007.03 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.04
df.2007.04 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.05
df.2007.05 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.06
df.2007.06 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.07
df.2007.07 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.08
df.2007.08 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.09
df.2007.09 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.10
df.2007.10 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.11
df.2007.11 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.12
df.2007.12 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.1q

#2007.2q

#2007.3q

#2007.4q

#2008
df.2008 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2008.01
df.2008.01 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))


#2008.02
df.2008.02 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2008.03
df.2008.03 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2008.04
df.2008.04 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2008.05
df.2008.05 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2008.06
df.2008.06 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2008.07
df.2008.07 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2008.08
df.2008.08 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2008.09
df.2008.09 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2008.10
df.2008.10 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2008.11
df.2008.11 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2008.12
df.2008.12 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009
df.2009 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009.01
df.2008.01 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

df.2009.01 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009.02
df.2009.02 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009.03
df.2009.03 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009.04
df.2009.04 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009.05
df.2009.05 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009.06
df.2009.06 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009.07
df.2009.07 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009.08
df.2009.08 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009.09
df.2009.09 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009.10
df.2009.10 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009.11
df.2009.11 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009.12
df.2009.12 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2010
df.2010 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2010.01
df.2007.01 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

df.2008.01 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

df.2009.01 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

df.2010.01 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2010.02
df.2007.02 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

df.2008.02 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

df.2009.02 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

df.2010.02 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2010.03
df.2010.03 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2010.04
df.2010.04 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2010.05
df.2010.05 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2010.06
df.2010.06 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2010.07
df.2010.07 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2010.08
df.2010.08 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2010.09
df.2010.09 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2010.10
df.2010.10 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2010.11
df.2010.11 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

## GAP vs. S2-S3
##2007
df.2007 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.01
df.2007.01 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.02
df.2007.02 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.03
df.2007.03 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.04
df.2007.04 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.05
df.2007.05 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.06
df.2007.06 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.07
df.2007.07 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.08
df.2007.08 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.09
df.2007.09 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.10
df.2007.10 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.11
df.2007.11 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.12
df.2007.12 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.1q

#2007.2q

#2007.3q

#2007.4q

#2008
df.2008 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2008.01
df.2008.01 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))


#2008.02
df.2008.02 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2008.03
df.2008.03 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2008.04
df.2008.04 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2008.05
df.2008.05 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2008.06
df.2008.06 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2008.07
df.2008.07 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2008.08
df.2008.08 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2008.09
df.2008.09 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2008.10
df.2008.10 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2008.11
df.2008.11 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2008.12
df.2008.12 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009
df.2009 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009.01
df.2008.01 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

df.2009.01 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009.02
df.2009.02 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009.03
df.2009.03 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009.04
df.2009.04 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009.05
df.2009.05 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009.06
df.2009.06 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009.07
df.2009.07 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009.08
df.2009.08 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009.09
df.2009.09 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009.10
df.2009.10 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009.11
df.2009.11 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009.12
df.2009.12 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2010
df.2010 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2010.01
df.2007.01 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

df.2008.01 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

df.2009.01 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

df.2010.01 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2010.02
df.2007.02 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

df.2008.02 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

df.2009.02 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

df.2010.02 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2010.03
df.2010.03 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2010.04
df.2010.04 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2010.05
df.2010.05 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2010.06
df.2010.06 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2010.07
df.2010.07 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2010.08
df.2010.08 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2010.09
df.2010.09 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2010.10
df.2010.10 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2010.11
df.2010.11 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

## GAP vs. S1-S2-S3
##2007
df.2007 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.01
df.2007.01 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.02
df.2007.02 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.03
df.2007.03 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.04
df.2007.04 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.05
df.2007.05 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.06
df.2007.06 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.07
df.2007.07 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.08
df.2007.08 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.09
df.2007.09 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.10
df.2007.10 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.11
df.2007.11 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.12
df.2007.12 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2007.1q

#2007.2q

#2007.3q

#2007.4q

#2008
df.2008 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2008.01
df.2008.01 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))


#2008.02
df.2008.02 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2008.03
df.2008.03 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2008.04
df.2008.04 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2008.05
df.2008.05 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2008.06
df.2008.06 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2008.07
df.2008.07 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2008.08
df.2008.08 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2008.09
df.2008.09 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2008.10
df.2008.10 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2008.11
df.2008.11 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2008.12
df.2008.12 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009
df.2009 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009.01
df.2008.01 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

df.2009.01 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009.02
df.2009.02 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009.03
df.2009.03 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009.04
df.2009.04 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009.05
df.2009.05 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009.06
df.2009.06 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009.07
df.2009.07 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009.08
df.2009.08 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009.09
df.2009.09 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009.10
df.2009.10 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009.11
df.2009.11 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2009.12
df.2009.12 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2010
df.2010 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2010.01
df.2007.01 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

df.2008.01 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

df.2009.01 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

df.2010.01 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2010.02
df.2007.02 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

df.2008.02 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

df.2009.02 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

df.2010.02 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2010.03
df.2010.03 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2010.04
df.2010.04 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2010.05
df.2010.05 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2010.06
df.2010.06 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2010.07
df.2010.07 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2010.08
df.2010.08 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2010.09
df.2010.09 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2010.10
df.2010.10 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

#2010.11
df.2010.11 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_active_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_active_power,col="Global Active Power"))

## GRP vs. S1
##2007
df.2007 %>% select(Date,DateTime,Sub_metering_1,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.01
df.2007.01 %>% select(Date,DateTime,Sub_metering_1,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.02
df.2007.02 %>% select(Date,DateTime,Sub_metering_1,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.03
df.2007.03 %>% select(Date,DateTime,Sub_metering_1,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.04
df.2007.04 %>% select(Date,DateTime,Sub_metering_1,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.05
df.2007.05 %>% select(Date,DateTime,Sub_metering_1,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.06
df.2007.06 %>% select(Date,DateTime,Sub_metering_1,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.07
df.2007.07 %>% select(Date,DateTime,Sub_metering_1,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.08
df.2007.08 %>% select(Date,DateTime,Sub_metering_1,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.09
df.2007.09 %>% select(Date,DateTime,Sub_metering_1,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.10
df.2007.10 %>% select(Date,DateTime,Sub_metering_1,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.11
df.2007.11 %>% select(Date,DateTime,Sub_metering_1,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.12
df.2007.12 %>% select(Date,DateTime,Sub_metering_1,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.1q

#2007.2q

#2007.3q

#2007.4q

#2008
df.2008 %>% select(Date,DateTime,Sub_metering_1,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2008.01
df.2008.01 %>% select(Date,DateTime,Sub_metering_1,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))


#2008.02
df.2008.02 %>% select(Date,DateTime,Sub_metering_1,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2008.03
df.2008.03 %>% select(Date,DateTime,Sub_metering_1,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2008.04
df.2008.04 %>% select(Date,DateTime,Sub_metering_1,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2008.05
df.2008.05 %>% select(Date,DateTime,Sub_metering_1,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2008.06
df.2008.06 %>% select(Date,DateTime,Sub_metering_1,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2008.07
df.2008.07 %>% select(Date,DateTime,Sub_metering_1,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2008.08
df.2008.08 %>% select(Date,DateTime,Sub_metering_1,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2008.09
df.2008.09 %>% select(Date,DateTime,Sub_metering_1,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2008.10
df.2008.10 %>% select(Date,DateTime,Sub_metering_1,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2008.11
df.2008.11 %>% select(Date,DateTime,Sub_metering_1,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2008.12
df.2008.12 %>% select(Date,DateTime,Sub_metering_1,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009
df.2009 %>% select(Date,DateTime,Sub_metering_1,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009.01
df.2008.01 %>% select(Date,DateTime,Sub_metering_1,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

df.2009.01 %>% select(Date,DateTime,Sub_metering_1,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009.02
df.2009.02 %>% select(Date,DateTime,Sub_metering_1,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009.03
df.2009.03 %>% select(Date,DateTime,Sub_metering_1,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009.04
df.2009.04 %>% select(Date,DateTime,Sub_metering_1,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009.05
df.2009.05 %>% select(Date,DateTime,Sub_metering_1,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009.06
df.2009.06 %>% select(Date,DateTime,Sub_metering_1,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009.07
df.2009.07 %>% select(Date,DateTime,Sub_metering_1,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009.08
df.2009.08 %>% select(Date,DateTime,Sub_metering_1,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009.09
df.2009.09 %>% select(Date,DateTime,Sub_metering_1,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009.10
df.2009.10 %>% select(Date,DateTime,Sub_metering_1,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009.11
df.2009.11 %>% select(Date,DateTime,Sub_metering_1,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009.12
df.2009.12 %>% select(Date,DateTime,Sub_metering_1,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2010
df.2010 %>% select(Date,DateTime,Sub_metering_1,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2010.01
df.2007.01 %>% select(Date,DateTime,Sub_metering_1,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

df.2008.01 %>% select(Date,DateTime,Sub_metering_1,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

df.2009.01 %>% select(Date,DateTime,Sub_metering_1,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

df.2010.01 %>% select(Date,DateTime,Sub_metering_1,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2010.02
df.2007.02 %>% select(Date,DateTime,Sub_metering_1,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

df.2008.02 %>% select(Date,DateTime,Sub_metering_1,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

df.2009.02 %>% select(Date,DateTime,Sub_metering_1,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

df.2010.02 %>% select(Date,DateTime,Sub_metering_1,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2010.03
df.2010.03 %>% select(Date,DateTime,Sub_metering_1,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2010.04
df.2010.04 %>% select(Date,DateTime,Sub_metering_1,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2010.05
df.2010.05 %>% select(Date,DateTime,Sub_metering_1,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2010.06
df.2010.06 %>% select(Date,DateTime,Sub_metering_1,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2010.07
df.2010.07 %>% select(Date,DateTime,Sub_metering_1,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2010.08
df.2010.08 %>% select(Date,DateTime,Sub_metering_1,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2010.09
df.2010.09 %>% select(Date,DateTime,Sub_metering_1,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2010.10
df.2010.10 %>% select(Date,DateTime,Sub_metering_1,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2010.11
df.2010.11 %>% select(Date,DateTime,Sub_metering_1,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))


## GRP vs. S2
##2007
df.2007 %>% select(Date,DateTime,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.01
df.2007.01 %>% select(Date,DateTime,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.02
df.2007.02 %>% select(Date,DateTime,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.03
df.2007.03 %>% select(Date,DateTime,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.04
df.2007.04 %>% select(Date,DateTime,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.05
df.2007.05 %>% select(Date,DateTime,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.06
df.2007.06 %>% select(Date,DateTime,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.07
df.2007.07 %>% select(Date,DateTime,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.08
df.2007.08 %>% select(Date,DateTime,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.09
df.2007.09 %>% select(Date,DateTime,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.10
df.2007.10 %>% select(Date,DateTime,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.11
df.2007.11 %>% select(Date,DateTime,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.12
df.2007.12 %>% select(Date,DateTime,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.1q

#2007.2q

#2007.3q

#2007.4q

#2008
df.2008 %>% select(Date,DateTime,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2008.01
df.2008.01 %>% select(Date,DateTime,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))


#2008.02
df.2008.02 %>% select(Date,DateTime,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2008.03
df.2008.03 %>% select(Date,DateTime,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2008.04
df.2008.04 %>% select(Date,DateTime,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2008.05
df.2008.05 %>% select(Date,DateTime,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2008.06
df.2008.06 %>% select(Date,DateTime,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2008.07
df.2008.07 %>% select(Date,DateTime,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2008.08
df.2008.08 %>% select(Date,DateTime,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2008.09
df.2008.09 %>% select(Date,DateTime,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2008.10
df.2008.10 %>% select(Date,DateTime,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2008.11
df.2008.11 %>% select(Date,DateTime,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2008.12
df.2008.12 %>% select(Date,DateTime,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009
df.2009 %>% select(Date,DateTime,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009.01
df.2008.01 %>% select(Date,DateTime,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

df.2009.01 %>% select(Date,DateTime,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009.02
df.2009.02 %>% select(Date,DateTime,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009.03
df.2009.03 %>% select(Date,DateTime,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009.04
df.2009.04 %>% select(Date,DateTime,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009.05
df.2009.05 %>% select(Date,DateTime,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009.06
df.2009.06 %>% select(Date,DateTime,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009.07
df.2009.07 %>% select(Date,DateTime,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009.08
df.2009.08 %>% select(Date,DateTime,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009.09
df.2009.09 %>% select(Date,DateTime,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009.10
df.2009.10 %>% select(Date,DateTime,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009.11
df.2009.11 %>% select(Date,DateTime,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009.12
df.2009.12 %>% select(Date,DateTime,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2010
df.2010 %>% select(Date,DateTime,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2010.01
df.2007.01 %>% select(Date,DateTime,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

df.2008.01 %>% select(Date,DateTime,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

df.2009.01 %>% select(Date,DateTime,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

df.2010.01 %>% select(Date,DateTime,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2010.02
df.2007.02 %>% select(Date,DateTime,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

df.2008.02 %>% select(Date,DateTime,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

df.2009.02 %>% select(Date,DateTime,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

df.2010.02 %>% select(Date,DateTime,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2010.03
df.2010.03 %>% select(Date,DateTime,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2010.04
df.2010.04 %>% select(Date,DateTime,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2010.05
df.2010.05 %>% select(Date,DateTime,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2010.06
df.2010.06 %>% select(Date,DateTime,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2010.07
df.2010.07 %>% select(Date,DateTime,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2010.08
df.2010.08 %>% select(Date,DateTime,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2010.09
df.2010.09 %>% select(Date,DateTime,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2010.10
df.2010.10 %>% select(Date,DateTime,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2010.11
df.2010.11 %>% select(Date,DateTime,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))


## GRP vs. S3
##2007
df.2007 %>% select(Date,DateTime,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.01
df.2007.01 %>% select(Date,DateTime,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.02
df.2007.02 %>% select(Date,DateTime,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.03
df.2007.03 %>% select(Date,DateTime,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.04
df.2007.04 %>% select(Date,DateTime,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.05
df.2007.05 %>% select(Date,DateTime,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.06
df.2007.06 %>% select(Date,DateTime,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.07
df.2007.07 %>% select(Date,DateTime,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.08
df.2007.08 %>% select(Date,DateTime,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.09
df.2007.09 %>% select(Date,DateTime,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.10
df.2007.10 %>% select(Date,DateTime,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.11
df.2007.11 %>% select(Date,DateTime,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.12
df.2007.12 %>% select(Date,DateTime,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.1q

#2007.2q

#2007.3q

#2007.4q

#2008
df.2008 %>% select(Date,DateTime,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2008.01
df.2008.01 %>% select(Date,DateTime,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))


#2008.02
df.2008.02 %>% select(Date,DateTime,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2008.03
df.2008.03 %>% select(Date,DateTime,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2008.04
df.2008.04 %>% select(Date,DateTime,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2008.05
df.2008.05 %>% select(Date,DateTime,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2008.06
df.2008.06 %>% select(Date,DateTime,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2008.07
df.2008.07 %>% select(Date,DateTime,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2008.08
df.2008.08 %>% select(Date,DateTime,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2008.09
df.2008.09 %>% select(Date,DateTime,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2008.10
df.2008.10 %>% select(Date,DateTime,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2008.11
df.2008.11 %>% select(Date,DateTime,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2008.12
df.2008.12 %>% select(Date,DateTime,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009
df.2009 %>% select(Date,DateTime,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009.01
df.2008.01 %>% select(Date,DateTime,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

df.2009.01 %>% select(Date,DateTime,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009.02
df.2009.02 %>% select(Date,DateTime,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009.03
df.2009.03 %>% select(Date,DateTime,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009.04
df.2009.04 %>% select(Date,DateTime,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009.05
df.2009.05 %>% select(Date,DateTime,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009.06
df.2009.06 %>% select(Date,DateTime,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009.07
df.2009.07 %>% select(Date,DateTime,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009.08
df.2009.08 %>% select(Date,DateTime,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009.09
df.2009.09 %>% select(Date,DateTime,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009.10
df.2009.10 %>% select(Date,DateTime,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009.11
df.2009.11 %>% select(Date,DateTime,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009.12
df.2009.12 %>% select(Date,DateTime,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2010
df.2010 %>% select(Date,DateTime,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2010.01
df.2007.01 %>% select(Date,DateTime,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

df.2008.01 %>% select(Date,DateTime,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

df.2009.01 %>% select(Date,DateTime,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

df.2010.01 %>% select(Date,DateTime,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2010.02
df.2007.02 %>% select(Date,DateTime,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

df.2008.02 %>% select(Date,DateTime,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

df.2009.02 %>% select(Date,DateTime,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

df.2010.02 %>% select(Date,DateTime,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2010.03
df.2010.03 %>% select(Date,DateTime,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2010.04
df.2010.04 %>% select(Date,DateTime,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2010.05
df.2010.05 %>% select(Date,DateTime,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2010.06
df.2010.06 %>% select(Date,DateTime,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2010.07
df.2010.07 %>% select(Date,DateTime,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2010.08
df.2010.08 %>% select(Date,DateTime,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2010.09
df.2010.09 %>% select(Date,DateTime,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2010.10
df.2010.10 %>% select(Date,DateTime,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2010.11
df.2010.11 %>% select(Date,DateTime,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

## GRP vs. S1-S2
##2007
df.2007 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.01
df.2007.01 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.02
df.2007.02 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.03
df.2007.03 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.04
df.2007.04 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.05
df.2007.05 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.06
df.2007.06 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.07
df.2007.07 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.08
df.2007.08 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.09
df.2007.09 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.10
df.2007.10 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.11
df.2007.11 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.12
df.2007.12 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.1q

#2007.2q

#2007.3q

#2007.4q

#2008
df.2008 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2008.01
df.2008.01 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))


#2008.02
df.2008.02 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2008.03
df.2008.03 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2008.04
df.2008.04 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2008.05
df.2008.05 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2008.06
df.2008.06 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2008.07
df.2008.07 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2008.08
df.2008.08 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2008.09
df.2008.09 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2008.10
df.2008.10 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2008.11
df.2008.11 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2008.12
df.2008.12 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009
df.2009 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009.01
df.2008.01 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

df.2009.01 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009.02
df.2009.02 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009.03
df.2009.03 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009.04
df.2009.04 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009.05
df.2009.05 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009.06
df.2009.06 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009.07
df.2009.07 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009.08
df.2009.08 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009.09
df.2009.09 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009.10
df.2009.10 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009.11
df.2009.11 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009.12
df.2009.12 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2010
df.2010 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2010.01
df.2007.01 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

df.2008.01 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

df.2009.01 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

df.2010.01 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2010.02
df.2007.02 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

df.2008.02 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

df.2009.02 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

df.2010.02 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2010.03
df.2010.03 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2010.04
df.2010.04 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2010.05
df.2010.05 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2010.06
df.2010.06 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2010.07
df.2010.07 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2010.08
df.2010.08 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2010.09
df.2010.09 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2010.10
df.2010.10 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2010.11
df.2010.11 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

## GRP vs. S1-S3
##2007
df.2007 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.01
df.2007.01 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.02
df.2007.02 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.03
df.2007.03 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.04
df.2007.04 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.05
df.2007.05 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.06
df.2007.06 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.07
df.2007.07 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.08
df.2007.08 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.09
df.2007.09 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.10
df.2007.10 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.11
df.2007.11 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.12
df.2007.12 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.1q

#2007.2q

#2007.3q

#2007.4q

#2008
df.2008 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2008.01
df.2008.01 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))


#2008.02
df.2008.02 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2008.03
df.2008.03 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2008.04
df.2008.04 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2008.05
df.2008.05 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2008.06
df.2008.06 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2008.07
df.2008.07 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2008.08
df.2008.08 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2008.09
df.2008.09 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2008.10
df.2008.10 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2008.11
df.2008.11 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2008.12
df.2008.12 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009
df.2009 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009.01
df.2008.01 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

df.2009.01 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009.02
df.2009.02 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009.03
df.2009.03 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009.04
df.2009.04 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009.05
df.2009.05 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009.06
df.2009.06 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009.07
df.2009.07 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009.08
df.2009.08 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009.09
df.2009.09 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009.10
df.2009.10 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009.11
df.2009.11 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009.12
df.2009.12 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2010
df.2010 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2010.01
df.2007.01 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

df.2008.01 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

df.2009.01 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

df.2010.01 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2010.02
df.2007.02 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

df.2008.02 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

df.2009.02 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

df.2010.02 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2010.03
df.2010.03 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2010.04
df.2010.04 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2010.05
df.2010.05 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2010.06
df.2010.06 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2010.07
df.2010.07 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2010.08
df.2010.08 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2010.09
df.2010.09 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2010.10
df.2010.10 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2010.11
df.2010.11 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

## GRP vs. S2-S3
##2007
df.2007 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.01
df.2007.01 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.02
df.2007.02 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.03
df.2007.03 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.04
df.2007.04 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.05
df.2007.05 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.06
df.2007.06 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.07
df.2007.07 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.08
df.2007.08 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.09
df.2007.09 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.10
df.2007.10 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.11
df.2007.11 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.12
df.2007.12 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.1q

#2007.2q

#2007.3q

#2007.4q

#2008
df.2008 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2008.01
df.2008.01 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))


#2008.02
df.2008.02 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2008.03
df.2008.03 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2008.04
df.2008.04 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2008.05
df.2008.05 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2008.06
df.2008.06 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2008.07
df.2008.07 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2008.08
df.2008.08 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2008.09
df.2008.09 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2008.10
df.2008.10 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2008.11
df.2008.11 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2008.12
df.2008.12 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009
df.2009 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009.01
df.2008.01 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

df.2009.01 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009.02
df.2009.02 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009.03
df.2009.03 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009.04
df.2009.04 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009.05
df.2009.05 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009.06
df.2009.06 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009.07
df.2009.07 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009.08
df.2009.08 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009.09
df.2009.09 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009.10
df.2009.10 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009.11
df.2009.11 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009.12
df.2009.12 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2010
df.2010 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2010.01
df.2007.01 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

df.2008.01 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

df.2009.01 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

df.2010.01 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2010.02
df.2007.02 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

df.2008.02 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

df.2009.02 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

df.2010.02 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2010.03
df.2010.03 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2010.04
df.2010.04 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2010.05
df.2010.05 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2010.06
df.2010.06 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2010.07
df.2010.07 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2010.08
df.2010.08 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2010.09
df.2010.09 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2010.10
df.2010.10 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2010.11
df.2010.11 %>% select(Date,DateTime,Sub_metering_3,Sub_metering_2,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

## GRP vs. S1-S2-S3
##2007
df.2007 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.01
df.2007.01 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.02
df.2007.02 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.03
df.2007.03 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.04
df.2007.04 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.05
df.2007.05 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.06
df.2007.06 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.07
df.2007.07 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.08
df.2007.08 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.09
df.2007.09 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.10
df.2007.10 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.11
df.2007.11 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.12
df.2007.12 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2007.1q

#2007.2q

#2007.3q

#2007.4q

#2008
df.2008 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2008.01
df.2008.01 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))


#2008.02
df.2008.02 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2008.03
df.2008.03 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2008.04
df.2008.04 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2008.05
df.2008.05 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2008.06
df.2008.06 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2008.07
df.2008.07 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2008.08
df.2008.08 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2008.09
df.2008.09 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2008.10
df.2008.10 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2008.11
df.2008.11 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2008.12
df.2008.12 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009
df.2009 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009.01
df.2008.01 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

df.2009.01 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009.02
df.2009.02 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009.03
df.2009.03 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009.04
df.2009.04 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009.05
df.2009.05 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009.06
df.2009.06 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009.07
df.2009.07 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009.08
df.2009.08 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009.09
df.2009.09 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009.10
df.2009.10 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009.11
df.2009.11 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2009.12
df.2009.12 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2010
df.2010 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2010.01
df.2007.01 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

df.2008.01 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

df.2009.01 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

df.2010.01 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2010.02
df.2007.02 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

df.2008.02 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

df.2009.02 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

df.2010.02 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2010.03
df.2010.03 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2010.04
df.2010.04 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2010.05
df.2010.05 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2010.06
df.2010.06 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2010.07
df.2010.07 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2010.08
df.2010.08 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2010.09
df.2010.09 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2010.10
df.2010.10 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

#2010.11
df.2010.11 %>% select(Date,DateTime,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_reactive_power) %>% 
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Sub_metering_1,col="Kitchen")) +
  geom_line(aes(y=Sub_metering_2,col="Laundry")) +
  geom_line(aes(y=Sub_metering_3,col="Climatization")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power"))

## GAP vs. GRP vs. Voltage vs. GI
## 2007
df.2007 %>% select(Date,DateTime,Global_active_power,Global_reactive_power,Global_intensity,Voltage) %>%
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Global_active_power,col="Global Active Power")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power")) +
  geom_line(aes(y=Global_intensity,col="Global Intensity")) +
  geom_line(aes(y=Voltage,col="Voltage"))

#2007.01
df.2007.01 %>% select(Date,DateTime,Global_active_power,Global_reactive_power,Global_intensity,Voltage) %>%
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Global_active_power,col="Global Active Power")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power")) +
  geom_line(aes(y=Global_intensity,col="Global Intensity")) +
  geom_line(aes(y=Voltage,col="Voltage"))

#2007.02
df.2007.02 %>% select(Date,DateTime,Global_active_power,Global_reactive_power,Global_intensity,Voltage) %>%
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Global_active_power,col="Global Active Power")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power")) +
  geom_line(aes(y=Global_intensity,col="Global Intensity")) +
  geom_line(aes(y=Voltage,col="Voltage"))

#2007.03
df.2007.03 %>% select(Date,DateTime,Global_active_power,Global_reactive_power,Global_intensity,Voltage) %>%
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Global_active_power,col="Global Active Power")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power")) +
  geom_line(aes(y=Global_intensity,col="Global Intensity")) +
  geom_line(aes(y=Voltage,col="Voltage"))

#2007.04
df.2007.04 %>% select(Date,DateTime,Global_active_power,Global_reactive_power,Global_intensity,Voltage) %>%
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Global_active_power,col="Global Active Power")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power")) +
  geom_line(aes(y=Global_intensity,col="Global Intensity")) +
  geom_line(aes(y=Voltage,col="Voltage"))

#2007.05
df.2007.05 %>% select(Date,DateTime,Global_active_power,Global_reactive_power,Global_intensity,Voltage) %>%
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Global_active_power,col="Global Active Power")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power")) +
  geom_line(aes(y=Global_intensity,col="Global Intensity")) +
  geom_line(aes(y=Voltage,col="Voltage"))

#2007.06
df.2007.06 %>% select(Date,DateTime,Global_active_power,Global_reactive_power,Global_intensity,Voltage) %>%
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Global_active_power,col="Global Active Power")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power")) +
  geom_line(aes(y=Global_intensity,col="Global Intensity")) +
  geom_line(aes(y=Voltage,col="Voltage"))

#2007.07
df.2007.07 %>% select(Date,DateTime,Global_active_power,Global_reactive_power,Global_intensity,Voltage) %>%
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Global_active_power,col="Global Active Power")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power")) +
  geom_line(aes(y=Global_intensity,col="Global Intensity")) +
  geom_line(aes(y=Voltage,col="Voltage"))

#2007.08
df.2007.08 %>% select(Date,DateTime,Global_active_power,Global_reactive_power,Global_intensity,Voltage) %>%
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Global_active_power,col="Global Active Power")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power")) +
  geom_line(aes(y=Global_intensity,col="Global Intensity")) +
  geom_line(aes(y=Voltage,col="Voltage"))

#2007.09
df.2007.09 %>% select(Date,DateTime,Global_active_power,Global_reactive_power,Global_intensity,Voltage) %>%
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Global_active_power,col="Global Active Power")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power")) +
  geom_line(aes(y=Global_intensity,col="Global Intensity")) +
  geom_line(aes(y=Voltage,col="Voltage"))

#2007.10
df.2007.10 %>% select(Date,DateTime,Global_active_power,Global_reactive_power,Global_intensity,Voltage) %>%
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Global_active_power,col="Global Active Power")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power")) +
  geom_line(aes(y=Global_intensity,col="Global Intensity")) +
  geom_line(aes(y=Voltage,col="Voltage"))

#2007.11
df.2007.11 %>% select(Date,DateTime,Global_active_power,Global_reactive_power,Global_intensity,Voltage) %>%
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Global_active_power,col="Global Active Power")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power")) +
  geom_line(aes(y=Global_intensity,col="Global Intensity")) +
  geom_line(aes(y=Voltage,col="Voltage"))

#2007.12
df.2007.12 %>% select(Date,DateTime,Global_active_power,Global_reactive_power,Global_intensity,Voltage) %>%
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Global_active_power,col="Global Active Power")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power")) +
  geom_line(aes(y=Global_intensity,col="Global Intensity")) +
  geom_line(aes(y=Voltage,col="Voltage"))

## 2008

#2008.01
df.2008.01 %>% select(Date,DateTime,Global_active_power,Global_reactive_power,Global_intensity,Voltage) %>%
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Global_active_power,col="Global Active Power")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power")) +
  geom_line(aes(y=Global_intensity,col="Global Intensity")) +
  geom_line(aes(y=Voltage,col="Voltage"))

#2008.02
df.2008.02 %>% select(Date,DateTime,Global_active_power,Global_reactive_power,Global_intensity,Voltage) %>%
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Global_active_power,col="Global Active Power")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power")) +
  geom_line(aes(y=Global_intensity,col="Global Intensity")) +
  geom_line(aes(y=Voltage,col="Voltage"))

#2008.03
df.2008.03 %>% select(Date,DateTime,Global_active_power,Global_reactive_power,Global_intensity,Voltage) %>%
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Global_active_power,col="Global Active Power")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power")) +
  geom_line(aes(y=Global_intensity,col="Global Intensity")) +
  geom_line(aes(y=Voltage,col="Voltage"))

#2008.04
df.2008.04 %>% select(Date,DateTime,Global_active_power,Global_reactive_power,Global_intensity,Voltage) %>%
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Global_active_power,col="Global Active Power")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power")) +
  geom_line(aes(y=Global_intensity,col="Global Intensity")) +
  geom_line(aes(y=Voltage,col="Voltage"))

#2008.05
df.2008.05 %>% select(Date,DateTime,Global_active_power,Global_reactive_power,Global_intensity,Voltage) %>%
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Global_active_power,col="Global Active Power")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power")) +
  geom_line(aes(y=Global_intensity,col="Global Intensity")) +
  geom_line(aes(y=Voltage,col="Voltage"))

#2008.06
df.2008.06 %>% select(Date,DateTime,Global_active_power,Global_reactive_power,Global_intensity,Voltage) %>%
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Global_active_power,col="Global Active Power")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power")) +
  geom_line(aes(y=Global_intensity,col="Global Intensity")) +
  geom_line(aes(y=Voltage,col="Voltage"))

#2008.07
df.2008.07 %>% select(Date,DateTime,Global_active_power,Global_reactive_power,Global_intensity,Voltage) %>%
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Global_active_power,col="Global Active Power")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power")) +
  geom_line(aes(y=Global_intensity,col="Global Intensity")) +
  geom_line(aes(y=Voltage,col="Voltage"))

#2008.08
df.2008.08 %>% select(Date,DateTime,Global_active_power,Global_reactive_power,Global_intensity,Voltage) %>%
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Global_active_power,col="Global Active Power")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power")) +
  geom_line(aes(y=Global_intensity,col="Global Intensity")) +
  geom_line(aes(y=Voltage,col="Voltage"))

#2008.09
df.2008.09 %>% select(Date,DateTime,Global_active_power,Global_reactive_power,Global_intensity,Voltage) %>%
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Global_active_power,col="Global Active Power")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power")) +
  geom_line(aes(y=Global_intensity,col="Global Intensity")) +
  geom_line(aes(y=Voltage,col="Voltage"))

#2008.10
df.2008.10 %>% select(Date,DateTime,Global_active_power,Global_reactive_power,Global_intensity,Voltage) %>%
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Global_active_power,col="Global Active Power")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power")) +
  geom_line(aes(y=Global_intensity,col="Global Intensity")) +
  geom_line(aes(y=Voltage,col="Voltage"))

#2008.11
df.2008.11 %>% select(Date,DateTime,Global_active_power,Global_reactive_power,Global_intensity,Voltage) %>%
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Global_active_power,col="Global Active Power")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power")) +
  geom_line(aes(y=Global_intensity,col="Global Intensity")) +
  geom_line(aes(y=Voltage,col="Voltage"))

#2008.12
df.2008.12 %>% select(Date,DateTime,Global_active_power,Global_reactive_power,Global_intensity,Voltage) %>%
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Global_active_power,col="Global Active Power")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power")) +
  geom_line(aes(y=Global_intensity,col="Global Intensity")) +
  geom_line(aes(y=Voltage,col="Voltage"))

## 2009
df.2009 %>% select(Date,DateTime,Global_active_power,Global_reactive_power,Global_intensity,Voltage) %>%
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Global_active_power,col="Global Active Power")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power")) +
  geom_line(aes(y=Global_intensity,col="Global Intensity")) +
  geom_line(aes(y=Voltage,col="Voltage"))

#2009.01
df.2009.01 %>% select(Date,DateTime,Global_active_power,Global_reactive_power,Global_intensity,Voltage) %>%
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Global_active_power,col="Global Active Power")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power")) +
  geom_line(aes(y=Global_intensity,col="Global Intensity")) +
  geom_line(aes(y=Voltage,col="Voltage"))

#2009.02
df.2009.02 %>% select(Date,DateTime,Global_active_power,Global_reactive_power,Global_intensity,Voltage) %>%
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Global_active_power,col="Global Active Power")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power")) +
  geom_line(aes(y=Global_intensity,col="Global Intensity")) +
  geom_line(aes(y=Voltage,col="Voltage"))

#2009.03
df.2009.03 %>% select(Date,DateTime,Global_active_power,Global_reactive_power,Global_intensity,Voltage) %>%
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Global_active_power,col="Global Active Power")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power")) +
  geom_line(aes(y=Global_intensity,col="Global Intensity")) +
  geom_line(aes(y=Voltage,col="Voltage"))

#2009.04
df.2009.04 %>% select(Date,DateTime,Global_active_power,Global_reactive_power,Global_intensity,Voltage) %>%
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Global_active_power,col="Global Active Power")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power")) +
  geom_line(aes(y=Global_intensity,col="Global Intensity")) +
  geom_line(aes(y=Voltage,col="Voltage"))

#2009.05
df.2009.05 %>% select(Date,DateTime,Global_active_power,Global_reactive_power,Global_intensity,Voltage) %>%
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Global_active_power,col="Global Active Power")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power")) +
  geom_line(aes(y=Global_intensity,col="Global Intensity")) +
  geom_line(aes(y=Voltage,col="Voltage"))

#2009.06
df.2009.06 %>% select(Date,DateTime,Global_active_power,Global_reactive_power,Global_intensity,Voltage) %>%
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Global_active_power,col="Global Active Power")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power")) +
  geom_line(aes(y=Global_intensity,col="Global Intensity")) +
  geom_line(aes(y=Voltage,col="Voltage"))

#2009.07
df.2009.07 %>% select(Date,DateTime,Global_active_power,Global_reactive_power,Global_intensity,Voltage) %>%
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Global_active_power,col="Global Active Power")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power")) +
  geom_line(aes(y=Global_intensity,col="Global Intensity")) +
  geom_line(aes(y=Voltage,col="Voltage"))

#2009.08
df.2009.08 %>% select(Date,DateTime,Global_active_power,Global_reactive_power,Global_intensity,Voltage) %>%
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Global_active_power,col="Global Active Power")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power")) +
  geom_line(aes(y=Global_intensity,col="Global Intensity")) +
  geom_line(aes(y=Voltage,col="Voltage"))

#2009.09
df.2009.09 %>% select(Date,DateTime,Global_active_power,Global_reactive_power,Global_intensity,Voltage) %>%
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Global_active_power,col="Global Active Power")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power")) +
  geom_line(aes(y=Global_intensity,col="Global Intensity")) +
  geom_line(aes(y=Voltage,col="Voltage"))

#2009.10
df.2009.10 %>% select(Date,DateTime,Global_active_power,Global_reactive_power,Global_intensity,Voltage) %>%
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Global_active_power,col="Global Active Power")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power")) +
  geom_line(aes(y=Global_intensity,col="Global Intensity")) +
  geom_line(aes(y=Voltage,col="Voltage"))

#2009.11
df.2009.11 %>% select(Date,DateTime,Global_active_power,Global_reactive_power,Global_intensity,Voltage) %>%
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Global_active_power,col="Global Active Power")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power")) +
  geom_line(aes(y=Global_intensity,col="Global Intensity")) +
  geom_line(aes(y=Voltage,col="Voltage"))

#2009.12
df.2009.12 %>% select(Date,DateTime,Global_active_power,Global_reactive_power,Global_intensity,Voltage) %>%
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Global_active_power,col="Global Active Power")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power")) +
  geom_line(aes(y=Global_intensity,col="Global Intensity")) +
  geom_line(aes(y=Voltage,col="Voltage"))

## 2010
df.2010 %>% select(Date,DateTime,Global_active_power,Global_reactive_power,Global_intensity,Voltage) %>%
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Global_active_power,col="Global Active Power")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power")) +
  geom_line(aes(y=Global_intensity,col="Global Intensity")) +
  geom_line(aes(y=Voltage,col="Voltage"))

#2010.01
df.2010.01 %>% select(Date,DateTime,Global_active_power,Global_reactive_power,Global_intensity,Voltage) %>%
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Global_active_power,col="Global Active Power")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power")) +
  geom_line(aes(y=Global_intensity,col="Global Intensity")) +
  geom_line(aes(y=Voltage,col="Voltage"))

#2010.02
df.2010.02 %>% select(Date,DateTime,Global_active_power,Global_reactive_power,Global_intensity,Voltage) %>%
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Global_active_power,col="Global Active Power")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power")) +
  geom_line(aes(y=Global_intensity,col="Global Intensity")) +
  geom_line(aes(y=Voltage,col="Voltage"))

#2010.03
df.2010.03 %>% select(Date,DateTime,Global_active_power,Global_reactive_power,Global_intensity,Voltage) %>%
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Global_active_power,col="Global Active Power")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power")) +
  geom_line(aes(y=Global_intensity,col="Global Intensity")) +
  geom_line(aes(y=Voltage,col="Voltage"))

#2010.04
df.2010.04 %>% select(Date,DateTime,Global_active_power,Global_reactive_power,Global_intensity,Voltage) %>%
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Global_active_power,col="Global Active Power")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power")) +
  geom_line(aes(y=Global_intensity,col="Global Intensity")) +
  geom_line(aes(y=Voltage,col="Voltage"))

#2010.05
df.2010.05 %>% select(Date,DateTime,Global_active_power,Global_reactive_power,Global_intensity,Voltage) %>%
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Global_active_power,col="Global Active Power")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power")) +
  geom_line(aes(y=Global_intensity,col="Global Intensity")) +
  geom_line(aes(y=Voltage,col="Voltage"))

#2010.06
df.2010.06 %>% select(Date,DateTime,Global_active_power,Global_reactive_power,Global_intensity,Voltage) %>%
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Global_active_power,col="Global Active Power")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power")) +
  geom_line(aes(y=Global_intensity,col="Global Intensity")) +
  geom_line(aes(y=Voltage,col="Voltage"))

#2010.07
df.2010.07 %>% select(Date,DateTime,Global_active_power,Global_reactive_power,Global_intensity,Voltage) %>%
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Global_active_power,col="Global Active Power")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power")) +
  geom_line(aes(y=Global_intensity,col="Global Intensity")) +
  geom_line(aes(y=Voltage,col="Voltage"))

#2010.08
df.2010.08 %>% select(Date,DateTime,Global_active_power,Global_reactive_power,Global_intensity,Voltage) %>%
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Global_active_power,col="Global Active Power")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power")) +
  geom_line(aes(y=Global_intensity,col="Global Intensity")) +
  geom_line(aes(y=Voltage,col="Voltage"))

#2010.09
df.2010.09 %>% select(Date,DateTime,Global_active_power,Global_reactive_power,Global_intensity,Voltage) %>%
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Global_active_power,col="Global Active Power")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power")) +
  geom_line(aes(y=Global_intensity,col="Global Intensity")) +
  geom_line(aes(y=Voltage,col="Voltage"))

#2010.10
df.2010.10 %>% select(Date,DateTime,Global_active_power,Global_reactive_power,Global_intensity,Voltage) %>%
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Global_active_power,col="Global Active Power")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power")) +
  geom_line(aes(y=Global_intensity,col="Global Intensity")) +
  geom_line(aes(y=Voltage,col="Voltage"))

#2010.11
df.2010.11 %>% select(Date,DateTime,Global_active_power,Global_reactive_power,Global_intensity,Voltage) %>%
  ggplot(aes(x=DateTime)) + geom_line(aes(y=Global_active_power,col="Global Active Power")) +
  geom_line(aes(y=Global_reactive_power,col="Global Reactive Power")) +
  geom_line(aes(y=Global_intensity,col="Global Intensity")) +
  geom_line(aes(y=Voltage,col="Voltage"))



##...

#### Building models



#### Applying model






















##missing values

str(df)
summary(df)
is.na(df)
names(which(colSums(is.na(df))>0))

colSums(is.na(df))
row.names(which(rowSums(is.na(df))>0))

na_df <- df[rowSums(is.na(df)) > 0,]
names(which(colSums(is.na(new_df))>0))

df2 <- df[rowSums(is.na(df)) == 0,]



####relationships between variables

na_df.2007.1q <- subset(na_df, Date>"2006-12-31" & Date<"2007-04-01")
na_df.2007.2q <- subset(na_df, Date>"2007-03-31" & Date<"2007-07-01")
na_df.2007.3q <- subset(na_df, Date>"2007-06-30" & Date<"2007-10-01")
na_df.2007.4q <- subset(na_df, Date>"2007-09-30" & Date<"2008-01-01")
na_df.2008.1q <- subset(na_df, Date>"2007-12-31" & Date<"2008-04-01")
na_df.2008.2q <- subset(na_df, Date>"2008-03-31" & Date<"2008-07-01")
na_df.2008.3q <- subset(na_df, Date>"2008-06-30" & Date<"2008-10-01")
na_df.2008.4q <- subset(na_df, Date>"2008-09-30" & Date<"2009-01-01")
na_df.2009.1q <- subset(na_df, Date>"2008-12-31" & Date<"2009-04-01")
na_df.2009.2q <- subset(na_df, Date>"2009-03-31" & Date<"2009-07-01")
na_df.2009.3q <- subset(na_df, Date>"2009-06-30" & Date<"2009-10-01")
na_df.2009.4q <- subset(na_df, Date>"2009-09-30" & Date<"2010-01-01")
na_df.2010.1q <- subset(na_df, Date>"2009-12-31" & Date<"2010-04-01")
na_df.2010.2q <- subset(na_df, Date>"2010-03-31" & Date<"2010-07-01")
na_df.2010.3q <- subset(na_df, Date>"2010-06-30" & Date<"2010-10-01")
na_df.2010.4q <- subset(na_df, Date>"2010-09-30" & Date<"2010-01-01")


na_df.2010.07 <- subset(na_df, Date>"2010-06-30" & Date<"2010-08-01")
na_df.2010.08 <- subset(na_df, Date>"2010-07-31" & Date<"2010-09-01")
na_df.2010.09 <- subset(na_df, Date>"2010-08-31" & Date<"2010-10-01")

summary(na_df.2010.08)


# Grafics de consum d'un mes qualsevol
plot(df.2007.01$DateTime,df.2007.01$Sub_metering_1,type = "l",xlab = "Temps",ylab="Consum [Wh]",col="red")
lines(df.2007.01$DateTime,df.2007.01$Sub_metering_2,type = "l",col="green")
lines(df.2007.01$DateTime,df.2007.01$Sub_metering_3,type = "l",col="blue")

g1.07 <- ggplot(df.2007.01, aes(x=DateTime))
g1.07 <- g + geom_line(aes(y=Sub_metering_1), colour="red")
g1.07 <- g + geom_line(aes(y=Sub_metering_2), colour="green")
g1.07 <- g + geom_line(aes(y=Sub_metering_3), colour="blue")
g1.07 <- g + geom_line(aes(y=Global_active_power), colour="black")
g1.07 <- g + ylab("Consumption [Wh]")
g1.07

g2.07 <- ggplot(df.2007.02, aes(x=DateTime))
g2.07 <- g + geom_line(aes(y=Sub_metering_1), colour="red")
g2.07 <- g + geom_line(aes(y=Sub_metering_2), colour="green")
g2.07 <- g + geom_line(aes(y=Sub_metering_3), colour="blue")
g2.07 <- g + geom_line(aes(y=Global_active_power), colour="black")
g2.07 <- g + ylab("Consumption [Wh]")
g2.07

g07 <- ggplot(df.2007, aes(x=DateTime))
g07 <- g07 + geom_line(aes(y=Sub_metering_1), colour="red")
g07 <- g07 + geom_line(aes(y=Sub_metering_2), colour="green")
g07 <- g07 + geom_line(aes(y=Sub_metering_3), colour="blue")
g07 <- g07 + geom_line(aes(y=Global_active_power), colour="black")
g07 <- g07 + ylab("Consumption [Wh]")
g07

g10 <- ggplot(df.2010, aes(x=DateTime))
g10 <- g10 + geom_line(aes(y=Sub_metering_1), colour="red")
g10 <- g10 + geom_line(aes(y=Sub_metering_2), colour="green")
g10 <- g10 + geom_line(aes(y=Sub_metering_3), colour="blue")
g10 <- g10 + geom_line(aes(y=Global_active_power), colour="black")
g10 <- g10 + ylab("Consumption [Wh]")
g10

g8.10 <- ggplot(df.2010.08, aes(x=DateTime))
g8.10 <- g8.10 + geom_line(aes(y=Sub_metering_1), colour="red")
g8.10 <- g8.10 + geom_line(aes(y=Sub_metering_2), colour="green")
g8.10 <- g8.10 + geom_line(aes(y=Sub_metering_3), colour="blue")
g8.10 <- g8.10 + geom_line(aes(y=Global_active_power), colour="black")
g8.10 <- g8.10 + ylab("Consumption [Wh]")
g8.10


#plot(df.2007.01$DateTime,df.2007.01$Sub_metering_2,type = "l",main = "Laundry",xlab = "Time",ylab="Consumption [Wh]")
#plot(df.2007.01$DateTime,df.2007.01$Sub_metering_3,type = "l",main = "AC/Heating",xlab = "Time",ylab="Consumption [Wh]")


#### 2007 plots


Months <-       c("January","February","March","April","May",
                  "June","July","August","September",
                  "October","November","December")

S1.07.Median.Month<-     c(median(na.omit(df.2007.01$Sub_metering_1)),
                           median(na.omit(df.2007.02$Sub_metering_1)),
                           median(na.omit(df.2007.03$Sub_metering_1)),
                           median(na.omit(df.2007.04$Sub_metering_1)),
                           median(na.omit(df.2007.05$Sub_metering_1)),
                           median(na.omit(df.2007.06$Sub_metering_1)),
                           median(na.omit(df.2007.07$Sub_metering_1)),
                           median(na.omit(df.2007.08$Sub_metering_1)),
                           median(na.omit(df.2007.09$Sub_metering_1)),
                           median(na.omit(df.2007.10$Sub_metering_1)),
                           median(na.omit(df.2007.11$Sub_metering_1)),
                           median(na.omit(df.2007.12$Sub_metering_1)))

S2.07.Median.Month<-          c(median(na.omit(df.2007.01$Sub_metering_2)),
                                median(na.omit(df.2007.02$Sub_metering_2)),
                                median(na.omit(df.2007.03$Sub_metering_2)),
                                median(na.omit(df.2007.04$Sub_metering_2)),
                                median(na.omit(df.2007.05$Sub_metering_2)),
                                median(na.omit(df.2007.06$Sub_metering_2)),
                                median(na.omit(df.2007.07$Sub_metering_2)),
                                median(na.omit(df.2007.08$Sub_metering_2)),
                                median(na.omit(df.2007.09$Sub_metering_2)),
                                median(na.omit(df.2007.10$Sub_metering_2)),
                                median(na.omit(df.2007.11$Sub_metering_2)),
                                median(na.omit(df.2007.12$Sub_metering_2)))

S3.07.Median.Month<-             c(median(na.omit(df.2007.01$Sub_metering_3)),
                                   median(na.omit(df.2007.02$Sub_metering_3)),
                                   median(na.omit(df.2007.03$Sub_metering_3)),
                                   median(na.omit(df.2007.04$Sub_metering_3)),
                                   median(na.omit(df.2007.05$Sub_metering_3)),
                                   median(na.omit(df.2007.06$Sub_metering_3)),
                                   median(na.omit(df.2007.07$Sub_metering_3)),
                                   median(na.omit(df.2007.08$Sub_metering_3)),
                                   median(na.omit(df.2007.09$Sub_metering_3)),
                                   median(na.omit(df.2007.10$Sub_metering_3)),
                                   median(na.omit(df.2007.11$Sub_metering_3)),
                                   median(na.omit(df.2007.12$Sub_metering_3)))

GAP.07.Median.Month<-         c(median(na.omit(df.2007.01$Global_active_power)),
                                median(na.omit(df.2007.02$Global_active_power)),
                                median(na.omit(df.2007.03$Global_active_power)),
                                median(na.omit(df.2007.04$Global_active_power)),
                                median(na.omit(df.2007.05$Global_active_power)),
                                median(na.omit(df.2007.06$Global_active_power)),
                                median(na.omit(df.2007.07$Global_active_power)),
                                median(na.omit(df.2007.08$Global_active_power)),
                                median(na.omit(df.2007.09$Global_active_power)),
                                median(na.omit(df.2007.10$Global_active_power)),
                                median(na.omit(df.2007.11$Global_active_power)),
                                median(na.omit(df.2007.12$Global_active_power)))


MonthlyMedian07<- cbind.data.frame(Months,GAP.07.Median,S1.07.Median,S2.07.Median,S3.07.Median)




S1.07.Mean.Month<-     c(mean(na.omit(df.2007.01$Sub_metering_1)),
                         mean(na.omit(df.2007.02$Sub_metering_1)),
                         mean(na.omit(df.2007.03$Sub_metering_1)),
                         mean(na.omit(df.2007.04$Sub_metering_1)),
                         mean(na.omit(df.2007.05$Sub_metering_1)),
                         mean(na.omit(df.2007.06$Sub_metering_1)),
                         mean(na.omit(df.2007.07$Sub_metering_1)),
                         mean(na.omit(df.2007.08$Sub_metering_1)),
                         mean(na.omit(df.2007.09$Sub_metering_1)),
                         mean(na.omit(df.2007.10$Sub_metering_1)),
                         mean(na.omit(df.2007.11$Sub_metering_1)),
                         mean(na.omit(df.2007.12$Sub_metering_1)))

S2.07.Mean.Month<-          c(mean(na.omit(df.2007.01$Sub_metering_2)),
                              mean(na.omit(df.2007.02$Sub_metering_2)),
                              mean(na.omit(df.2007.03$Sub_metering_2)),
                              mean(na.omit(df.2007.04$Sub_metering_2)),
                              mean(na.omit(df.2007.05$Sub_metering_2)),
                              mean(na.omit(df.2007.06$Sub_metering_2)),
                              mean(na.omit(df.2007.07$Sub_metering_2)),
                              mean(na.omit(df.2007.08$Sub_metering_2)),
                              mean(na.omit(df.2007.09$Sub_metering_2)),
                              mean(na.omit(df.2007.10$Sub_metering_2)),
                              mean(na.omit(df.2007.11$Sub_metering_2)),
                              mean(na.omit(df.2007.12$Sub_metering_2)))

S3.07.Mean.Month<-             c(mean(na.omit(df.2007.01$Sub_metering_3)),
                                 mean(na.omit(df.2007.02$Sub_metering_3)),
                                 mean(na.omit(df.2007.03$Sub_metering_3)),
                                 mean(na.omit(df.2007.04$Sub_metering_3)),
                                 mean(na.omit(df.2007.05$Sub_metering_3)),
                                 mean(na.omit(df.2007.06$Sub_metering_3)),
                                 mean(na.omit(df.2007.07$Sub_metering_3)),
                                 mean(na.omit(df.2007.08$Sub_metering_3)),
                                 mean(na.omit(df.2007.09$Sub_metering_3)),
                                 mean(na.omit(df.2007.10$Sub_metering_3)),
                                 mean(na.omit(df.2007.11$Sub_metering_3)),
                                 mean(na.omit(df.2007.12$Sub_metering_3)))

GAP.07.Mean.Month<-         c(mean(na.omit(df.2007.01$Global_active_power)),
                              mean(na.omit(df.2007.02$Global_active_power)),
                              mean(na.omit(df.2007.03$Global_active_power)),
                              mean(na.omit(df.2007.04$Global_active_power)),
                              mean(na.omit(df.2007.05$Global_active_power)),
                              mean(na.omit(df.2007.06$Global_active_power)),
                              mean(na.omit(df.2007.07$Global_active_power)),
                              mean(na.omit(df.2007.08$Global_active_power)),
                              mean(na.omit(df.2007.09$Global_active_power)),
                              mean(na.omit(df.2007.10$Global_active_power)),
                              mean(na.omit(df.2007.11$Global_active_power)),
                              mean(na.omit(df.2007.12$Global_active_power)))

MonthlyMean07 <- cbind.data.frame(Months,GAP.07.Mean,S1.07.Mean,S2.07.Mean,S3.07.Mean)
MonthlyMean07$Months <- m(MonthlyMean07$Months)


#### 2008 plots



#### Variables behaviour by year

Year<- c("2007","2008","2009","2010")

GAP.AnnualMedian <-       c(median(na.omit(df.2007$Global_active_power)),
                            median(na.omit(df.2008$Global_active_power)),
                            median(na.omit(df.2009$Global_active_power)),
                            median(na.omit(df.2010$Global_active_power)))

S1.AnnualMedian<-         c(median(na.omit(df.2007$Sub_metering_1)),
                            median(na.omit(df.2008$Sub_metering_1)),
                            median(na.omit(df.2009$Sub_metering_1)),
                            median(na.omit(df.2010$Sub_metering_1)))

S2.AnnualMedian<-         c(median(na.omit(df.2007$Sub_metering_2)),
                            median(na.omit(df.2008$Sub_metering_2)),
                            median(na.omit(df.2009$Sub_metering_2)),
                            median(na.omit(df.2010$Sub_metering_2)))

S3.AnnualMedian<-        c(median(na.omit(df.2007$Sub_metering_3)),
                           median(na.omit(df.2008$Sub_metering_3)),
                           median(na.omit(df.2009$Sub_metering_3)),
                           median(na.omit(df.2010$Sub_metering_3)))


AnnualMedian<- cbind.data.frame(Year,GAP.AnnualMedian,S1.AnnualMedian,
                                S2.AnnualMedian,S3.AnnualMedian)


####Annual Mean

GAP.AnnualMean <-       c(mean(na.omit(df.2007$Global_active_power)),
                          mean(na.omit(df.2008$Global_active_power)),
                          mean(na.omit(df.2009$Global_active_power)),
                          mean(na.omit(df.2010$Global_active_power)))

S1.AnnualMean<-         c(mean(na.omit(df.2007$Sub_metering_1)),
                          mean(na.omit(df.2008$Sub_metering_1)),
                          mean(na.omit(df.2009$Sub_metering_1)),
                          mean(na.omit(df.2010$Sub_metering_1)))

S2.AnnualMean<-         c(mean(na.omit(df.2007$Sub_metering_2)),
                          mean(na.omit(df.2008$Sub_metering_2)),
                          mean(na.omit(df.2009$Sub_metering_2)),
                          mean(na.omit(df.2010$Sub_metering_2)))

S3.AnnualMean<-        c(mean(na.omit(df.2007$Sub_metering_3)),
                         mean(na.omit(df.2008$Sub_metering_3)),
                         mean(na.omit(df.2009$Sub_metering_3)),
                         mean(na.omit(df.2010$Sub_metering_3)))


AnnualMean<- cbind.data.frame(Year,GAP.AnnualMean,S1.AnnualMean,
                              S2.AnnualMean,S3.AnnualMean)


g.months07 <- ggplot(Mean07, aes(x=Months))
g.months07 <- g.months07 + geom_line(aes(y=S1.07.Mean.Month), colour="red")
g.months07 <- g.months07 + geom_line(aes(y=S2.07.Mean.Month), colour="green")
g.months07 <- g.months07 + geom_line(aes(y=S3.07.Mean.Month), colour="blue")
g.months07 <- g.months07 + geom_line(aes(y=GAP.07.Mean.Month), colour="orange")
g.months07 <- g.months07 + ylab("Mean")
g.months07