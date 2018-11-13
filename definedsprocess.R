

#### first commit

setwd("C:/Users/User/Desktop/DataAnalytics/41DefineDSProcess")

library(dplyr)
library(lubridate)
library(ggplot2)


#### Loading and exploring dataset

df<- read.table("household_power_consumption.txt", sep = ";", dec = ".", header = TRUE, na.strings = c("NA","?"))

df[1:6,]
head(df)


df <-cbind(df,paste(df$Date,df$Time), stringsAsFactors=FALSE)
colnames(df)[10] <-"DateTime"
df <- df[,c(ncol(df), 1:(ncol(df)-1))]

df$DateTime<- dmy_hms(paste(df$Date,df$Time))
df$Date<- dmy(df$Date)
df$Time<- hms(df$Time)
df$DateTime <- as_datetime(df$DateTime)
df$Date <- as_date(df$Date)

df$Global_active_power <- as.numeric(df$Global_active_power)
df$Global_reactive_power <- as.numeric(df$Global_reactive_power)
df$Voltage <- as.numeric(df$Voltage)
df$Global_intensity <- as.numeric(df$Global_intensity)
df$Sub_metering_1 <- as.numeric(df$Sub_metering_1)
df$Sub_metering_2 <- as.numeric(df$Sub_metering_2)
df$Sub_metering_3 <- as.numeric(df$Sub_metering_3)

##missing values

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