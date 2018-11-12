

#### first commit

setwd("C:/Users/User/Desktop/DataAnalytics/41DefineDSProcess")


#### Loading and exploring dataset

df<- read.table("household_power_consumption.txt", sep = ";", dec = ".", header = TRUE)

df[1:6,]
head(df)


df <-cbind(df,paste(df$Date,df$Time), stringsAsFactors=FALSE)
colnames(df)[10] <-"DateTime"
df <- df[,c(ncol(df), 1:(ncol(df)-1))]
head(df)


df$DateTime <- strptime(df$DateTime, "%d/%m/%Y %H:%M:%S")
df$Date <- as.Date(df$Date, "%d/%m/%Y")
str(df)