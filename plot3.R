######################################################
##  
##  Exploratory Data Analysis - Plot1.R - 2014-07-13
##
######################################################


## Read dataset
df = read.csv("./data/household_power_consumption.txt", sep=";")

## Replace '?' symbol for missing values with NA
df[df == '?'] <- NA

## The variables and their classes are below
##                  Date                  Time   Global_active_power 
##              "factor"              "factor"              "factor" 
## Global_reactive_power               Voltage      Global_intensity 
##              "factor"              "factor"              "factor" 
##        Sub_metering_1        Sub_metering_2        Sub_metering_3 
##              "factor"              "factor"             "numeric"

## Convert factor variables to numeric
for (i in 3:8) {
  df[,i]=as.numeric(levels(df[,i])[df[,i]])
}

## set the class of date column to date
df[,1] <- as.Date(df[,1], '%d/%m/%Y')

## Create a new datetime column of class POSIXlt from the Date and Time columns
df$datetime <- strptime(paste(df[,1], df[,2]), '%Y-%m-%d %H:%M:%S')

## extract subset for analysis
df1 = subset(df, df$Date == "2007-02-01" | df$Date == "2007-02-02")

## prepare plot3
png(filename='plot3.png', width=480, height=480, unit='px', pointsize=12, bg='white', res=NA, restoreConsole=TRUE)
plot(df1$datetime, df1$Sub_metering_1, main='', type="n", xlab='', ylab='Energy sub metering')
lines(df1$datetime, df1$Sub_metering_1, main='', type="l", col='black')
lines(df1$datetime, df1$Sub_metering_2, main='', type="l", col='red')
lines(df1$datetime, df1$Sub_metering_3, main='', type="l", col='blue')
legend('topright', col = c('black', 'red', 'blue'), legend=c('Sub_metering_1','Sub_metering_2','Sub_metering_3'), lty=c(1,1,1))
dev.off()
