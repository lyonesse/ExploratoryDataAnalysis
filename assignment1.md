Assignement 1 - Household Power Consumption Analysis
-----------------------------------------------------

### References
  * [Date Time classins in R](https://www.stat.berkeley.edu/classes/s133/dates.html)

### Load libraries

```r
library(lubridate)
```


### Load dataset

```r
df = read.csv("./data/household_power_consumption.txt", sep = ";")
```



### Prepare missing values

```r
## size of dataset
dim(df)
```

```
## [1] 2075259       9
```

```r
## find classes of columns
sapply(df[1, ], class)
```

```
##                  Date                  Time   Global_active_power 
##              "factor"              "factor"              "factor" 
## Global_reactive_power               Voltage      Global_intensity 
##              "factor"              "factor"              "factor" 
##        Sub_metering_1        Sub_metering_2        Sub_metering_3 
##              "factor"              "factor"             "numeric"
```

```r
## Replace '?' symbol for missing values with NA
df[df == "?"] <- NA
```


### Prepare column classes

```r
## Converting factors to numeric: as.numeric(levels(fac))[fac]

for (i in 3:8) {
    df[, i] = as.numeric(levels(df[, i])[df[, i]])
}
sapply(df[1, ], class)
```

```
##                  Date                  Time   Global_active_power 
##              "factor"              "factor"             "numeric" 
## Global_reactive_power               Voltage      Global_intensity 
##             "numeric"             "numeric"             "numeric" 
##        Sub_metering_1        Sub_metering_2        Sub_metering_3 
##             "numeric"             "numeric"             "numeric"
```

```r
head(df)
```

```
##         Date     Time Global_active_power Global_reactive_power Voltage
## 1 16/12/2006 17:24:00               4.216                 0.418   234.8
## 2 16/12/2006 17:25:00               5.360                 0.436   233.6
## 3 16/12/2006 17:26:00               5.374                 0.498   233.3
## 4 16/12/2006 17:27:00               5.388                 0.502   233.7
## 5 16/12/2006 17:28:00               3.666                 0.528   235.7
## 6 16/12/2006 17:29:00               3.520                 0.522   235.0
##   Global_intensity Sub_metering_1 Sub_metering_2 Sub_metering_3
## 1             18.4              0              1             17
## 2             23.0              0              1             16
## 3             23.0              0              2             17
## 4             23.0              0              1             17
## 5             15.8              0              1             17
## 6             15.0              0              2             17
```

```r
summary(df)
```

```
##         Date               Time         Global_active_power
##  1/1/2007 :   1440   17:24:00:   1442   Min.   : 0         
##  1/1/2008 :   1440   17:25:00:   1442   1st Qu.: 0         
##  1/1/2009 :   1440   17:26:00:   1442   Median : 1         
##  1/1/2010 :   1440   17:27:00:   1442   Mean   : 1         
##  1/10/2007:   1440   17:28:00:   1442   3rd Qu.: 2         
##  1/10/2008:   1440   17:29:00:   1442   Max.   :11         
##  (Other)  :2066619   (Other) :2066607   NA's   :25979      
##  Global_reactive_power    Voltage      Global_intensity Sub_metering_1 
##  Min.   :0             Min.   :223     Min.   : 0       Min.   : 0     
##  1st Qu.:0             1st Qu.:239     1st Qu.: 1       1st Qu.: 0     
##  Median :0             Median :241     Median : 3       Median : 0     
##  Mean   :0             Mean   :241     Mean   : 5       Mean   : 1     
##  3rd Qu.:0             3rd Qu.:243     3rd Qu.: 6       3rd Qu.: 0     
##  Max.   :1             Max.   :254     Max.   :48       Max.   :88     
##  NA's   :25979         NA's   :25979   NA's   :25979    NA's   :25979  
##  Sub_metering_2  Sub_metering_3 
##  Min.   : 0      Min.   : 0     
##  1st Qu.: 0      1st Qu.: 0     
##  Median : 0      Median : 1     
##  Mean   : 1      Mean   : 6     
##  3rd Qu.: 1      3rd Qu.:17     
##  Max.   :80      Max.   :31     
##  NA's   :25979   NA's   :25979
```


There are two POSIX date/time classes, which differ in the way that the values are stored internally. The POSIXct class stores date/time values as the number of seconds since January 1, 1970, while the POSIXlt class stores them as a list with elements for second, minute, hour, day, month, and year, among others. Unless you need the list nature of the POSIXlt class, the POSIXct class is the usual choice for storing dates in R. 

### Prepare date and time columns

```r
## set the class of date column to date
df[, 1] <- as.Date(df[, 1], "%d/%m/%Y")
df$datetime <- strptime(paste(df[, 1], df[, 2]), "%Y-%m-%d %H:%M:%S")
sapply(df[1, ], class)
```

```
## $Date
## [1] "Date"
## 
## $Time
## [1] "factor"
## 
## $Global_active_power
## [1] "numeric"
## 
## $Global_reactive_power
## [1] "numeric"
## 
## $Voltage
## [1] "numeric"
## 
## $Global_intensity
## [1] "numeric"
## 
## $Sub_metering_1
## [1] "numeric"
## 
## $Sub_metering_2
## [1] "numeric"
## 
## $Sub_metering_3
## [1] "numeric"
## 
## $datetime
## [1] "POSIXlt" "POSIXt"
```

```r
head(df)
```

```
##         Date     Time Global_active_power Global_reactive_power Voltage
## 1 2006-12-16 17:24:00               4.216                 0.418   234.8
## 2 2006-12-16 17:25:00               5.360                 0.436   233.6
## 3 2006-12-16 17:26:00               5.374                 0.498   233.3
## 4 2006-12-16 17:27:00               5.388                 0.502   233.7
## 5 2006-12-16 17:28:00               3.666                 0.528   235.7
## 6 2006-12-16 17:29:00               3.520                 0.522   235.0
##   Global_intensity Sub_metering_1 Sub_metering_2 Sub_metering_3
## 1             18.4              0              1             17
## 2             23.0              0              1             16
## 3             23.0              0              2             17
## 4             23.0              0              1             17
## 5             15.8              0              1             17
## 6             15.0              0              2             17
##              datetime
## 1 2006-12-16 17:24:00
## 2 2006-12-16 17:25:00
## 3 2006-12-16 17:26:00
## 4 2006-12-16 17:27:00
## 5 2006-12-16 17:28:00
## 6 2006-12-16 17:29:00
```

```r
summary(df)
```

```
##       Date                  Time         Global_active_power
##  Min.   :2006-12-16   17:24:00:   1442   Min.   : 0         
##  1st Qu.:2007-12-12   17:25:00:   1442   1st Qu.: 0         
##  Median :2008-12-06   17:26:00:   1442   Median : 1         
##  Mean   :2008-12-05   17:27:00:   1442   Mean   : 1         
##  3rd Qu.:2009-12-01   17:28:00:   1442   3rd Qu.: 2         
##  Max.   :2010-11-26   17:29:00:   1442   Max.   :11         
##                       (Other) :2066607   NA's   :25979      
##  Global_reactive_power    Voltage      Global_intensity Sub_metering_1 
##  Min.   :0             Min.   :223     Min.   : 0       Min.   : 0     
##  1st Qu.:0             1st Qu.:239     1st Qu.: 1       1st Qu.: 0     
##  Median :0             Median :241     Median : 3       Median : 0     
##  Mean   :0             Mean   :241     Mean   : 5       Mean   : 1     
##  3rd Qu.:0             3rd Qu.:243     3rd Qu.: 6       3rd Qu.: 0     
##  Max.   :1             Max.   :254     Max.   :48       Max.   :88     
##  NA's   :25979         NA's   :25979   NA's   :25979    NA's   :25979  
##  Sub_metering_2  Sub_metering_3     datetime                  
##  Min.   : 0      Min.   : 0      Min.   :2006-12-16 17:24:00  
##  1st Qu.: 0      1st Qu.: 0      1st Qu.:2007-12-12 00:18:30  
##  Median : 0      Median : 1      Median :2008-12-06 07:13:00  
##  Mean   : 1      Mean   : 6      Mean   :2008-12-06 06:48:06  
##  3rd Qu.: 1      3rd Qu.:17      3rd Qu.:2009-12-01 14:07:30  
##  Max.   :80      Max.   :31      Max.   :2010-11-26 21:02:00  
##  NA's   :25979   NA's   :25979   NA's   :240
```

```r
df.na = subset(df, df$datetime == NA)
```



### Extract subset for analysis

```r
df1 = subset(df, df$Date == "2007-02-01" | df$Date == "2007-02-02")
head(df1)
```

```
##             Date     Time Global_active_power Global_reactive_power
## 66637 2007-02-01 00:00:00               0.326                 0.128
## 66638 2007-02-01 00:01:00               0.326                 0.130
## 66639 2007-02-01 00:02:00               0.324                 0.132
## 66640 2007-02-01 00:03:00               0.324                 0.134
## 66641 2007-02-01 00:04:00               0.322                 0.130
## 66642 2007-02-01 00:05:00               0.320                 0.126
##       Voltage Global_intensity Sub_metering_1 Sub_metering_2
## 66637   243.2              1.4              0              0
## 66638   243.3              1.4              0              0
## 66639   243.5              1.4              0              0
## 66640   243.9              1.4              0              0
## 66641   243.2              1.4              0              0
## 66642   242.3              1.4              0              0
##       Sub_metering_3            datetime
## 66637              0 2007-02-01 00:00:00
## 66638              0 2007-02-01 00:01:00
## 66639              0 2007-02-01 00:02:00
## 66640              0 2007-02-01 00:03:00
## 66641              0 2007-02-01 00:04:00
## 66642              0 2007-02-01 00:05:00
```

```r
dim(df1)
```

```
## [1] 2880   10
```

```r
summary(df1)
```

```
##       Date                  Time      Global_active_power
##  Min.   :2007-02-01   00:00:00:   2   Min.   :0.22       
##  1st Qu.:2007-02-01   00:01:00:   2   1st Qu.:0.32       
##  Median :2007-02-01   00:02:00:   2   Median :1.06       
##  Mean   :2007-02-01   00:03:00:   2   Mean   :1.21       
##  3rd Qu.:2007-02-02   00:04:00:   2   3rd Qu.:1.69       
##  Max.   :2007-02-02   00:05:00:   2   Max.   :7.48       
##                       (Other) :2868                      
##  Global_reactive_power    Voltage    Global_intensity Sub_metering_1 
##  Min.   :0.000         Min.   :233   Min.   : 1.0     Min.   : 0.00  
##  1st Qu.:0.000         1st Qu.:238   1st Qu.: 1.4     1st Qu.: 0.00  
##  Median :0.104         Median :241   Median : 4.6     Median : 0.00  
##  Mean   :0.101         Mean   :240   Mean   : 5.1     Mean   : 0.41  
##  3rd Qu.:0.144         3rd Qu.:242   3rd Qu.: 7.0     3rd Qu.: 0.00  
##  Max.   :0.500         Max.   :247   Max.   :32.0     Max.   :38.00  
##                                                                      
##  Sub_metering_2  Sub_metering_3    datetime                  
##  Min.   :0.000   Min.   : 0.0   Min.   :2007-02-01 00:00:00  
##  1st Qu.:0.000   1st Qu.: 0.0   1st Qu.:2007-02-01 11:59:45  
##  Median :0.000   Median : 0.0   Median :2007-02-01 23:59:30  
##  Mean   :0.258   Mean   : 8.5   Mean   :2007-02-01 23:59:30  
##  3rd Qu.:0.000   3rd Qu.:17.0   3rd Qu.:2007-02-02 11:59:15  
##  Max.   :2.000   Max.   :19.0   Max.   :2007-02-02 23:59:00  
## 
```



### Disply NA values 

```r
head(df.na)
```

```
##  [1] Date                  Time                  Global_active_power  
##  [4] Global_reactive_power Voltage               Global_intensity     
##  [7] Sub_metering_1        Sub_metering_2        Sub_metering_3       
## [10] datetime             
## <0 rows> (or 0-length row.names)
```




### Prepare plot1

```r
# png(filename='plot1.png', width=480, height=480, unit='px',
# pointsize=12, bg='white', res=NA, restoreConsole=TRUE)
hist(df1$Global_active_power, col = "red", main = "Global Active Power", xlab = "Global Active Power (kilowatts)")
```

![plot of chunk s6](figure/s6.png) 

```r
# dev.off()
```


### Prepare plot2

```r
# png(filename='plot2.png', width=480, height=480, unit='px',
# pointsize=12, bg='white', res=NA, restoreConsole=TRUE)
plot(df1$datetime, df1$Global_active_power, main = "", type = "l", xlab = "", 
    ylab = "Global Active Power (kilowatts)")
```

![plot of chunk s7](figure/s7.png) 

```r
# dev.off()
```


### Prepare plot3

```r
# png(filename='plot3.png', width=480, height=480, unit='px',
# pointsize=12, bg='white', res=NA, restoreConsole=TRUE)
plot(df1$datetime, df1$Sub_metering_1, main = "", type = "n", xlab = "", ylab = "Energy sub metering")
lines(df1$datetime, df1$Sub_metering_1, main = "", type = "l", col = "black")
lines(df1$datetime, df1$Sub_metering_2, main = "", type = "l", col = "red")
lines(df1$datetime, df1$Sub_metering_3, main = "", type = "l", col = "blue")
legend("topright", col = c("black", "red", "blue"), legend = c("Sub_metering_1", 
    "Sub_metering_2", "Sub_metering_3"), lty = c(1, 1, 1))
```

![plot of chunk s8](figure/s8.png) 

```r
# dev.off()
```



### Prepare plot4

```r
# png(filename='plot4.png', width=480, height=480, unit='px',
# pointsize=12, bg='white', res=NA, restoreConsole=TRUE)
par(mfrow = c(2, 2))
with(df1, {
    plot(datetime, Global_active_power, main = "", type = "l", xlab = "", ylab = "Global Active Power")
    plot(datetime, Voltage, main = "", type = "l", ylab = "Voltage")
    plot(datetime, Sub_metering_1, main = "", type = "n", xlab = "", ylab = "Energy sub metering")
    lines(df1$datetime, df1$Sub_metering_1, main = "", type = "l", col = "black")
    lines(df1$datetime, df1$Sub_metering_2, main = "", type = "l", col = "red")
    lines(df1$datetime, df1$Sub_metering_3, main = "", type = "l", col = "blue")
    legend("topright", col = c("black", "red", "blue"), legend = c("Sub_metering_1", 
        "Sub_metering_2", "Sub_metering_3"), lty = c(1, 1, 1))
    plot(datetime, Global_reactive_power, main = "", type = "l")
})
```

![plot of chunk s9](figure/s9.png) 

```r
# dev.off()
```



