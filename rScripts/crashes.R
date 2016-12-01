#Loading crashes data for NYC 2014-2015
crashes <- read.csv("capstone_proj/NYPD_Motor_Vehicle_Collisions.csv")
crashes <- crashes[, c(2,4:6, 8, 10, 11)]

#Identiying if crash involved has injuries/ fatalities or none.
#Also, extracating hour of crash
FATALITIES_INJURIES = CRASH_HOUR = vector()
for(i in 1:nrow(crashes)){
  if(sum(crashes[i, 6:7]) == 0) {FATALITIES_INJURIES[[i]] = 0}
  else {FATALITIES_INJURIES[[i]] = 1}
  CRASH_HOUR[[i]] <- format(as.POSIXct(crashes[i, 1], format = "%H:%M"), "%H")
}

data <- cbind(CRASH_HOUR, crashes[,1:5], FATALITIES_INJURIES)
colnames(data)[5] <- "STREETNAME"

#Grouping accidents by timeperiods
TIMEPERIOD <- vector()
for(i in 1:nrow(data)){
  if(data[i,1] %in% c(07:09)){
    TIMEPERIOD[[i]] <- 7
  }
  else if(data[i,1] %in% c(10:13)){
    TIMEPERIOD[[i]] <- 10}
  else if(data[i,1] %in% c(14:15)){
    TIMEPERIOD[[i]] <- 14}
  else if(data[i,1] %in% c(16:17)){
    TIMEPERIOD[[i]] <- 16}
  else if(data[i,1] %in% c(18:20)){
    TIMEPERIOD[[i]] <- 18}
  else if(data[i,1] %in% c(21:24, 00)){
    TIMEPERIOD[[i]] <- 21}
  else {TIMEPERIOD[[i]] <- 1}
}

data <- cbind(TIMEPERIOD, data[, 3:7])

#Extracting data for two routes
crashes_a <- subset(data, STREETNAME == "7 AVENUE")
crashes_b <- subset(data, STREETNAME == "BROADWAY")

#Group by to get counts of crash types by time period
library(plyr)
crashCount_a <- ddply(crashes_a, .(crashes_a$TIMEPERIOD, crashes_a$FATALITIES_INJURIES), nrow)
crashCount_b <- ddply(crashes_b, .(crashes_b$TIMEPERIOD, crashes_b$FATALITIES_INJURIES), nrow)
names(crashCount_a) <- c("Hour","F_I","Count")
names(crashCount_b) <- c("Hour","F_I","Count")

#Crash coefficient by crash type
crash_coef <- vector()
for(i in 1:nrow(crashCount_a)){
  if(crashCount_a[i,2] == 0)
    crash_coef[[i]] = crashCount_a[i,3]*0.20
  else
    crash_coef[[i]] = crashCount_a[i,3]*0.80
}
crashCount_a <- cbind(crashCount_a, crash_coef)
rm(i)
crash_coef <- vector()
for(i in 1:nrow(crashCount_b)){
  if(crashCount_b[i,2] == 0)
    crash_coef[[i]] = crashCount_b[i,3]*0.20
  else
    crash_coef[[i]] = crashCount_b[i,3]*0.80
}
crashCount_b <- cbind(crashCount_b, crash_coef)
rm(i)

#Crash coefficient by time period
crashCoef_a <- ddply(crashCount_a,.(crashCount_a$Hour), sum)
crashCoef_b <- ddply(crashCount_b,.(crashCount_b$Hour), sum)
colnames(crashCoef_a) <- c("Hour", "crashCoef")
colnames(crashCoef_b) <- c("Hour", "crashCoef")

#Merging crash and volume data
data_a <- merge(crashCoef_a, volCoef_a, by = "Hour")
data_b <- merge(crashCoef_b, volCoef_a, by = "Hour")

#Initial individual time period rsi values
values_a = values_b = vector()
for(i in 1:nrow(data_a)){
  values_a[[i]] <- (data_a[i,2]*as.numeric(data_a[i,3]))/100
}
for(i in 1:nrow(data_b)){
  values_b[[i]] <- (data_b[i,2]*as.numeric(data_b[i,3]))/100
}

#RSI by route
rsi_a <- sum(values_a)/10
rsi_b <- sum(values_b)/10