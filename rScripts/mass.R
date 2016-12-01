#Loading Massachusetts fatal acccidents data for 2014-2015
data1 <- read.csv("capstone_proj/FileName1.csv")
data2 <- read.csv("capstone_proj/FileName2.csv")

#Cleaning data to remove unknown values
data <- rbind(data1,data2)
data <- data[!data$acchr == 99,]

#Grouping accidents by time groups 
timegroup <- vector()
for(i in 1:nrow(data)){
  if(data[i, 3] == 7 | data[i, 3] == 8 | data[i, 3] == 9 | data[i, 3] == 10 | data[i, 3] == 11){
    timegroup[[i]] = 7
  }
  else if (data[i, 3] == 12 | data[i, 3] == 13 | data[i, 3] == 14){
    timegroup[[i]] = 11
  }
  else if (data[i, 3] == 15 | data[i, 3] == 16){
    timegroup[[i]] <- 14
  }
  else if (data[i, 3] == 17 | data[i, 3] == 18 | data[i, 3] == 19 | data[i, 3] == 20 | data[i, 3] == 21){
    timegroup[[i]] <- 16
  }
  else if (data[i, 3] == 22 | data[i, 3] == 23 | data[i, 3] == 0){
    timegroup[[i]] = 21
  }
  else {timegroup[[i]] = 1}
}

#Assigning each accidents its corresponding time group
data <- cbind(data, timegroup)

#Assigning some points to two arbitrary routes from data
r1 <- rbind(data[1:10,], data[18:19,], data[30:31,])
r2 <- rbind(data[1:3,], data[15:19,], data[21:23,])
r1 <- r1[c(4, 5, 8)]
r2 <- r2[c(4, 5, 8)]

#Finding RSI for each route during time group 16
RSI1_16 <- nrow(r1[r1$timegroup == 16,])/nrow(r1)
RSI2_16 <- nrow(r2[r2$timegroup == 16,])/nrow(r2)

RSI1_16
RSI2_16
