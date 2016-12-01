#Loading crashes data for NYC 2014-2015
crashes <- read.csv("capstone_proj/NYPD_Motor_Vehicle_Collisions.csv")

#Identiying if crash involved has injuries/ fatalities or none.
for(i in 1:nrow(crashes)){
  crashes[i,10] <- sum(crashes[i, 10:11])
}

#Grouping crashes with injuries/fatalities and crashes without injuries/ fatalities
injury_killed <- vector()
for(i in 1:nrow(crashes)){
  if(crashes[i,10] == 0) injury_killed[[i]] = 0
  else injury_killed[[i]] = 1
}
rm(i)

accidents_data <- cbind(crashes[, 1:10], injury_killed, crashes[, c(14, 17)])
accidents_data <- accidents_data[!accidents_data$VEHICLE.TYPE.CODE.1 == "",]

#Logitic regression on fatality/injury depending on vehicle type and reason for crash individually
vehicle_log <- glm(accidents_data$injury_killed~accidents_data$VEHICLE.TYPE.CODE.1, family = "binomial")
reason_log <- glm(accidents_data$injury_killed~accidents_data$CONTRIBUTING.FACTOR.VEHICLE.1, family = "binomial")

#Logistic regression with both independent variables together
logistic_model <- glm(injury_killed~CONTRIBUTING.FACTOR.VEHICLE.1+VEHICLE.TYPE.CODE.1, data = accidents_data, family = "binomial")
