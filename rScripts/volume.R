#Loading volume data
volume <- read.csv("capstone_proj/Volume.csv")

#Grouping hourly volume in timeperiods
tbins <- vector("list", 7)
for(i in 1:nrow(volume)){
  tbins[[1]][i] <- sum(volume[i, 15:17])
  tbins[[2]][i] <- sum(volume[i, 18:21])
  tbins[[3]][i] <- sum(volume[i, 22:23])
  tbins[[4]][i] <- sum(volume[i, 24:25])
  tbins[[5]][i] <- sum(volume[i, 26:28])
  tbins[[6]][i] <- sum(volume[i, 29:31], volume[i, 8])
  tbins[[7]][i] <- sum(volume[i, 9:14])
}
rm(i)

tbins <- as.data.frame(tbins)
cnames <- c(7,10,14,16,18,21,1)
names(tbins) <- cnames
rm(cnames)

tbins <- cbind(volume[,c(3)], tbins)
colnames(tbins)[1] <- "STREETNAME"

#Extracting two routes
route_a <- subset(tbins, STREETNAME == "7 AVENUE")
route_b <- subset(tbins, STREETNAME == "BROADWAY")

#Normalizing data
for(i in 1:nrow(route_a)){
  for(j in 2:8){
    route_a[i, j] <- (route_a[i, j] - min(route_a[, j])) / (max(route_a[,j]) - min(route_a[,j]))
  }
}

for(i in 1:nrow(route_b)){
  for(j in 2:8){
    route_b[i, j] <- (route_b[i, j] - min(route_b[, j])) / (max(route_b[,j]) - min(route_b[,j]))
  }
}
rm(i,j)

#Principal component analysis to get volume coeffiecient by timeperiod
pca_a <- princomp(route_a[, 2:8])
pca_b <- princomp(route_b[, 2:8])

coef_rta <- abs(pca_a$loadings[,1])
coef_rtb <- abs(pca_b$loadings[,1])

#Volume coefficients by time period
volCoef_a <- as.data.frame(cbind(coef_rta, names(coef_rta)))
volCoef_b <- as.data.frame(cbind(coef_rtb, names(coef_rtb)))
colnames(volCoef_a) <- c("volCoef", "Hour")
colnames(volCoef_b) <- c("volCoef", "Hour")