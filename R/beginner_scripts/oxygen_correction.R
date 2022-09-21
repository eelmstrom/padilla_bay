#### Script calculate tank test offset####

# Housekeeping
rm(list = ls())  #Clear the workspace
invisible (cat("\014"))  #Clear the console

#Load packages
library(here)
library(tidyverse)
library(lubridate)
library(hms)

##############PRE DEPLOYMENT TANK TEST FIGURE AND EQUATIONS#####################
#Set directories
ysi <- here("data/2_2021_MayJune_deployment/tank_tests/pre_deployment/clean")
files <- here("data/2_2021_MayJune_deployment/tank_tests/pre_deployment/clean/sensors")

#############READ YSI
ysi <- read_csv(file.path(ysi, "tanktest_ysi.csv"), skip=9)
ysi_dat <- ysi %>% select(c("Date (MM/DD/YYYY)","Time (HH:mm:ss)","ODO mg/L"))
colnames(ysi_dat) <- c("date", 'time',"ysi")

# // add 1 hour to YSI
ysi_dat$time <- format(as.POSIXlt(ysi_dat$time, format='%H:%M:%S') %m+% hours(1), format = "%H:%M:%S")
ysi_dat

##### READ IN SENSORS, GET EQUATIONS VS YSI 
hoboeqns <- matrix(nrow=6, ncol=6) # create a table to fill in with equation values for each Hobo SN
colnames(hoboeqns) <- c('SerialNo','Intercept','Slope','Rsquared',"Offset", "STD")

par(mfrow=c(2,3), mgp=c(1.5,0.5,0), mar=c(4,3,3,1)) # set up multipanel plotting window; 
HFiles <- dir(files, pattern = "*.csv") # create list of all txt files in the directory
for(i in 1:length(HFiles)){
  ##sensor SN label
  res <- str_match(HFiles, "_\\s*(.*?)\\s*.csv")
  hoboSN <- res[,2]
  ##grab the data 
  hobo <- read_csv(file.path(files, HFiles[i]))
  hobo$time <- format(hobo$time, format = "%H:%M:%S")
  hobo$date <- format(hobo$date, format = "%m/%d/%y")
  #merge to ysi
  do_dat <- merge(ysi_dat, hobo, by=c('date','time'))
  do_dat <- do_dat[c(-1,-2,-86,-87),]
  do_dat$offset <- do_dat$ysi-do_dat$do_mgL
  ##Plot with equation & R^2
  plot(do_dat$ysi ~ do_dat$do_mgL, main = hoboSN[i], pch=19, ylab='ysi DO', xlab='sensor DO')
  hoboeqn <- lm(do_dat$ysi ~ do_dat$do_mgL)
  abline(hoboeqn, col=2, lty=2) # plots best fit line
  legend('topleft', legend = c(paste('DO = ',round(coef(hoboeqn)[2],4),'*sensor + ',round(coef(hoboeqn)[1],4), sep=''), paste('R^2 =',round(summary(hoboeqn)$adj.r.squared,2))), bty='n', text.col=4)
  ##Write equations and avg offset to matrix
  hoboeqns[i,1 ] <- hoboSN[i]
  hoboeqns[i,2:3] <- coef(hoboeqn)
  hoboeqns[i,4] <- summary(hoboeqn)$adj.r.squared
  hoboeqns[i,5] <- mean(do_dat$offset)
  hoboeqns[i,6] <- sd(do_dat$offset)
}
hoboeqns

##############POST DEPLOYMENT TANK TEST FIGURE AND EQUATIONS#####################
#Set directories
ysi2 <- here("data/2_2021_MayJune_deployment/tank_tests/post_deployment/clean")
files2 <- here("data/2_2021_MayJune_deployment/tank_tests/post_deployment/clean/sensors")

#############READ YSI
ysi_post <- read_csv(file.path(ysi2, "drifttest_ysi.csv"), skip=9)
ysi_post <- ysi_post %>% select(c("Date (MM/DD/YYYY)","Time (HH:mm:ss)","ODO mg/L"))
colnames(ysi_post) <- c("date", 'time',"ysi")

# // add 1 hour to YSI
ysi_post$time <- format(as.POSIXlt(ysi_post$time, format='%H:%M:%S') %m+% hours(1), format = "%H:%M:%S")
ysi_post

##### READ IN SENSORS, GET EQUATIONS VS YSI 
post_eqns <- matrix(nrow=5, ncol=6) # create a table to fill in with equation values for each Hobo SN
colnames(post_eqns) <- c('SerialNo','Intercept','Slope','Rsquared',"Offset","STD")

par(mfrow=c(2,3), mgp=c(1.5,0.5,0), mar=c(4,3,3,1)) # set up multipanel plotting window; 
HFiles <- dir(files2, pattern = "*.csv") # create list of all txt files in the directory
for(i in 1:length(HFiles)){
  ##sensor SN label
  res <- str_match(HFiles, "_\\s*(.*?)\\s*.csv")
  hoboSN <- res[,2]
  ##grab the data 
  hobo <- read_csv(file.path(files2, HFiles[i]))
  tail(hobo)
  hobo$time <- format(hobo$time, format = "%H:%M:%S")
  hobo$date <- format(hobo$date, format = "%m/%d/%y")
  #merge to ysi
  do_dat <- merge(ysi_post, hobo, by=c('date','time'))
  tail(do_dat)
  do_dat <- do_dat[c(-1,-2,-93,-94,-95),]
  do_dat$offset <- do_dat$ysi-do_dat$do_mgL
  ##Plot with equation & R^2
  plot(do_dat$ysi ~ do_dat$do_mgL, main = hoboSN[i], pch=19, ylab='ysi DO', xlab='sensor DO')
  hoboeqn <- lm(do_dat$ysi ~ do_dat$do_mgL)
  abline(hoboeqn, col=2, lty=2) # plots best fit line
  legend('topleft', legend = c(paste('DO = ',round(coef(hoboeqn)[2],4),'*sensor + ',round(coef(hoboeqn)[1],4), sep=''), paste('R^2 =',round(summary(hoboeqn)$adj.r.squared,2))), bty='n', text.col=4)
  ##Write equations and avg offset to matrix
  post_eqns[i,1 ] <- hoboSN[i]
  post_eqns[i,2:3] <- coef(hoboeqn)
  post_eqns[i,4] <- summary(hoboeqn)$adj.r.squared
  post_eqns[i,5] <- mean(do_dat$offset)
  post_eqns[i,6] <- sd(do_dat$offset)
}

## Compare pre and post

(hoboeqns2 <- as_tibble(hoboeqns[-1,]))
(post_eqns <- as_tibble(post_eqns))
## post offset is less, suggesting sensors drifted up in value.
## hobos don't need to be corrected

## use post equations to correct raw files

test <- cbind(hoboeqns2$Offset, post_eqns$Offset)

#--------- SECTION 2: APPLY EQUATIONS ----------
## CONVERT FIELD DEPLOYMENT of ODY LOGGERS TO PAR UNITS 

calib <- read.csv('PARequations_Field20210903.csv') # import data table of equation coefficients to be used for PAR conversion

setwd("Z:/LiCor/FieldLoggers") # set directory with raw logger files to be converted to PAR
dir.create("Z:/LiCor/FieldLoggers/ConvertedPAR") # create output folder for converted files within the working directory

Files <- Sys.glob("*.CSV") # all csv files in the working directory will be converted

for (j in 1:length(Files))  # begin j FOR LOOP
{
  loggerfile <- Files[j]
  
  data <- read.csv(loggerfile, skip=9, header=FALSE, strip.white=TRUE, col.names=c('ScanNo','Date','Time','RAW1','RAW2')) # import logger data to be converted to PAR
  
  dataSN <- as.numeric(as.character(read.csv(loggerfile, header=FALSE)[4,2])) # extract serial number of logger
  
  print(dataSN)
  
  data$PAR <- calib[calib$SerialNo == dataSN,3]*data$RAW1+calib[calib$SerialNo == dataSN,4] # use coefficients associated with matching serial number for conversion of raw data to PAR
  
  write.csv(data, paste('ConvertedPAR/cal_',loggerfile, sep=''), row.names=FALSE) # export to the output folder
  
} # END j FOR LOOP 

  


