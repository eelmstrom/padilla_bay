#### Correcting oxygen sensor files  ####

# Housekeeping
rm(list = ls())  #Clear the workspace
invisible (cat("\014"))  #Clear the console

#Load packages
library(here)
library(tidyverse)
library(lubridate)

# #Set directories
# calib <- here("data/2_MayJune_deployment/calibration_tests/pre_deployment/clean")
# field <- here("data/2_MayJune_deployment/field_deployment/cleanO2")
# drift <- here("data/2_MayJune_deployment/calibration_tests/post_deployment/clean")

calib <- here("data/3_JulyAug_deployment/calibration_tests/pre_deployment/clean")
field <- here("data/3_JulyAug_deployment/field_deployment/cleanO2")
drift <- here("data/3_JulyAug_deployment/calibration_tests/post_deployment/clean")

#--------- SECTION 1A: PRE-DEPLOYMENT CALIBRATION EQUATIONS ----------

## READ YSI
ysi <- read_csv(file.path(calib, "july_tanktest_ysi.csv"),col_names = T)
ysi_dat <- ysi %>% select(c("Date (MM/DD/YYYY)","Time (HH:mm:ss)","ODO mg/L"))
colnames(ysi_dat) <- c("date", 'time',"ysi")

# // add 1 hour to YSI
ysi_dat$time <- format(as.POSIXlt(ysi_dat$time, format='%H:%M:%S') %m+% hours(1), format = "%H:%M:%S")
ysi_dat$date <- format(as.POSIXct(ysi_dat$date, format = "%m/%d/%Y"),"%m/%d/%y")
ysi_dat

## READ SENSORS
sensors <- dir(calib,pattern = "*oxygen.csv")
calib_dat <- sensors %>%
  map(~ read_csv(file.path(calib, .),skip = 2, col_names = c("scan_no","datetime","do_mgL",'temp'), col_types = "????"))

#// fix dates
calib_dat <- map(calib_dat, ~ .x %>%
                  mutate(datetime=as.POSIXct(datetime, format = "%m/%d/%y %I:%M:%S %p"),
                         date = format(datetime, format = "%m/%d/%y"),
                         time = format(datetime, format = "%H:%M:%S")))

names(calib_dat) <-  c("BO", 'JL', 'NI_C', 'SA_C', 'TS')
calib_dat

## PLOT AND CREATE EQUATIONS VS YSI 

calib_eqns <- matrix(nrow=5, ncol=5) # create a table to fill in with equation values for each Hobo SN
colnames(calib_eqns) <- c('site','Intercept','Slope','Rsquared',"Offset")
par(mfrow=c(2,3), mgp=c(1.5,0.5,0), mar=c(4,3,3,1)) 
for(i in 1:length(calib_dat)){
  
  site <- names(calib_dat[i])
  df_list <- list(calib_dat[[i]], ysi_dat)# pull site specific df
  eqn_data <- df_list %>% reduce(inner_join, by=c('date', 'time'))%>%
    slice(-(1:3))%>%slice(1:(n() - 3)) 
  
  plot(eqn_data$ysi~eqn_data$do_mgL, main = site, pch=19, ylab='ysi', xlab='sensorDO')
  eqn <- lm(eqn_data$ysi ~ eqn_data$do_mgL); abline(eqn, col=2, lty=2) # plots best fit line
  legend('topleft', legend = c(paste('DO = ',round(coef(eqn)[2],4),'*sensor + ',round(coef(eqn)[1],4), sep=''), paste('R^2 =',round(summary(eqn)$adj.r.squared,2))), bty='n', text.col=4)
  eqn_data$offset <- eqn_data$ysi-eqn_data$do_mgL
  
  ##Write equations and avg offset to matrix
  calib_eqns[i,1 ] <- site
  calib_eqns[i,2:3] <- coef(eqn)
  calib_eqns[i,4] <- summary(eqn)$adj.r.squared
  calib_eqns[i,5] <- mean(eqn_data$offset)
  
}


#--------- SECTION 1B: POST DEPLOYMENT DRIFT CALCULATION ----------

## READ YSI
ysi <- read_csv(file.path(drift, "aug_drifttest_ysi.csv"), col_names = T)
ysi_drift <- ysi %>% select(c("Date (MM/DD/YYYY)","Time (HH:mm:ss)","ODO mg/L"))
colnames(ysi_drift) <- c("date", 'time',"ysi")

# // add 1 hour to YSI
ysi_drift$time <- format(as.POSIXlt(ysi_drift$time, format='%H:%M:%S') %m+% hours(1), format = "%H:%M:%S")
ysi_drift$date <- format(as.POSIXct(ysi_drift$date, format = "%m/%d/%Y"),"%m/%d/%y")
ysi_drift

## READ SENSORS
sensors <- dir(drift,pattern = "*oxygen.csv")
drift_dat <- sensors %>%
  map(~ read_csv(file.path(drift, .),skip = 2, col_names = c("scan_no","datetime","do_mgL",'temp'), col_types = "????"))

#// fix dates
drift_dat <- map(drift_dat, ~ .x %>%
                   mutate(datetime=as.POSIXct(datetime, format = "%m/%d/%y %I:%M:%S %p"),
                          date = format(datetime, format = "%m/%d/%y"),
                          time = format(datetime, format = "%H:%M:%S")))

names(drift_dat) <-  c("BO", 'JL', 'NI_C', 'SA_C', 'TS')
drift_dat

## PLOT AND CREATE EQUATIONS VS YSI 

## Equations from above needed to correct sensor files 
calib_eqns <- as_tibble(calib_eqns)

drift_eqns <- matrix(nrow=5, ncol=5) # create a table to fill in with equations
colnames(drift_eqns) <- c('site','Intercept','Slope','Rsquared',"Offset")
par(mfrow=c(2,3), mgp=c(1.5,0.5,0), mar=c(4,3,3,1)) 
for(j in 1:length(drift_dat)){
  
  site <- names(drift_dat[j])#Apply tank test correction to sensors
  drift_dat[[j]]$do_mgL <- as.numeric(calib_eqns[calib_eqns$site == site,3])*drift_dat[[j]]$do_mgL+as.numeric(calib_eqns[calib_eqns$site == site,2]) 
  
  df_list <- list(drift_dat[[j]], ysi_drift)# pull site specific df
  deqn_data <- df_list %>% reduce(inner_join, by=c('date', 'time'))%>%
     slice(-(1:3))%>%slice(1:(n() - 3)) # merge to ysi
   
   plot(deqn_data$ysi~deqn_data$do_mgL, main = site, pch=19, ylab='ysi', 
        xlab='sensorDOcor', ylim=c(8, 10), xlim=c(8,10))
   deqn <- lm(deqn_data$ysi ~ deqn_data$do_mgL); abline(deqn, col=3, lty=2)
   curve(as.numeric(calib_eqns[j,2])+as.numeric(calib_eqns[j,3])*x,add=T, col=2)
   legend('topleft', legend = c(paste('DO = ',round(coef(deqn)[2],4),'*sensor + ',round(coef(deqn)[1],4), sep=''), paste('R^2 =',round(summary(deqn)$adj.r.squared,2))), bty='n', text.col=4)
   deqn_data$offset <- deqn_data$ysi-deqn_data$do_mgL
   
   ##Write equations and avg offset to matrix
   drift_eqns[j,1 ] <- site
   drift_eqns[j,2:3] <- coef(deqn)
   drift_eqns[j,4] <- summary(deqn)$adj.r.squared
   drift_eqns[j,5] <- mean(deqn_data$offset)
   
   deqn_data$do_cor2 <- coef(deqn)["deqn_data$do_mgL"]*deqn_data$do_mgL+coef(deqn)['(Intercept)']
   print(head(deqn_data))
}

#filter(calib_eqns, site==site)[['Slope']]

drift_eqns2 <- as.data.frame(drift_eqns)
calib_eqns## Doesn't look like much drift?

as.numeric(calib_eqns$Intercept)-as.numeric(drift_eqns2$Intercept)
#--------- SECTION 1C: TESTING SLOPES OF CALIBRATION AND DRIFT TESTS ----------

#df_list <- list()
names <- c("BO", 'JL', 'NIC', 'SAC', 'TS')
mod_summaries <- list()
for(i in 1:length(names)){
  df_list <- list(calib_dat[[i]], ysi_dat)# pull site specific df
  
  trial1 <- df_list %>% reduce(inner_join, by=c('date', 'time'))
  trial1$trial <- "A"
  
  df_list2 <- list(drift_dat[[i]], ysi_drift)
  
  trial2 <- df_list2 %>% reduce(inner_join, by =c("date", "time"))
  trial2$trial <- "B"
  
  dat <- rbind(trial1, trial2)
  
  test <- lm(ysi~do_mgL*trial, data=dat)
  mod_summaries[[i]] <-summary(test)
  
}
names(mod_summaries) <- names

mod_summaries$TS

#--------- SECTION 2: APPLY CALIBRATION EQUATIONS TO DATA ----------

## READ SENSORS
sensors <- dir(field,pattern = "*Oxygen.csv")
field_dat <- sensors %>%
  map(~ read_csv(file.path(field, .),skip = 2, col_names = c("scan_no","datetime","do_mgL",'temp'), col_types = "????"))

#// fix dates
field_dat <- map(field_dat, ~ .x %>%
                   mutate(datetime=as.POSIXct(datetime, format = "%m/%d/%y %I:%M:%S %p"),
                          date = format(datetime, format = "%m/%d/%y"),
                          time = format(datetime, format = "%H:%M:%S")))

names(field_dat) <-  c("BO", 'JL','NI_C','SA_C','TS')

corrected <- here("data/3_JulyAug_deployment/field_deployment/correctedO2")

par(mfrow=c(2,3), mgp=c(1.5,0.5,0), mar=c(4,3,3,1)) 
plot_list <- list()
for (j in 1:length(field_dat)) {
  
  site <- names(field_dat[j])#Apply tank test correction to sensors
  field_dat[[j]]$do_cor <- as.numeric(calib_eqns[calib_eqns$site == site,3])*field_dat[[j]]$do_mgL+as.numeric(calib_eqns[calib_eqns$site == site,2]) 
  
  print(site)
  
 # use coefficients associated with matching serial number for conversion of raw data to PAR
  
  plot(field_dat[[j]]$do_cor~field_dat[[j]]$scan_no, main = site, pch=19,cex=0.1, ylab='DO', xlab='time',
       ylim=c(0, 25))
  
  print(field_dat[[j]])
  
  #write_csv(field_dat[[j]], paste('ConvertedPAR/cal_',loggerfile, sep=''), row.names=FALSE) # export to the output folder
  write_csv(field_dat[[j]], file.path(corrected, paste0(site,"_O2_cor.csv")))
}

