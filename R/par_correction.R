#### Correcting PAR/HOBO light files  ####

# Housekeeping
rm(list = ls())  #Clear the workspace
invisible (cat("\014"))  #Clear the console

#Load packages
library(here)
library(tidyverse)
library(lubridate)

#Set directories
files <- here("data/2_MayJune_deployment/field_deployment/cleanPAR")
eqn <- here("data/2_MayJune_deployment/calibration_tests")

output <- here("data/2_MayJune_deployment/field_deployment/correctedPAR")

#--------- SECTION 1: Read in raw Odyssey csv files and correct ----------

## Odyssey Loggers
par <- dir(files,pattern = "*PAR.CSV")
par_dat <- par %>%
  map(~ read_csv(file.path(files, .),skip = 9, col_names = c("scan_no","date","time","raw1","raw2"), col_types = "?????"))
names(par_dat) <-  c("BO", 'JL', 'NI_C', 'SA_C', 'TS')

## Equations
calib <- read_csv(file.path(eqn, "PAR_odyequations.csv"))

par(mfrow=c(2,3), mgp=c(1.5,0.5,0), mar=c(4,3,3,1)) 
for (j in 1:length(par_dat)) {

  site <- names(par_dat[j])
  
  print(site)
  
  par_dat[[j]]$odyPAR <- as.numeric(calib[calib$site == site,3])*par_dat[[j]]$raw1+as.numeric(calib[calib$site == site,4]) # use coefficients associated with matching serial number for conversion of raw data to PAR
  
  plot(par_dat[[j]]$odyPAR~par_dat[[j]]$scan_no, main = site, pch=19, ylab='PAR', xlab='time')
  
  print(par_dat[[j]])
  
  write_csv(par_dat[[j]], file.path(output, paste0(site,"_PARody_cor.csv")))
  
} # END j FOR LOOP 


#--------- SECTION 2: Read in raw HOBO csv files and correct ----------
## HOBO Loggers
hobo <- dir(files,pattern = "*_Light.csv")
hobo_dat <- hobo %>%
  map(~ read_csv(file.path(files, .),skip = 2, col_names = c("scan_no","datetime","intensity_lux"), col_types = "??_?"))

hobo_dat <- map(hobo_dat, ~ .x %>%
                 mutate(datetime=as.POSIXct(datetime, format = "%m/%d/%y %I:%M:%S %p")))

names(hobo_dat) <-  c("BO", 'JL', 'NI_C', 'SA_C', 'TS')

## Equations
calib <- read_csv(file.path(eqn, "PAR_hoboequations.csv"))

par(mfrow=c(2,3), mgp=c(1.5,0.5,0), mar=c(4,3,3,1)) 
for (j in 1:length(hobo_dat)) {
  
  site <- names(hobo_dat[j])

  # hobo_dat[[j]]$date <- format(hobo_dat[[j]]$datetime, format = "%m/%d/%y")
  # 
  # hobo_dat[[j]]$time <- format(as.POSIXlt(hobo_dat[[j]]$datetime, format='%H:%M:%S'), '%H:%M')
  
  # Hobo fitting as exponential decay:
  hob <- hobo_dat[[j]]$intensity_lux
  
  hobo_dat[[j]]$hoboPAR <-  calib$A1[calib$site== site]*exp(-hob/calib$t1[calib$site== site]) + calib$y0[calib$site== site]
  
  plot(hobo_dat[[j]]$hoboPAR~hobo_dat[[j]]$scan_no, main = site, pch=19, ylab='PAR', xlab='time')
  
  print(hobo_dat[[j]])
  
  write_csv(hobo_dat[[j]], file.path(output, paste0(site,"_PARhobo_cor.csv")))
  
} # END j FOR LOOP 



