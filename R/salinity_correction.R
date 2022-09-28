#### Correcting conductivity files and calculating salinity  ####

# Housekeeping
rm(list = ls())  #Clear the workspace
invisible (cat("\014"))  #Clear the console

#Load packages
library(here)
library(tidyverse)
library(lubridate)
library(wql)

#Set directories
files <- here("data/2_MayJune_deployment/field_deployment/cleanSalinity")# File path to my field salinity files
eqn <- here("data/2_MayJune_deployment/calibration_tests") # File path to conductivity equations

output <- here("data/2_MayJune_deployment/field_deployment/correctedSalinity")# File output folder

#####################################################################################
#--------- SECTION 1: Read in raw hobo csv files and correct ----------
#####################################################################################

## Raw conductance 
conductivity <- dir(files,pattern = "*Salinity.csv")
con_dat <- conductivity %>%
  map(~ read_csv(file.path(files, .),skip = 2, col_names = c("datetime",'conductivity', "temp"), col_types = "_???"))
names(con_dat) <-  c("BO", 'JL', 'NIC', 'SAC', 'TS')


con_dat <- map(con_dat, ~ .x %>%
                 mutate(datetime=as.POSIXct(datetime, format = "%m/%d/%y %I:%M:%S %p")))

hist(con_dat$NIC$conductivity)

## Equations
calib <- read_csv(file.path(eqn, "conductivity_hoboequations.csv"))

## Loop to correct each site tibble, make plots, and write out
par(mfrow=c(5,2), mgp=c(1.5,0.5,0), mar=c(4,3,3,1)) 
for (j in 1:length(con_dat)) {
  
  site <- names(con_dat[j])
  
  print(site)
  
  con_dat[[j]]$conductivity_cor <- as.numeric(calib[calib$site == site,3])*con_dat[[j]]$conductivity+as.numeric(calib[calib$site == site,4]) # use coefficients associated with matching serial number for correction
  
  plot(con_dat[[j]]$conductivity~con_dat[[j]]$datetime, main = site, pch=21, ylab='Conductivity', xlab='time', ylim=c(0, 60000))
  points(con_dat[[j]]$conductivity_cor~con_dat[[j]]$datetime, col = "blue", pch=21)

  
  con_dat[[j]]$salinity <- ec2pss(con_dat[[j]]$conductivity*0.001, con_dat[[j]]$temp, p = 0)# Salinity using the raw hobo conductivity value
  con_dat[[j]]$salinity_cor <- ec2pss(con_dat[[j]]$conductivity_cor*0.001, con_dat[[j]]$temp, p = 0) # Salinity using the corrected conductivity value
  
  plot(con_dat[[j]]$salinity~con_dat[[j]]$datetime, main = site, pch=21, ylab='Salinity', xlab='time', ylim=c(0, 40))
  points(con_dat[[j]]$salinity_cor~con_dat[[j]]$datetime, col = "blue", pch=21)
  
  print(con_dat[[j]])
  
  write_csv(con_dat[[j]], file.path(output, paste0(site,"_salinity_cor.csv")))
  
} # END j FOR LOOP 


## Checking things
hist(con_dat$TS$salinity_cor, xlab = 'Corrected Salinity', main = 'TS Logger', xlim = c(0, 36))
hist(con_dat$TS$salinity, xlab = 'Raw Salinity', main = 'TS Logger',xlim = c(0, 36))

boxplot(con_dat$NIC$salinity_cor)

