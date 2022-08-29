#### Correcting PAR/HOBO light files  ####

#Load packages
library(here)
library(tidyverse)
library(lubridate)

#Set directories
files <- here("data/3_2021_JulyAug_deployment/")
test <- here("data/2_2021_MayJune_deployment/calibration_tests/par")

#--------- SECTION 1: Read in raw Odyssey csv files and correct ----------

## Odyssey Loggers
par <- dir(files,pattern = "*PAR.CSV")
par_dat <- par %>%
  map(~ read_csv(file.path(files, .),skip = 9, col_names = c("scan_no","date","time","raw1","raw2"), col_types = "?????"))

par_dat <- map(par_dat, ~ .x %>%
                  mutate(date=as.POSIXct(date, format = "%d/%m/%Y"),
                         time=as.POSIXct(time, format = "%H:%M:%S")))

names(par_dat) <-  c("BO", 'JL', 'NI_C', 'SA_C', 'TS')

## Equations
calib <- read_csv(file.path(test, "PAR_odyequations.csv"))

par(mfrow=c(2,3), mgp=c(1.5,0.5,0), mar=c(4,3,3,1)) 
for (j in 1:length(par_dat)) {
  
  #par_dat[[j]]$time <- format(par_dat[[j]]$time, format = "%H:%M:%S")
  par_dat[[j]]$date <- format(par_dat[[j]]$date, format = "%m/%d/%y")
  
  par_dat[[j]]$time <- format(as.POSIXlt(par_dat[[j]]$time, format='%H:%M:%S'), '%H:%M')
  
  site <- names(par_dat[j])
  
  print(site)
  
  par_dat[[j]]$PAR <- as.numeric(calib[calib$site == site,3])*par_dat[[j]]$raw1+as.numeric(calib[calib$site == site,4]) # use coefficients associated with matching serial number for conversion of raw data to PAR
  
  plot(par_dat[[j]]$PAR~par_dat[[j]]$scan_no, main = site, pch=19, ylab='PAR', xlab='time')
  
  print(par_dat[[j]])

  #write.csv(data, paste('ConvertedPAR/cal_',loggerfile, sep=''), row.names=FALSE) # export to the output folder
  
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
calib <- read_csv(file.path(test, "PAR_hoboequations.csv"))

par(mfrow=c(2,3), mgp=c(1.5,0.5,0), mar=c(4,3,3,1)) 
for (j in 1:length(hobo_dat)) {
  
  site <- names(hobo_dat[j])
  
  print(site)
  
  #hobo_dat[[j]]$time <- format(hobo_dat[[j]]$datetime, format = "%H:%M:%S")
  hobo_dat[[j]]$date <- format(hobo_dat[[j]]$datetime, format = "%m/%d/%y")
  
  hobo_dat[[j]]$time <- format(as.POSIXlt(hobo_dat[[j]]$datetime, format='%H:%M:%S'), '%H:%M')
  
  # Hobo fitting as exponential decay:
  hob <- hobo_dat[[j]]$intensity_lux
  
  hobo_dat[[j]]$PAR <-  calib$A1[calib$site== site]*exp(-hob/calib$t1[calib$site== site]) + calib$y0[calib$site== site]
  
  plot(hobo_dat[[j]]$PAR~hobo_dat[[j]]$scan_no, main = site, pch=19, ylab='PAR', xlab='time')
  
  print(hobo_dat[[j]])
  
  #write.csv(data, paste('ConvertedPAR/cal_',loggerfile, sep=''), row.names=FALSE) # export to the output folder
  
} # END j FOR LOOP 

test1 <- par_dat$TS
test2 <- hobo_dat$TS

unnest_df <- unnest(
  par_dat , cols = c('date','time','PAR'))
str(unnest_df)

#df_list2 <- list()
par(mfrow=c(2,3), mgp=c(1.5,0.5,0), mar=c(4,3,3,1)) 
names <- c("BO", 'JL', 'NI_C', 'SA_C', 'TS')
for(i in 1:length(names)){
  
  df_list <- list(par_dat[[i]], hobo_dat[[i]])# pull site specific df
  
  df_list2 <- df_list %>% reduce(full_join, by=c('date', 'time')) #merge
  
  plot(df_list2$PAR.x~df_list2$scan_no.x, main = names[i], pch=21, ylab='hobo_par', xlab='time')
  points(df_list2$PAR.y~df_list2$scan_no.x, col = 'blue')
  
}
df_list2
