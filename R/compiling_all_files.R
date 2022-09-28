#### Combining HOBO files to one datasheet ####

# Housekeeping
rm(list = ls())  #Clear the workspace
invisible (cat("\014"))  #Clear the console

#Load packages
library(here)
library(tidyverse)
library(lubridate)
library(strings)

#Set directories
files <- here("data/2_MayJune_deployment/field_deployment/raw")
ofiles <- here('data/2_MayJune_deployment/field_deployment/correctedO2')
parfiles <- here('data/2_MayJune_deployment/field_deployment/correctedPAR')
salfiles <- here('data/2_MayJune_deployment/field_deployment/correctedSalinity')

####################################################################################
#--------- SECTION 1A: Read in  HOBO csv files (Depth, Salinity, Temp) ----------
#####################################################################################

## Depth
depth <- dir(files,pattern = "*Depth_0.csv")
depth_dat <- depth %>%
  map(~ read_csv(file.path(files, .),skip = 2, col_names = c("datetime","depth"), col_types = "_?__?"))
names(depth_dat) <- depth

## Temp 
temp <- dir(files,pattern = "*Temp_0.csv")
temp_dat <- temp %>%
  map(~ read_csv(file.path(files, .),skip = 2, col_names = c("datetime","Temperature"), col_types = "_??"))
names(temp_dat) <- temp

## Salinity (corrected)
salinity <- dir(salfiles,pattern = "*salinity_cor.csv")
sal_dat <- salinity %>%
  map(~ read_csv(file.path(salfiles, .),skip = 2, col_names = c("datetime", "Salinity"), col_types = "?____?"))
names(sal_dat) <- salinity

## Change dates
temp_dat <- map(temp_dat, ~ .x %>%
                  mutate(datetime=as.POSIXct(datetime, format = "%m/%d/%y %I:%M:%S %p")))

depth_dat <- map(depth_dat, ~ .x %>%
                   mutate(datetime=as.POSIXct(datetime, format = "%m/%d/%y %I:%M:%S %p")))

sal_dat <- map(sal_dat, ~ .x %>%
                  mutate(datetime=as.POSIXct(datetime, format = "%m/%d/%y %I:%M:%S %p")))


####################################################################################
#--------- SECTION 1B: Read in corrected oxygen data (see O2 correction script) ----------
####################################################################################

# Oxygen data has been corrected for sensor drift. Still need to correct for salinity.
## Oxygen
oxygen <- dir(ofiles,pattern = "*O2_cor.csv")
o_dat <- oxygen %>%
  map(~ read_csv(file.path(ofiles, .),skip = 1, col_names = c("datetime", 'date', 'time', "do_cor"), , col_types = "_?__???"))
names(o_dat) <- oxygen

# o_dat <- map(o_dat, ~ .x %>%
#                  mutate(datetime=as.POSIXct(datetime, format = "%m/%d/%y %I:%M:%S %p")))

## Fix date time

o_dat <- map(o_dat, ~ .x %>%
                     mutate(date = as.Date(date, format = "%m/%d/%y"), # years function not working
                            time = format(time, format = '%H:%M:%S'))%>%
                     mutate(datetime = with(., ymd(date)+hms(time)))%>%
                     select(., -c(date, time)))

o_dat$TS

####################################################################################
#--------- SECTION 1C: Read in corrected PAR data (see PAR correction script) ----------
####################################################################################

# PAR has been calibrated to the LICOR 


## Odyssey PAR
ody_par <- dir(parfiles,pattern = "*PARody_cor.csv")
ody_par_dat <- ody_par %>%
  map(~ read_csv(file.path(parfiles, .), skip = 1, col_names = c("date", 'time', "odyPAR"), col_types = "_??__?"))
names(ody_par_dat) <- ody_par

ody_par_dat <- map(ody_par_dat, ~ .x %>%
                   mutate(date = as.Date(date, format = "%d/%m/%y")+days(365), # years function not working
                          time = format(time, format = '%H:%M:%S'))%>%
                   mutate(datetime = with(., ymd(date)+hms(time)))%>%
                   select(., -c(date, time)))

## Hobo par
hobo_par <- dir(parfiles,pattern = "*PARhobo_cor.csv")
hobo_par_dat <- hobo_par %>%
  map(~ read_csv(file.path(parfiles, .), skip = 1, col_names = c("datetime", "hoboPAR"), col_types = "_?_?"))
hobo_par_dat
names(hobo_par_dat) <- hobo_par



####################################################################################
#--------- SECTION 2: Reduce files to one csv per site and read out ----------
####################################################################################


## Merge datasets to one per site
names(depth_dat) <- names(temp_dat) <- names(sal_dat) <- names(o_dat) <- names(ody_par_dat)<- c("BO", 'JL', 'NIC', 'SAC', 'TS')

test <- here("data/2_MayJune_deployment/metabolism_csvs")

df_list2 <- list()
dftest <- list()
names <- c("BO", 'JL', 'NIC', 'SAC', 'TS')
par(mfrow=c(2,3), mgp=c(1.5,0.5,0), mar=c(4,3,3,1)) 
for(i in 1:length(names)){
  df_list <- list(depth_dat[[i]], temp_dat[[i]], sal_dat[[i]], o_dat[[i]], ody_par_dat[[i]])# pull site specific df
  
  df_list2[[i]] <- df_list %>% reduce(full_join, by='datetime') %>%#merge
    filter(datetime > as.POSIXct("2021-05-29 00:00:00", tz="UTC"))%>%
    filter(datetime < as.POSIXct("2021-06-22 00:00:00", tz="UTC"))
  
  dftest[[i]] <- DO_salinity_correction(df_list2[[i]], method = "garcia-gordon")
  dftest[[i]]$do_corF_s <- dftest[[i]]$do_cor- dftest[[i]]$F_s
  
  plot(dftest[[i]]$do_corF_s~dftest[[i]]$datetime, main = names[i], ylim=c(0, 25),
       ylab = "Corrected oxygen mg/L", xlab='datetime')
  #points(dftest[[i]]$depth~dftest[[i]]$datetime, col ='blue')
  
  print(min(dftest[[i]]$depth))
  
  print(dftest[[i]])
  
  write_csv(dftest[[i]], file.path(test, paste0(names[i],"_metab_data.csv"))) # write
}




par(mfrow=c(2,3), mgp=c(1.5,0.5,0), mar=c(4,3,3,1)) 
for(i in 1:length(names)){
  
  plot(dftest[[i]]$depth~dftest[[i]]$datetime, main = names[i], ylim=c(0, 4),
       ylab = "Depth", xlab='datetime', col = 'blue', type ='l')
}


BO <- df_list2[[1]]

colnames(BO)[3:4]<- c("Temperature", "Salinity")


BO_cor <- DO_salinity_correction(BO, method = "garcia-gordon")

ts <- dftest[[5]]

plot(F_s ~ datetime, ts)

# Tidy version (aka long version so you can group_by)
long_dat <- dat %>%
  mutate(scanNo = row_number())%>%
  pivot_longer(starts_with("SN_"),
               names_to = "SerialNo",
               names_prefix = "SN_")

long_dat  

tail(df_list2)

plot(salinity_cor~datetime, df_list2[[5]])
