#### Combining HOBO files to one datasheet ####

# Housekeeping
rm(list = ls())  #Clear the workspace
invisible (cat("\014"))  #Clear the console

#Load packages
library(here)
library(tidyverse)
library(lubridate)

#Set directories
files <- here("data/2_MayJune_deployment/field_deployment/raw")
ofiles <- here('data/2_MayJune_deployment/field_deployment/correctedO2')
parfiles <- here('data/2_MayJune_deployment/field_deployment/correctedPAR')

####################################################################################
#--------- SECTION 1A: Read in raw HOBO csv files (Depth, Salinity, Temp) ----------
#####################################################################################

## Depth
depth <- dir(files,pattern = "*Depth_0.csv")
depth_dat <- depth %>%
  map(~ read_csv(file.path(files, .),skip = 2, col_names = c("datetime","depth"), col_types = "_?__?"))
names(depth_dat) <- depth

## Temp 
temp <- dir(files,pattern = "*Temp_0.csv")
temp_dat <- temp %>%
  map(~ read_csv(file.path(files, .),skip = 2, col_names = c("datetime","temp"), col_types = "_??"))
names(temp_dat) <- temp

## Salinity
salinity <- dir(files,pattern = "*Salinity_0.csv")
sal_dat <- salinity %>%
  map(~ read_csv(file.path(files, .),skip = 2, col_names = c("datetime", "salinity"), col_types = "_?_?"))
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
                   mutate(date = as.Date(date, format = "%d/%m/%y")+years(1), 
                          time = format(time, format = '%H:%M:%S'))%>%
                   mutate(datetime = with(., ymd(date)+hms(time)))%>%
                   select(., -c(date, time)))

## Hobo par
hobo_par <- dir(parfiles,pattern = "*PARhobo_cor.csv")
hobo_par_dat <- hobo_par %>%
  map(~ read_csv(file.path(parfiles, .), skip = 1, col_names = c("datetime", "hoboPAR"), col_types = "_?_?"))
hobo_par_dat
names(hobo_par_dat) <- hobo_par


#--------- SECTION 2: Merge all datasets per site and clip to study dates ----------
## Merge datasets to one per site
names(depth_dat) <- names(temp_dat) <- names(sal_dat) <- names(o_dat) <- names(ody_par_dat)<- c("BO", 'JL', 'NIC', 'SAC', 'TS')

test <- files <- here("data/2_2021_MayJune_deployment/field_deployment/test")

#df_list <- list()
names <- c("BO", 'JL', 'NIC', 'SAC', 'TS')
for(i in 1:length(names)){
  df_list <- list(depth_dat[[i]], temp_dat[[i]], sal_dat[[i]], o_dat[[i]], ody_par_dat[[i]])# pull site specific df
  
  df_list2 <- df_list %>% reduce(full_join, by='datetime') #merge
  
  #write_csv(df_list2, file.path(test, paste0(names[i],"_metab_data.csv"))) # write
}

df_list2

sal_dat_TS <- sal_dat$TS_Salinity_0.csv
sal_dat_SA <- sal_dat$SA_C_Salinity_0.csv
sal_dat_BO <- sal_dat$BO_Salinity_0.csv
sal_dat_NI <- sal_dat$NI_C_Salinity_0.csv

sal_dat2 <- sal_dat %>% reduce(full_join, by='datetime')

plot(sal_dat_TS, datetime~salinity)

plot(sal_dat_SA, datetime~salinity)

plot(sal_dat_BO, datetime~salinity)

plot(sal_dat_NI, datetime~salinity)

points(sal_dat2, datetime~salinity.y.y, col ="blue")
