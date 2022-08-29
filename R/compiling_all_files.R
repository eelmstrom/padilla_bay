#### Combining HOBO files to one datasheet ####

#Load packages
library(here)
library(tidyverse)
library(lubridate)

#Set directories
files <- here("data/2_2021_MayJune_deployment/field_deployment/raw")

#--------- SECTION 1A: Read in raw HOBO csv files (Depth, Salinity, Temp) ----------
## Depth
depth <- dir(files,pattern = "*Depth.csv")
depth_dat <- depth %>%
  map(~ read_csv(file.path(files, .),skip = 2, col_names = c("datetime","psi"), col_types = "_??_"))

## Temp 
temp <- dir(temp_files,pattern = "*Temp.csv")
temp_dat <- temp %>%
  map(~ read_csv(file.path(temp_files, .),skip = 2, col_names = c("datetime","temp"), col_types = "_??"))

## Salinity
salinity <- dir(salinity_files,pattern = "*Salinity.csv")
sal_dat <- salinity %>%
  map(~ read_csv(file.path(salinity_files, .),skip = 2, col_names = c("datetime", "salinity"), , col_types = "_??"))

## Change dates
temp_dat <- map(temp_dat, ~ .x %>%
                  mutate(datetime=as.POSIXct(datetime, format = "%m/%d/%y %I:%M:%S %p")))

depth_dat <- map(depth_dat, ~ .x %>%
                   mutate(datetime=as.POSIXct(datetime, format = "%m/%d/%y %I:%M:%S %p")))

sal_dat <- map(sal_dat, ~ .x %>%
                  mutate(datetime=as.POSIXct(datetime, format = "%m/%d/%y %I:%M:%S %p")))

#--------- SECTION 1B: Read in corrected oxygen data (see O2 correction script) ----------
# Oxygen data has been corrected for sensor drift and corrected for salinity using Garcia and Gordon

#--------- SECTION 1C: Read in corrected PAR data (see PAR correction script) ----------
# PAR has been calibrated to the LICOR 

#--------- SECTION 2: Merge all datasets per site and clip to study dates ----------
## Merge datasets to one per site
names(depth_dat) <- names(temp_dat) <- names(sal_dat) <- c("BO", 'JL', 'NIC', 'SAC', 'TS')

test <- files <- here("data/2_2021_MayJune_deployment/field_deployment/test")

#df_list <- list()
names <- c("BO", 'JL', 'NIC', 'SAC', 'TS')
for(i in 1:length(names)){
  df_list <- list(depth_dat[[i]], temp_dat[[i]], sal_dat[[i]])# pull site specific df
  
  df_list2 <- df_list %>% reduce(full_join, by='datetime') #merge
  
  write_csv(df_list2, file.path(test, paste0(names[i],"_metab_data.csv"))) # write
}

