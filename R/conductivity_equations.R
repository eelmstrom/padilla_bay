#### Script calculate conductivity bucket test offset####

# Housekeeping
rm(list = ls())  #Clear the workspace
invisible (cat("\014"))  #Clear the console


#Load packages
library(here)
library(tidyverse)
library(lubridate)

####################################################################################
#--------- SECTION FOR SYLVIA  ----------
#####################################################################################
#### Package for Salinity
install.packages('wql')
library(wql)

## SALINITY CONVERSION EXAMPLE
ec2pss(47997.66153*0.001, 26.16, p = 0)
# uses raw conductivity (not specific), and temperature

####################################################################################
#--------- SECTION 1A: Read in raw HOBO csv files (Just conductivity) ----------
#####################################################################################

#Set directories
files <- here("data/misc_sensor_tests")

# Read HOBO files raw conductivities
hobo <- dir(files,pattern = "*_test.csv")

hobo_dat <- hobo %>%
  map(~ read_csv(file.path(files, .),skip = 1, col_types = "_??__?"))%>% 
  reduce(full_join, by='Date Time, GMT-08:00')


colnames(hobo_dat) <- c("datetime", 'SN_BO_21076249', 'BO_sal',
                        'SN_JL_21076247', 'JL_sal',
                        'SN_NIC_10745660', 'NIC_sal', 
                        'SN_SAC_10745661', 'SAC_sal',
                        'SN_TS_21076248', 'TS_sal')# serial number/site for each conductivity logger

# have to trim to ysi date time stamps
hobo_dat <- hobo_dat %>%
  mutate(datetime=as.POSIXct(datetime, format = "%m/%d/%y %I:%M:%S %p"))%>%
  filter(datetime > as.POSIXct("2022-02-15 14:15:00"))%>%
  filter(datetime < as.POSIXct("2022-02-16 19:15:00"))

hobo_dat #check 


####################################################################################
#--------- SECTION 1B: Read in raw YSI csv file (Just conductivity) ----------
#####################################################################################

# Read KOR raw conductivities
ysi <- read_csv(file.path(files, "KorEXO_ysi_file.csv"),skip = 8)%>%
  select(c("Date (MM/DD/YYYY)","Time (HH:mm:ss)","Cond uS/cm",'Temp C', 'Sal psu'))

colnames(ysi) <- c('date','time', 'ysi_conductivity', 'temp','ysi_sal')

ysi #check


####################################################################################
#--------- SECTION 2: Combine and tidy data table ----------
#####################################################################################

# Combine
dat <- 
  bind_cols(ysi, hobo_dat)%>%
  select(-c(date, time))

# SA_C logger first eleven data points are trash
# But we talked about changing this today!
dat$SN_SAC_10745661[1:11]<- rep(NA, 11)

# Tidy version (aka long version so you can group_by)
long_dat <- dat %>%
  mutate(scanNo = row_number())%>%
  pivot_longer(starts_with("SN_"),
               names_to = "SerialNo",
               names_prefix = "SN_")

long_dat  

####################################################################################
#--------- SECTION 3: Create equations and output table ----------
#####################################################################################

# But this might change from linear to something else

lm_equation_coefs <- long_dat %>%
  group_by(SerialNo) %>%
  do(model = lm(ysi_conductivity ~ value, data = .)) %>%
  mutate(slope = model$coefficients[2],
         intercept = model$coefficients[1],
         Rsquared = summary(model)$adj.r.squared)
lm_equation_coefs

lm_equation_coefs <- lm_equation_coefs %>% separate(SerialNo, c('site', 'SerialNo'))


####################################################################################
#--------- SECTION 4: Test equations before write out ----------
#####################################################################################


## Reading in the same data (hobo_dat) as before, but keeping it as a list of tibbles so I can loop through it
test_dat <- hobo %>%
  map(~ read_csv(file.path(files, .),skip = 2,col_names = c('datetime', 'conductivity', 'temp', 'salinity'), col_types = "_???_?"))
names(test_dat) <-  c("BO", 'JL', 'NIC', 'SAC', 'TS')

# Pulling out the correct datetimestamps to match ysi
test_dat <- map(test_dat, ~ .x %>%
                 mutate(datetime=as.POSIXct(datetime, format = "%m/%d/%y %I:%M:%S %p"))%>%
                  filter(datetime > as.POSIXct("2022-02-15 14:15:00"))%>%
                  filter(datetime < as.POSIXct("2022-02-16 19:15:00")))


# Loop to apply equations and check 
par(mfrow=c(2,3), mgp=c(1.5,0.5,0), mar=c(4,3,3,1)) 
for (j in 1:length(test_dat)) {
  
  site <- names(test_dat[j])

  test_dat[[j]]$conductivity_cor <- as.numeric(lm_equation_coefs[lm_equation_coefs$site == site,4])*test_dat[[j]]$conductivity+as.numeric(lm_equation_coefs[lm_equation_coefs$site == site,5]) # use coefficients associated with matching serial number to correct
  
  # plot(test_dat[[j]]$conductivity~test_dat[[j]]$datetime, main = site, pch=21, ylab='Conductivity', xlab='time', ylim=c(0, 60000))
  # points(test_dat[[j]]$conductivity_cor~test_dat[[j]]$datetime, col = "blue", pch=21)
  
  plot(ysi$ysi_conductivity~test_dat[[j]]$conductivity, main = site, xlim=c(0, 36000), ylim=c(0, 36000))
  points(ysi$ysi_conductivity~test_dat[[j]]$conductivity_cor, col ='blue')
  abline(a = 0, b = 1, col = "green")

  test_dat[[j]]$salinity_cor <- ec2pss(test_dat[[j]]$conductivity_cor*0.001, test_dat[[j]]$temp, p = 0)
  
  #plot(test_dat[[j]]$salinity~test_dat[[j]]$datetime, main = site, pch=21, ylab='Salinity', xlab='time', ylim=c(0, 40)) # This is the HOBOware corrected salinity using the YSI numbers in the hobo platform
  # points(test_dat[[j]]$salinity_cor~test_dat[[j]]$datetime, col = "blue", pch=21) # This is the salinity using the corrected conductivity/the R package wql
  # 
  print(test_dat[[j]])
  
  
} 


## Success.

####################################################################################
#--------- SECTION 5: Write out equation coefficients ----------
#####################################################################################

output <- here("data/2_MayJune_deployment/calibration_tests")

to_write <- lm_equation_coefs%>%
  select(-model)

### Write equations to csv

write_csv(to_write, file.path(output, 'conductivity_hoboequations.csv'))


