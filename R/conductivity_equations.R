#### Script calculate conductivity bucket test offset####

# Housekeeping
rm(list = ls())  #Clear the workspace
invisible (cat("\014"))  #Clear the console


#Load packages
library(here)
library(tidyverse)
library(lubridate)
library(wql)


#Set directories
files <- here("data/misc_sensor_tests")

####################################################################################
#--------- SECTION 1A: Read in raw HOBO csv files (Just conductivity) ----------
#####################################################################################

# Read HOBO files raw conductivities
hobo <- dir(files,pattern = "*_test.csv")

hobo_dat <- hobo %>%
  map(~ read_csv(file.path(files, .),skip = 1, col_types = "_??___"))%>% 
  reduce(full_join, by='Date Time, GMT-08:00')

hobo_dat <- hobo_dat[1:115,]# have to trim to ysi entries
colnames(hobo_dat) <- c("datetime", 'SN_BO_21076249', 'SN_JL_21076247', 'SN_NIC_10745660','SN_SAC_10745661', 'SN_TS_21076248')# serial number/site for each conductivity logger

hobo_dat #check 

####################################################################################
#--------- SECTION 1B: Read in raw YSI csv file (Just conductivity) ----------
#####################################################################################

# Read KOR raw conductivities
ysi <- read_csv(file.path(files, "KorEXO_ysi_file.csv"),skip = 8)%>%
  select(c("Date (MM/DD/YYYY)","Time (HH:mm:ss)","Cond uS/cm",'Temp C'))
ysi

colnames(ysi) <- c('date','time', 'ysi_conductivity', 'temp')

ysi #check

####################################################################################
#--------- SECTION 2: Combine and tidy data table ----------
#####################################################################################

# Combine
dat <- 
  bind_cols(ysi, hobo_dat)%>%
  select(-c(date, time))

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

###### Quick check using the Blau Oyster sensor

# Correct BO conductivity sensor and compare to ysi
dat$BO_cond_cor <- dat$SN_BO_21076249*lm_equation_coefs$slope[1]+lm_equation_coefs$intercept[1]

plot(ysi_conductivity ~ BO_cond_cor, dat)
summary(lm(ysi_conductivity ~BO_cond_cor, dat))

# Calc ysi and BO salinity and compare

dat$ysi_sal <- ec2pss(dat$ysi_conductivity*0.001, dat$temp, p = 0)
dat$BO_sal <- ec2pss(dat$BO_cond_cor*0.001, dat$temp, p = 0) 

plot(ysi_sal ~ BO_sal, dat)
summary(lm(ysi_sal ~ BO_sal, dat))

## V close. Success.

####################################################################################
#--------- SECTION 5: Write out equation coefficients ----------
#####################################################################################

output <- here("data/2_MayJune_deployment/calibration_tests")

to_write <- lm_equation_coefs%>%
  select(-model)

### Write equations to csv

write_csv(to_write, file.path(output, 'conductivity_hoboequations.csv'))


