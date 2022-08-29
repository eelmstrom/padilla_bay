#### Script calculate tank test offset####

#Load packages
library(here)
library(tidyverse)
library(lubridate)

#Set directories
files <- here("data/2_2021_MayJune_deployment/1_tank_test")

# Read HOBO
hobo <- dir(files,pattern = "*_hobo.csv")

dat <- hobo %>%
  map(~ read_csv(file.path(files, .),skip = 1))%>%
  reduce(cbind)
head(dat)

colnames(dat) <- c("time", "datetime", 'DO_10441886', 'temp',
                   "time2", "datetime2", 'DO_20409019', 'temp2')

hobo_dat <- as_tibble(dat) %>%
  select(-c("temp","time2", "datetime2", 'temp2'))
hobo_dat

# Read mini
mini <- dir(files, pattern = "*.TXT")
mini

mini_dat <- mini %>%
  map(~ read_csv(file.path(files, .),skip = 8, 
                 col_types=cols_only(`(none)_1` = col_datetime(format = ""),
                                     `(mg/l)` = col_double())),
                 col_names=F)%>%
  reduce(cbind)
mini_dat

mini_dat %>% 
  rename(datetime= `(none)_1`)%>%
  rename(DO_013141= `(mg/l)`)

test <- read_csv(file.path(files, "Cat.TXT"),skip = 8)


