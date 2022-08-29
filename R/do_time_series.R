#--------- SECTION 2: APPLY CALIBRATION EQUATIONS TO DATA ----------

# Housekeeping
rm(list = ls())  #Clear the workspace
invisible (cat("\014"))  #Clear the console

#Load packages
library(here)
library(tidyverse)
library(lubridate)


june <- here("data/2_MayJune_deployment/field_deployment/correctedO2")
aug <- here("data/3_JulyAug_deployment/field_deployment/correctedO2")

#June plots

## READ SENSORS
sensors <- dir(june,pattern = ".csv")
jun_dat <- sensors %>%
  map(~ read_csv(file.path(june, .)))

names(jun_dat) <-  c("BO", 'JL', 'NI_C', 'SA_C', 'TS')
tail(jun_dat$BO)

jun_dat <- map(jun_dat, ~ .x %>%
                   mutate(datetime=as.POSIXct(datetime, format = "%m/%d/%y %I:%M:%S %p")))

jun_dat <- map(jun_dat, ~ .x %>%
                 filter(datetime >=  as.Date("2021-05-29"),
                        datetime <= as.Date("2021-06-21")))

## Depth
june_depth <- here("data/2_MayJune_deployment/field_deployment/raw")
depth <- dir(june_depth,pattern = "*Depth_0.csv")
depth_dat <- depth %>%
  map(~ read_csv(file.path(june_depth, .),skip = 2, col_names = c("datetime","psi","depth"), col_types = "_??_?"))

depth_dat <- map(depth_dat, ~ .x %>%
                   mutate(datetime=as.POSIXct(datetime, format = "%m/%d/%y %I:%M:%S %p")))

depth_dat <- map(depth_dat, ~ .x %>%
                 filter(datetime >=  as.Date("2021-05-29"),
                        datetime <= as.Date("2021-06-21")))

jun_list <- list()
names <- c("BO", 'JL', 'NIC', 'SAC', 'TS')
for(i in 1:length(names)){
  df_list <- list(jun_dat[[i]], depth_dat[[i]])# pull site specific df
  
  data <- df_list %>% reduce(inner_join, by='datetime') #merge
  
  jun_list[[i]] <-data
  
  #write_csv(df_list2, file.path(test, paste0(names[i],"_metab_data.csv"))) # write
}

#sensors under water
water_dat <- map(jun_list, ~ .x %>%
                   filter(depth >=  0.25))


names(water_dat) <- c("Blau Oyster", 'Joe Leary', 'NI_C', 'SA_C', 'Taylor Shellfish')

plot_list <- list()
for (i in 1:length(water_dat)) {
  
  
  site <- names(water_dat)
  p <- ggplot(water_dat[[i]], aes(datetime,do_cor))+
       geom_point(cex=0.5)+
       xlab("")+ylab("DO mg/L")+
       ggtitle(site[i])+ylim(c(0,30))+
       theme_classic()
  # p <- ggplot(water_dat[[i]], aes(datetime,do_cor))+
  #   geom_point(cex = 0.5)+
  #   xlab("")+ylab("DO mg/L")
  #   ggtitle(site[i])+ylim(c(5,30))+
  #   theme_classic()
  print(p)
  
  # 
  # 
  plot_list [[i]] <-p

}
 

names(plot_list) <- c("BO", 'JL', 'NI_C', 'SA_C', 'TS')
plot_list$JL

library(ggpubr)
print(ggarrange(plot_list$TS,
                plot_list$BO,
                plot_list$SA_C,
                plot_list$JL,
                plot_list$NI_C,
                ncol = 5, nrow = 1))%>%
  ggsave(filename=paste("juneDOwater.png"), path = './data', 
         width = 16, height =4)

# August Plots

## READ SENSORS
sensors <- dir(aug,pattern = ".csv")
aug_dat <- sensors %>%
  map(~ read_csv(file.path(aug, .)))

names(aug_dat) <- c("Blau Oyster", 'Joe Leary', 'NI_C', 'SA_C', 'Taylor Shellfish')
aug_dat

aug_dat <- map(aug_dat, ~ .x %>%
                  filter(datetime >=  as.Date("2021-07-24"),
                         datetime <= as.Date("2021-08-20")))

plot_list <- list()
for (i in 1:length(aug_dat)) {

  
  site <- names(aug_dat)
  p <- ggplot(aug_dat[[i]], aes(datetime,do_cor))+
    geom_point(cex=0.5)+
    xlab("")+ylab("DO mg/L")+
    ggtitle(site[i])+ylim(c(0,30))+
    theme_classic()
  print(p)
  
  plot_list [[i]] <-p
  
}

names(plot_list) <- c("BO", 'JL', 'NI_C', 'SA_C', 'TS')
plot_list$JL

library(ggpubr)
print(ggarrange(plot_list$TS,
                plot_list$BO,
                plot_list$SA_C,
                plot_list$JL,
                plot_list$NI_C,
                ncol = 5, nrow = 1))%>%
  ggsave(filename=paste("augustDO.png"), path = './data', 
         width = 16, height =4)

         