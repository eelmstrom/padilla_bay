## Tank test

#Load packages
library(here)
library(tidyverse)
source('R/ggplot_theme.R')

## set directory locations
tank_test <- here("minidot_data/tank_test")
tank_test_drift <- here("minidot_data/tank_test_drift")
drift_test <- here("minidot_data/drift_test2")

drift <- read_csv(file.path(drift_test, "drift_test.csv"))


## Bayview sensor
(ysi <- ggplot()+
    geom_point(drift,mapping = aes(time1, DO_YSI), pch=21, cex =4, col = "black", fill="grey70")+
    xlab("time")+
    scale_y_continuous(name = "DO mgL")+
    ggtitle("YSI")+
    theme_example()+theme(plot.title = element_text(size=20)))

(bayview <- ggplot()+
    geom_point(drift,mapping = aes(time1, DO_013141), pch=21, cex =4, col = "black", fill="grey70")+
    xlab("time")+
    scale_y_continuous(name = "DO mgL")+
    ggtitle("Bayview site - 013141")+
    theme_example()+theme(plot.title = element_text(size=20)))

(ysi_bayview <- ggplot()+
    geom_point(drift,mapping = aes(DO_YSI, DO_013141), pch=21, cex =4, col = "black", fill="grey70")+
    xlab("YSI DO mgL")+
    scale_y_continuous(name = "minidot DO mgL")+
    ggtitle("013141 vs YSI")+
    theme_example()+theme(plot.title = element_text(size=20)))

#Export Bayview fig
ggarrange(ysi,
          bayview,
          ysi_bayview,
          align = "v",
          ncol=1, nrow =3)%>%
  ggexport(filename = "Bayview_013141_drift_Test.png", width = 600, height=1200)

##### Interpretive

(interpretive <- ggplot()+
    geom_point(drift,mapping = aes(time1, DO_122893), pch=21, cex =4, col = "black", fill="grey70")+
    xlab("time")+
    scale_y_continuous(name = "DO mgL")+
    ggtitle("Interpretive center - 122893")+
    theme_example()+theme(plot.title = element_text(size=20)))

(ysi_interpretive <- ggplot()+
    geom_point(drift,mapping = aes(DO_YSI, DO_122893), pch=21, cex =4, col = "black", fill="grey70")+
    xlab("YSI DO mgL")+
    scale_y_continuous(name = "minidot DO mgL")+
    ggtitle("122893 vs YSI")+
    theme_example()+theme(plot.title = element_text(size=20)))

#Export Interpretive center fig
ggarrange(ysi,
          interpretive,
          ysi_interpretive,
          align = "v",
          ncol=1, nrow =3)%>%
  ggexport(filename = "Interpretive_122893_drift_Test.png", width = 600, height=1200)

### Samish Island 
(samish <- ggplot()+
    geom_point(drift,mapping = aes(time1, DO_798741), pch=21, cex =4, col = "black", fill="grey70")+
    xlab("time")+
    scale_y_continuous(name = "DO mgL")+
    ggtitle("Samish Island - 798741")+
    theme_example()+theme(plot.title = element_text(size=20)))

(ysi_samish <- ggplot()+
    geom_point(drift,mapping = aes(DO_YSI, DO_798741), pch=21, cex =4, col = "black", fill="grey70")+
    xlab("YSI DO mgL")+
    scale_y_continuous(name = "minidot DO mgL")+
    ggtitle("798741 vs YSI")+
    theme_example()+theme(plot.title = element_text(size=20)))

#Export Interpretive center fig
ggarrange(ysi,
          samish,
          ysi_samish,
          align = "v",
          ncol=1, nrow =3)%>%
  ggexport(filename = "Samish_798741_drift_Test.png", width = 600, height=1200)

