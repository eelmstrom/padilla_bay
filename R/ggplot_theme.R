## ggplot theme

library(ggplot2)

##ggplot theme
theme_example <- function (base_size = 12, base_family = "") {
  theme_gray(base_size = base_size, base_family = base_family) %+replace%
    theme(axis.text = element_text(colour = "black",size=16),
          axis.title.x = element_text(colour = "black",size = 22),
          axis.title.y = element_text(colour = "black",size=22, angle = 90),
          panel.background = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major = element_blank(),
          plot.background = element_blank(),
          legend.key = element_blank(),
          legend.text = element_text(size=16),
          legend.title = element_text(size=18)
    )
}
