rm(list=ls())

library(readr)
library(readxl)
library(scales)
library(dplyr)
library(ggpmisc)
library(ggplot2)
library(lubridate)
library(tidyr)
library(maptools)
library(ggpmisc)
library(e1071)


#S5A
ggplot(filter(S5A_WQ_and_flow,Flow>1),aes(Flow,`PHOSPHATE, TOTAL AS P`*1000))+geom_point()+theme_bw()+
stat_poly_eq(formula = y~x, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"),color="red"),parse = TRUE)+
labs(y= expression(TP~mu~g~L^-1),x="Flow (cfs)", title = "Total Phosphorus vs Flow at S5A from Grab Samples from Jan 2015 to May 2021",caption="Flows less than 1 cfs excluded")

ggsave("Total Phosphorus vs Flow at S5A from Grab Samples from Jan 2015 to May 2021.jpg", plot = last_plot(), path ="./Figures/",width = 10.666, height = 6, units = "in", dpi = 300, limitsize = TRUE)



ggplot(filter(G538_WQ_and_flow,Flow>1),aes(Flow,`PHOSPHATE, TOTAL AS P`*1000))+geom_point()+theme_bw()+geom_smooth(method="lm")+
stat_poly_eq(formula = y~x, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"),color="red"),parse = TRUE)+
labs(y= expression(TP~mu~g~L^-1),x="Flow (cfs)", title = "Total Phosphorus vs Flow at G538 from Grab Samples from Jan 2015 to May 2021",caption="Flows less than 1 cfs excluded")


ggplot(S5A_WQ_and_flow,aes(Flow,`PHOSPHATE, ORTHO AS P`))+geom_point()

ggplot(S5A_WQ_and_flow,aes(Flow,`PHOSPHATE, DISSOLVED AS P`))+geom_point()


ggplot(S319_WQ_and_flow,aes(Flow,`PHOSPHATE, TOTAL AS P`))+geom_point()+theme_bw()

ggplot(S319_WQ_and_flow,aes(Flow,`PHOSPHATE, ORTHO AS P`))+geom_point()

ggplot(S319_WQ_and_flow,aes(Flow,`PHOSPHATE, DISSOLVED AS P`))+geom_point()

ggplot(filter(G300_G301_G302_WQ_and_flow,Station=="G302",Flow>1),aes(Flow,`PHOSPHATE, TOTAL AS P`*1000))+geom_point()+theme_bw()+
stat_poly_eq(formula = y~x, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"),color="red"),parse = TRUE)+  
labs(y= expression(TP~mu~g~L^-1),x="Flow (cfs)", title = "Total Phosphorus vs Flow at G302 from Grab Samples from Jan 2015 to May 2021",caption="Flows less than 1 cfs excluded")

ggsave("Total Phosphorus vs Flow at G302 from Grab Samples from Jan 2015 to May 2021.jpg", plot = last_plot(), path ="./Figures/",width = 10.666, height = 6, units = "in", dpi = 300, limitsize = TRUE)

ggplot(filter(G300_G301_G302_WQ_and_flow,Station=="G302",Flow>1),aes(Flow,`Particulate Phosphorus`*1000))+geom_point()+theme_bw()+
stat_poly_eq(formula = y~x, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"),color="red"),parse = TRUE)+
labs(y= expression(TP~mu~g~L^-1),x="Flow (cfs)", title = "Particulate Phosphorus vs Flow at G302 from Grab Samples from Jan 2015 to May 2021",caption="Flows less than 1 cfs excluded")

ggsave("Particulate Phosphorus vs Flow at G302 from Grab Samples from Jan 2015 to May 2021.jpg", plot = last_plot(), path ="./Figures/",width = 10.666, height = 6, units = "in", dpi = 300, limitsize = TRUE)
