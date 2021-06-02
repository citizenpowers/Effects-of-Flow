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






ggplot(S5A_WQ_and_flow,aes(Flow,`PHOSPHATE, TOTAL AS P`))+geom_point()+theme_bw()

ggplot(S5A_WQ_and_flow,aes(Flow,`PHOSPHATE, ORTHO AS P`))+geom_point()

ggplot(S5A_WQ_and_flow,aes(Flow,`PHOSPHATE, DISSOLVED AS P`))+geom_point()


ggplot(S319_WQ_and_flow,aes(Flow,`PHOSPHATE, TOTAL AS P`))+geom_point()+theme_bw()

ggplot(S319_WQ_and_flow,aes(Flow,`PHOSPHATE, ORTHO AS P`))+geom_point()

ggplot(S319_WQ_and_flow,aes(Flow,`PHOSPHATE, DISSOLVED AS P`))+geom_point()

