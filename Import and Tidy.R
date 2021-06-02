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



# Step 1: Import and Tidy Flow  Data from CSV --------

S5A_Flow <-  read_csv("Data/S5A Flow 2015-16.csv") %>%
bind_rows(read_csv("Data/S5A Flow 2017-21.csv")) %>%
mutate(Date=mdy_hm(`Date Time`))
       
#Combined Outflow over entire flowway
S5A_Flow_by_minute <-  setNames(as.data.frame(seq(from=ISOdate(2015,1,01,0,0,0,tz = "UTC"), to=ISOdate(2021,06,01,0,0,0,tz = "UTC"),by = "min")),"Date") %>%
left_join(S5A_Flow,by="Date") %>%
fill(Flow)  


S319_Flow <-   read_csv("Data/S319 Flow 2015-21.csv") %>%
mutate(Date=mdy_hm(`Date Time`))

#Combined Outflow over entire flowway
S319_Flow_by_minute <-  setNames(as.data.frame(seq(from=ISOdate(2015,1,01,0,0,0,tz = "UTC"), to=ISOdate(2021,06,01,0,0,0,tz = "UTC"),by = "min")),"Date") %>%
left_join(S319_Flow,by="Date") %>%
fill(Flow)  



# Step 2: Import and Tidy WQ data  --------------------------------------

#RPA data from outflows 
S5A_WQ_Data <- read_csv("Data/S5A WQ Data.csv") %>%
select(Collection_Date,`Test Name`,Value) %>%
filter(!is.na(`Test Name`))%>%
pivot_wider(names_from=`Test Name`,values_from=Value) %>%
mutate(Date=mdy_hm(`Collection_Date`))

#RPA data from outflows 
S319_WQ_Data <- read_csv("Data/S319 WQ Data.csv") %>%
select(Collection_Date,`Test Name`,Value) %>%
filter(!is.na(`Test Name`))%>%
pivot_wider(names_from=`Test Name`,values_from=Value) %>%
mutate(Date=mdy_hm(`Collection_Date`))



# Step 3: Join Flow and RPA data and save DF --------------------------------------

S5A_WQ_and_flow <-S5A_WQ_Data %>%
left_join(S5A_Flow_by_minute,by="Date")


S319_WQ_and_flow <-S319_WQ_Data %>%
left_join(S319_Flow_by_minute ,by="Date")




