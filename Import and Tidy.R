rm(list=ls())


library(readr)
library(readxl)
library(scales)
library(dplyr)
library(ggpmisc)
library(ggplot2)
library(lubridate)
library(tidyr)
library(dbhydroR)


# Step 1: Import and Tidy Flow  Data from CSV --------

#Import S5A flow data
S5A_Flow <-  read_csv("Data/S5A Flow 2015-16.csv") %>%
bind_rows(read_csv("Data/S5A Flow 2017-21.csv")) %>%
bind_rows(read_csv("Data/S5A Flow 2021-22.csv")) %>%
mutate(Date=mdy_hm(`Date Time`)) %>%
distinct(Date,.keep_all=TRUE)
       
#Combined flow data with DF with every minute included
S5A_Flow_by_minute <-  setNames(as.data.frame(seq(from=ISOdate(2015,1,01,0,0,0,tz = "UTC"), to=ISOdate(2022,08,08,0,0,0,tz = "UTC"),by = "min")),"Date") %>%
left_join(S5A_Flow,by="Date") %>%
fill(Flow)  

#Import G538 Flow data
G538_Flow <-  read_csv("Data/G538 Flow 2016-22.csv") %>%
mutate(Date=mdy_hm(`Date Time`))

#Combine G538 data with DF with every minute included
G538_Flow_by_minute <-  setNames(as.data.frame(seq(from=ISOdate(2016,6,01,0,0,0,tz = "UTC"), to=ISOdate(2022,08,08,0,0,0,tz = "UTC"),by = "min")),"Date") %>%
left_join(G538_Flow,by="Date") %>%
fill(Flow)  


# Step 2: Import and Tidy WQ data  --------------------------------------

#S5A grab data 
S5A_WQ_Data <- read_csv("Data/S5A WQ 2015-22.csv") %>%
filter(Matrix=="SW",`Sample Type New`=="SAMP" ) %>%  
select(Collection_Date,`Test Name`,Value) %>%
filter(!is.na(`Test Name`)) %>%
pivot_wider(names_from=`Test Name`,values_from=Value,values_fn ="mean" ) %>%
mutate(Date=mdy_hm(`Collection_Date`))

#G538 data 
G538_WQ_Data <- read_csv("Data/G538 WQ 2015-22.csv") %>%
filter(`Sample Type New`=="SAMP",is.na(Flag)) %>%   
select(Collection_Date,`Test Name`,Value) %>%
filter(!is.na(`Test Name`))%>%
pivot_wider(names_from=`Test Name`,values_from=Value) %>%
mutate(Date=dmy_hm(`Collection_Date`))


# Step 3: Join Flow and RPA data and save DF --------------------------------------

S5A_WQ_and_flow <-S5A_WQ_Data %>%
left_join(S5A_Flow_by_minute,by="Date")

write_csv(S5A_WQ_and_flow,"Data/S5A_WQ_and_flow.csv")

G538_WQ_and_flow <-G538_WQ_Data %>%
left_join(G538_Flow_by_minute,by="Date")

write_csv(G538_WQ_and_flow ,"Data/G538_WQ_and_flow.csv")



# Daily Average Flow ------------------------------------------------------

S5A_Daily_Average_flow <- S5A_Flow_by_minute  %>%
mutate(`Day`=as.Date(Date))  %>%
group_by(Day) %>%  
summarise(n(),`Daily Flow`=mean(Flow))

write_csv(S5A_Daily_Average_flow,"Data/S5A_Daily_Average_flow.csv")

G538_Daily_Average_flow <- G538_Flow_by_minute  %>%
mutate(`Day`=as.Date(Date))  %>%
group_by(Day) %>%  
summarise(n(),`Daily Flow`=mean(Flow))

write_csv(G538_Daily_Average_flow,"Data/G538_Daily_Average_flow.csv")

# Test Code ---------------------------------------------------------------


#S319
S319_Flow <-   read_csv("Data/S319 Flow 2015-21.csv") %>%
  mutate(Date=mdy_hm(`Date Time`))

#Combined 
S319_Flow_by_minute <-  setNames(as.data.frame(seq(from=ISOdate(2015,1,01,0,0,0,tz = "UTC"), to=ISOdate(2022,06,01,0,0,0,tz = "UTC"),by = "min")),"Date") %>%
  left_join(S319_Flow,by="Date") %>%
  fill(Flow)  

#G300 Flow
G300_Flow_by_minute <-  setNames(as.data.frame(seq(from=ISOdate(2015,1,01,0,0,0,tz = "UTC"), to=ISOdate(2022,06,01,0,0,0,tz = "UTC"),by = "min")),"Date") %>%
  left_join(mutate(read_csv("Data/G300 Flow 2015-21.csv"),Date=mdy_hm(`Date`)) ,by="Date") %>%
  mutate(Station="G300") %>%  
  fill(Flow)  

#G301 Flow
G301_Flow_by_minute <-  setNames(as.data.frame(seq(from=ISOdate(2015,1,01,0,0,0,tz = "UTC"), to=ISOdate(2022,06,01,0,0,0,tz = "UTC"),by = "min")),"Date") %>%
  left_join(mutate(read_csv("Data/G301 Flow 2015-21.csv"),Date=mdy_hm(`Date`)) ,by="Date") %>%
  mutate(Station="G301") %>%    
  fill(Flow)  

#G302 Flow
G302_Flow_by_minute <-  setNames(as.data.frame(seq(from=ISOdate(2015,1,01,0,0,0,tz = "UTC"), to=ISOdate(2022,06,01,0,0,0,tz = "UTC"),by = "min")),"Date") %>%
  left_join(mutate(read_csv("Data/G302 Flow 2015-21.csv"),Date=mdy_hm(`Date`)) ,by="Date") %>%
  mutate(Station="G302") %>%  
  fill(Flow)  

#G300, 301, 302 flow combined
G300s <- bind_rows(G300_Flow_by_minute,G301_Flow_by_minute) %>%
  bind_rows(G302_Flow_by_minute)   




#S319 data  
S319_WQ_Data <- read_csv("Data/S319 WQ Data.csv") %>%
  select(Collection_Date,`Test Name`,Value) %>%
  
  filter(!is.na(`Test Name`))%>%
  pivot_wider(names_from=`Test Name`,values_from=Value) %>%
  mutate(Date=mdy_hm(`Collection_Date`))

#G300 structures data  
G302_G301_and_G300_WQ_Data <- read_csv("Data/G302, G301, and G300 WQ Data.csv") %>%
  select(Collection_Date,`Test Name`,Value,`Station ID`) %>%
  filter(!is.na(`Test Name`)) %>%
  pivot_wider(names_from=`Test Name`,values_from=Value) %>%
  mutate(Date=mdy_hm(`Collection_Date`),Station=`Station ID`,`Particulate Phosphorus`=`PHOSPHATE, TOTAL AS P`-`PHOSPHATE, DISSOLVED AS P`) %>%
  select(`PHOSPHATE, TOTAL AS P`,`PHOSPHATE, ORTHO AS P`,`PHOSPHATE, DISSOLVED AS P`,`Particulate Phosphorus`,`Date`,`Station`)


S319_WQ_and_flow <-S319_WQ_Data %>%
  left_join(S319_Flow_by_minute ,by="Date")

G300_G301_G302_WQ_and_flow <-G302_G301_and_G300_WQ_Data %>%
  left_join(G300s ,by=c("Date","Station")) 




