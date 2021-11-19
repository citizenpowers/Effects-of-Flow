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
       
#Combined 
S5A_Flow_by_minute <-  setNames(as.data.frame(seq(from=ISOdate(2015,1,01,0,0,0,tz = "UTC"), to=ISOdate(2021,06,01,0,0,0,tz = "UTC"),by = "min")),"Date") %>%
left_join(S5A_Flow,by="Date") %>%
fill(Flow)  

#S319
S319_Flow <-   read_csv("Data/S319 Flow 2015-21.csv") %>%
mutate(Date=mdy_hm(`Date Time`))

#Combined O
S319_Flow_by_minute <-  setNames(as.data.frame(seq(from=ISOdate(2015,1,01,0,0,0,tz = "UTC"), to=ISOdate(2021,06,01,0,0,0,tz = "UTC"),by = "min")),"Date") %>%
left_join(S319_Flow,by="Date") %>%
fill(Flow)  

#G300 Flow
G300_Flow_by_minute <-  setNames(as.data.frame(seq(from=ISOdate(2015,1,01,0,0,0,tz = "UTC"), to=ISOdate(2021,06,01,0,0,0,tz = "UTC"),by = "min")),"Date") %>%
left_join(mutate(read_csv("Data/G300 Flow 2015-21.csv"),Date=mdy_hm(`Date`)) ,by="Date") %>%
mutate(Station="G300") %>%  
fill(Flow)  

#G301 Flow
G301_Flow_by_minute <-  setNames(as.data.frame(seq(from=ISOdate(2015,1,01,0,0,0,tz = "UTC"), to=ISOdate(2021,06,01,0,0,0,tz = "UTC"),by = "min")),"Date") %>%
left_join(mutate(read_csv("Data/G301 Flow 2015-21.csv"),Date=mdy_hm(`Date`)) ,by="Date") %>%
mutate(Station="G301") %>%    
fill(Flow)  

#G302 Flow
G302_Flow_by_minute <-  setNames(as.data.frame(seq(from=ISOdate(2015,1,01,0,0,0,tz = "UTC"), to=ISOdate(2021,06,01,0,0,0,tz = "UTC"),by = "min")),"Date") %>%
left_join(mutate(read_csv("Data/G302 Flow 2015-21.csv"),Date=mdy_hm(`Date`)) ,by="Date") %>%
mutate(Station="G302") %>%  
fill(Flow)  

#G300, 301, 302 flow combined
G300s <- bind_rows(G300_Flow_by_minute,G301_Flow_by_minute) %>%
bind_rows(G302_Flow_by_minute)   

#G538 Flow
G538_Flow <-  read_csv("Data/G538 Flow Data.csv") %>%
mutate(Date=mdy_hm(`Date Time`))

#G538 Combined 
G538_Flow_by_minute <-  setNames(as.data.frame(seq(from=ISOdate(2015,1,01,0,0,0,tz = "UTC"), to=ISOdate(2021,06,01,0,0,0,tz = "UTC"),by = "min")),"Date") %>%
left_join(G538_Flow,by="Date") %>%
fill(Flow)  


# Step 2: Import and Tidy WQ data  --------------------------------------

#S5A grab data 
S5A_WQ_Data <- read_csv("Data/S5A WQ Data.csv") %>%
select(Collection_Date,`Test Name`,Value) %>%
filter(!is.na(`Test Name`))%>%
pivot_wider(names_from=`Test Name`,values_from=Value) %>%
mutate(Date=mdy_hm(`Collection_Date`))


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

#G538 data 
G538_WQ_Data <- read_csv("Data/G538 WQ Data.csv") %>%
filter(`Sample Type New`=="SAMP",is.na(Flag)) %>%   
select(Collection_Date,`Test Name`,Value) %>%
filter(!is.na(`Test Name`))%>%
pivot_wider(names_from=`Test Name`,values_from=Value) %>%
mutate(Date=dmy_hm(`Collection_Date`))


# Step 3: Join Flow and RPA data and save DF --------------------------------------

S5A_WQ_and_flow <-S5A_WQ_Data %>%
left_join(S5A_Flow_by_minute,by="Date")

S319_WQ_and_flow <-S319_WQ_Data %>%
left_join(S319_Flow_by_minute ,by="Date")

G300_G301_G302_WQ_and_flow <-G302_G301_and_G300_WQ_Data %>%
left_join(G300s ,by=c("Date","Station")) 

G538_WQ_and_flow <-G538_WQ_Data %>%
left_join(G538_Flow_by_minute,by="Date")





