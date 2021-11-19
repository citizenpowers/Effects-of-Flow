#autosampler vs grab data
#Do autosampler and grab data match when flow is high

library(readr)
library(readxl)
library(scales)
library(dplyr)
library(ggpmisc)
library(ggplot2)
library(lubridate)
library(tidyr)
library(maptools)
library(data.table)
library(hexbin)


# #Import and Tidy  -------------------------------------------------------




#S5A Autosampler data
S5A_AS_Data <- data.table(read_csv("Data/S5A Autosampler Data.csv")) %>%
mutate(`Collection_Date_Autos`=dmy_hm(Collection_Date))    %>%
filter(!is.na(`Test Name`),`Test Name`=="PHOSPHATE, TOTAL AS P",`Sample Type New`=="SAMP",is.na(Flag))%>%  
select(`Collection_Date_Autos`,Value) %>%
mutate(Date=as.Date(`Collection_Date_Autos`),Hour=hour(`Collection_Date_Autos`),Minute=minute(`Collection_Date_Autos`)) %>%    
rename(`TP Autosampler`="Value")  

#S5A grab data 
S5A_Grab_Data <- data.table(read_csv("Data/S5A WQ Data.csv")) %>%
mutate(`Collection_Date_Grabs`=mdy_hm(`Collection_Date`),Date=mdy_hm(Collection_Date)) %>%
filter(!is.na(`Test Name`),`Test Name`=="PHOSPHATE, TOTAL AS P",`Sample Type New`=="SAMP",is.na(Flag))%>%  
select(`Collection_Date_Grabs`,Value,Date) %>%
rename(`TP Grab`="Value")  

#import flow data
S5A_Flow <-  read_csv("Data/S5A Flow 2015-16.csv") %>%
bind_rows(read_csv("Data/S5A Flow 2017-21.csv")) %>%
mutate(Date=mdy_hm(`Date Time`))

#Combined 
S5A_Flow_by_minute <-  setNames(as.data.frame(seq(from=ISOdate(2015,1,01,0,0,0,tz = "UTC"), to=ISOdate(2021,06,01,0,0,0,tz = "UTC"),by = "min")),"Date") %>%
left_join(S5A_Flow,by="Date") %>%
fill(Flow)  


# Join data ---------------------------------------------------------------

#Join Grab and Flow Data
S5A_Grab_and_flow <-S5A_Grab_Data %>%
left_join(S5A_Flow_by_minute,by="Date") %>%
select(Collection_Date_Grabs,`TP Grab`,`Flow`)  %>%
rename(`Flow at time of grab (cfs)`="Flow") %>%
mutate(`Flow Range`=case_when(between(`Flow at time of grab (cfs)`,0,750) ~ "0 to 750 (cfs)",
                                between(`Flow at time of grab (cfs)`,750,1500) ~ "750 to 1500 (cfs)",
                                between(`Flow at time of grab (cfs)`,1500,2250) ~ "1500 to 2250 (cfs)",
                                between(`Flow at time of grab (cfs)`,2250,3000) ~ "2250 to 3000 (cfs)",
                                between(`Flow at time of grab (cfs)`,3000,3750) ~ "3000 to 3750 (cfs)",
                                `Flow at time of grab (cfs)` >3750 ~ "3750+ (cfs)", 
                                TRUE~as.character("Unclassified")))   %>%
mutate(`Flow Range`=  factor(`Flow Range`, levels = c("0 to 750 (cfs)", "750 to 1500 (cfs)","1500 to 2250 (cfs)","2250 to 3000 (cfs)","3000 to 3750 (cfs)","3750+ (cfs)")))    

#Join autosample with flow 
S5A_AS_Flow <- S5A_Flow_by_minute %>%
arrange(Date) %>%  
ungroup()%>%
mutate(Flow=if_else(is.na(Flow),0,Flow))  %>%   #fill NAs with 0
mutate(`Cumulative Flow`=cumsum(Flow)*60/43559.9)    %>%
mutate(`lag flow 1 week`=if_else(is.na(lag(Flow, 10080)),0,lag(Flow, 10080))) %>%
mutate(`Cumulative lag Flow 1 week`=cumsum(`lag flow 1 week`)*60/43559.9)    %>%
mutate(`Flow for previous 1-week`=`Cumulative Flow`-`Cumulative lag Flow 1 week`)  %>%   #calculate flow in previous week
mutate(Hour=hour(Date),Minute=minute(Date),Date=as.Date(Date)) %>%    
right_join(S5A_AS_Data,by=c("Date","Hour","Minute")) %>%
select(`Collection_Date_Autos`,`Flow for previous 1-week`,`TP Autosampler`) %>%
data.table()


# Create time column by which to do a rolling join
S5A_AS_Flow [, RollDate := Collection_Date_Autos]
S5A_Grab_and_flow[, RollDate := Collection_Date_Grabs]
setkey(S5A_AS_Flow ,`RollDate`)
setkey(S5A_Grab_and_flow ,`RollDate`)

# Rolling join by nearest time
S5A_Grab_and_Auto <- S5A_AS_Flow [S5A_Grab_and_flow, roll = TRUE] %>%
mutate(`TP Difference Grab - Auto`=`TP Grab`-`TP Autosampler`)
   



# Figures -----------------------------------------------------------------

#auto vs grab
ggplot(S5A_Grab_and_Auto,aes(`TP Autosampler`,`TP Grab`))+geom_point()+geom_smooth(method="lm")+
stat_poly_eq(formula = y~x, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"),color="red"),parse = TRUE)

#differnce grab-auto by flow at time of grab
ggplot(S5A_Grab_and_Auto,aes(`Flow at time of grab (cfs)`,`TP Difference Grab - Auto`*1000))+geom_point()+geom_smooth(method="lm")+
scale_x_continuous(breaks = seq(0,5000,250))+theme_bw()+scale_y_continuous(breaks = seq(-400,400,25))+
labs(y= expression(Grab-Autosample~(TP~mu~g~L^-1)), title = "Difference between Grab and Autosampler vs Flow at Time of Grab Sample")

ggsave("Difference between Grab and Autosampler vs Flow at Time of Grab Sample.jpg", plot = last_plot(), path ="./Figures/",width = 10.666, height = 6, units = "in", dpi = 300, limitsize = TRUE)



#differnce grab-auto by flow at time of grab boxplots
ggplot(S5A_Grab_and_Auto,aes(`Flow Range`,`TP Difference Grab - Auto`*1000))+geom_boxplot()+theme_bw()+scale_y_continuous(breaks = seq(-400,400,25))+
labs(y= expression(Grabs-Autosamples~(mu~g~L^-1)), title = "Differences in Flow and Autosampler and Grab by Flow")


#instantanious flow vs weekly flow and differnce in TP
ggplot(S5A_Grab_and_Auto,aes(`Flow for previous 1-week`,`Flow at time of grab (cfs)`,color=`TP Difference Grab - Auto`))+geom_hex()+theme_bw()

#differnce grab-auto by flow at time of grab
ggplot(S5A_Grab_and_Auto,aes(`Flow for previous 1-week`,`TP Difference Grab - Auto`*1000))+geom_point()+geom_smooth(method="lm")+theme_bw()+
scale_x_continuous(breaks = seq(0,60000,5000))+theme_bw()+scale_y_continuous(breaks = seq(-400,400,25))+
labs(y= expression(Grab-Autosample~(TP~mu~g~L^-1)),x="Flow prevous week (acre-ft)" ,title = "Difference between Grab and Autosampler vs Total Flow from Previous Week")

ggsave("Difference between Grab and Autosampler vs Total Flow from Previous Week.jpg", plot = last_plot(), path ="./Figures/",width = 10.666, height = 6, units = "in", dpi = 300, limitsize = TRUE)



