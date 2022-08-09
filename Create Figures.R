rm(list=ls())

library(readr)
library(readxl)
library(scales)
library(dplyr)
library(ggpmisc)
library(ggplot2)
library(lubridate)
library(tidyr)
library(ggpmisc)
library(ggthemr)
library(broom)
library(memisc)
library(pander)

# Set theme ---------------------------------------------------------------

ggthemr("light",type="outer", layout="scientific")

# Import Combined Data ----------------------------------------------------

S5A_WQ_and_flow <- read_csv("Data/S5A_WQ_and_flow.csv")   #WQ and Flow
G538_WQ_and_flow <- read_csv("Data/G538_WQ_and_flow.csv") #WQ and Flow
S5A_Daily_Average_flow <-read_csv("Data/S5A_Daily_Average_flow.csv")  #Daily mean flows
G538_Daily_Average_flow <-read_csv("Data/G538_Daily_Average_flow.csv") #Daily mean flows

#Flow Summary Stats and Histograms --------------------------------------------------------------

ggplot(filter(G538_Daily_Average_flow,`Daily Flow`!=0),aes(`Daily Flow`))+geom_histogram(binwidth=50)+theme_bw()+
labs(y= "Count of Days",x="Flow (cfs)", title = "Histogram of daily mean flow June 2016 to Aug 2022 G538",caption="Days with no flow excluded")+scale_x_continuous(breaks=c(seq(0,3000,500))) 

ggsave("G538_flow_histogram.jpg", plot = last_plot(), path ="./Figures/",width = 10.666, height = 6, units = "in", dpi = 300, limitsize = TRUE)

S5A_flow_histogram <-ggplot(filter(S5A_Daily_Average_flow,`Daily Flow`!=0),aes(`Daily Flow`))+geom_histogram(binwidth=50)+theme_bw()+
labs(y= "Count of Days",x="Flow (cfs)", title = "Histogram of daily mean flow Jan 2015 to Aug 2022 S5A",caption="Days with no flow excluded")+scale_x_continuous(breaks=c(seq(0,5000,500)))

G538_summary_stats <-G538_Daily_Average_flow %>% mutate(Station="G538") %>% group_by(Station) %>%
summarise(`Total Days`=n(),`Days without Flow`=sum(ifelse(`Daily Flow`==0,TRUE,FALSE),na.rm=TRUE),`Days of Flow >0`=sum(ifelse(`Daily Flow`>0,TRUE,FALSE),na.rm=TRUE),
`Days of missing data`=sum(is.na(`Daily Flow`)),`Days of Flow <0`=sum(ifelse(`Daily Flow`<0,TRUE,FALSE),na.rm=TRUE),`Daily flow mean`=mean(`Daily Flow`,na.rm=TRUE),`Daily flow median`=median(`Daily Flow`,na.rm=TRUE),
IQR=IQR(`Daily Flow`,na.rm=TRUE),SD=sd(`Daily Flow`,na.rm=TRUE),`Mean flow from days which flow occured`=sum(`Daily Flow`,na.rm=TRUE)/(`Days of Flow <0`+`Days of Flow >0`)) %>%
mutate(across(where(is.numeric), round, digits=0)) 

S5A_summary_stats <-S5A_Daily_Average_flow %>% mutate(Station="S5A") %>% group_by(Station) %>%
summarise(`Total Days`=n(),`Days without Flow`=sum(ifelse(`Daily Flow`==0,TRUE,FALSE),na.rm=TRUE),`Days of Flow >0`=sum(ifelse(`Daily Flow`>0,TRUE,FALSE),na.rm=TRUE),
`Days of missing data`=sum(is.na(`Daily Flow`)),`Days of Flow <0`=sum(ifelse(`Daily Flow`<0,TRUE,FALSE),na.rm=TRUE),`Daily flow mean`=mean(`Daily Flow`,na.rm=TRUE),`Daily flow median`=median(`Daily Flow`,na.rm=TRUE),
IQR=IQR(`Daily Flow`,na.rm=TRUE),SD=sd(`Daily Flow`,na.rm=TRUE),`Mean flow from days which flow occured`=sum(`Daily Flow`,na.rm=TRUE)/(`Days of Flow <0`+`Days of Flow >0`)) %>%
mutate(across(where(is.numeric), round, digits=0))

Summary_stats <-  rbind(G538_summary_stats,S5A_summary_stats)


# Water Quality Summary Stats  --------------------------------------------

G538_WQ_summary_stats_TP <-G538_WQ_and_flow %>% mutate(Station="G538",Analyte="TP",Units="ug/l") %>% group_by(Station,Analyte,Units) %>%
summarise(`Samples`=sum(ifelse(!is.na(`PHOSPHATE, TOTAL AS P`),TRUE,FALSE),na.rm=TRUE),`Mean`=mean(`PHOSPHATE, TOTAL AS P`,na.rm=TRUE),
`Median`=median(`PHOSPHATE, TOTAL AS P`,na.rm=TRUE),`SD`=sd(`PHOSPHATE, TOTAL AS P`,na.rm=TRUE),IQR=IQR(`PHOSPHATE, TOTAL AS P`,na.rm=TRUE)) %>%
mutate(across(where(is.numeric), round, digits=3)) 

G538_WQ_summary_stats_TN <-G538_WQ_and_flow %>% mutate(Station="G538",Analyte="TN",Units="mg/l") %>% group_by(Station,Analyte,Units) %>%
summarise(`Samples`=sum(ifelse(!is.na(`TOTAL NITROGEN`),TRUE,FALSE),na.rm=TRUE),`Mean`=mean(`TOTAL NITROGEN`,na.rm=TRUE),
`Median`=median(`TOTAL NITROGEN`,na.rm=TRUE),`SD`=sd(`TOTAL NITROGEN`,na.rm=TRUE),IQR=IQR(`TOTAL NITROGEN`,na.rm=TRUE)) %>%
mutate(across(where(is.numeric), round, digits=3)) 

S5A_WQ_summary_stats_TP <-S5A_WQ_and_flow %>% mutate(Station="S5A",Analyte="TP",Units="ug/l") %>% group_by(Station,Analyte,Units) %>%
summarise(`Samples`=sum(ifelse(!is.na(`PHOSPHATE, TOTAL AS P`),TRUE,FALSE),na.rm=TRUE),`Mean`=mean(`PHOSPHATE, TOTAL AS P`,na.rm=TRUE),
`Median`=median(`PHOSPHATE, TOTAL AS P`,na.rm=TRUE),`SD`=sd(`PHOSPHATE, TOTAL AS P`,na.rm=TRUE),IQR=IQR(`PHOSPHATE, TOTAL AS P`,na.rm=TRUE)) %>%
mutate(across(where(is.numeric), round, digits=3)) 

S5A_WQ_summary_stats_TN <-S5A_WQ_and_flow %>% mutate(Station="S5A",Analyte="TN",Units="mg/l") %>% group_by(Station,Analyte,Units) %>%
summarise(`Samples`=sum(ifelse(!is.na(`TOTAL NITROGEN`),TRUE,FALSE),na.rm=TRUE),`Mean`=mean(`TOTAL NITROGEN`,na.rm=TRUE),
`Median`=median(`TOTAL NITROGEN`,na.rm=TRUE),`SD`=sd(`TOTAL NITROGEN`,na.rm=TRUE),IQR=IQR(`TOTAL NITROGEN`,na.rm=TRUE)) %>%
mutate(across(where(is.numeric), round, digits=3)) 

Summary_stats_WQ <- rbind(G538_WQ_summary_stats_TP,S5A_WQ_summary_stats_TP) %>% rbind(G538_WQ_summary_stats_TN) %>% rbind(S5A_WQ_summary_stats_TN) 


# Scatter Plots ----------------------------------------------------------

#S5A TPO4
ggplot(filter(WQ_Time_Series,Flow>1),aes(Flow,Value,color=Station))+geom_point()+theme_bw()+geom_smooth(method="lm",fill="grey")+facet_wrap(~Parameter,ncol=1,scales="free")+
stat_poly_eq(formula = y~x, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),parse = TRUE)+
labs(y= expression(TP~mu~g~L^-1),x="Flow (cfs)", title = "Water Quality Analytes vs Flow at G538 and S5A",caption="Flows less than 1 cfs excluded")

ggsave("Water Quality Analytes vs Flow at G538 and S5A.jpg", plot = last_plot(), path ="./Figures/",width = 10.666, height = 8, units = "in", dpi = 300, limitsize = TRUE)

#S5A Total nitrogen
ggplot(filter(S5A_WQ_and_flow,Flow>-1000),aes(Flow,`TOTAL NITROGEN`))+geom_point()+theme_bw()+geom_smooth(method="lm")+
stat_poly_eq(formula = y~x, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"),color="red"),parse = TRUE)+scale_x_continuous(breaks=c(seq(0,6400,800)))+
labs(y= expression(TN~mg~L^-1),x="Flow (cfs)", title = "Total Phosphorus vs Flow at S5A from Grab Samples from Jan 2015 to Aug 2022",caption="Flows less than 1 cfs excluded")

#G538 TPO4
ggplot(filter(G538_WQ_and_flow,Flow>-1000),aes(Flow,`PHOSPHATE, TOTAL AS P`*1000))+geom_point()+theme_bw()+geom_smooth(method="lm")+
stat_poly_eq(formula = y~x, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"),color="red"),parse = TRUE)+
labs(y= expression(TP~mu~g~L^-1),x="Flow (cfs)", title = "Total Phosphorus vs Flow at G538 from Grab Samples from Jan 2015 to Aug 2022",caption="Flows less than 1 cfs excluded")

ggsave("Total Phosphorus vs Flow at G538 from Grab Samples from Jan 2015 to Aug 2022.jpg", plot = last_plot(), path ="./Figures/",width = 10.666, height = 6, units = "in", dpi = 300, limitsize = TRUE)

#G538 Total nitrogen
ggplot(filter(G538_WQ_and_flow,Flow!=0),aes(Flow,`TOTAL NITROGEN`))+geom_point()+theme_bw()+geom_smooth(method="lm")+
stat_poly_eq(formula = y~x, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"),color="red"),parse = TRUE)+
labs(y= expression(TN~mg~L^-1),x="Flow (cfs)", title = "Total Nitrogen vs Flow at S5A from Grab Samples from Jan 2015 to Aug 2022",caption="Flows less than 1 cfs excluded")

#S5A TPO4 logistic growth curve
ggplot(filter(S5A_WQ_and_flow,Flow>-1000),aes(Flow,`PHOSPHATE, TOTAL AS P`/max(`PHOSPHATE, TOTAL AS P`,na.rm=TRUE)))+geom_point()+theme_bw()+geom_smooth(method="lm",color="red")+
geom_smooth(method = "glm",formula = "y ~ x",method.args = list(family="quasibinomial"),se = T)+
stat_poly_eq(method = "glm",formula = "y ~ x", aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"),color="red"),parse = TRUE)+
scale_y_continuous(labels = function(x) x * max(S5A_WQ_and_flow$`PHOSPHATE, TOTAL AS P`,na.rm=TRUE))+#scale_x_continuous(breaks=c(seq(0,6400,800)))+
labs(y= expression(TP~mu~g~L^-1),x="Flow (cfs)", title = "Total Phosphorus vs Flow at S5A from Grab Samples from Jan 2015 to Aug 2022",caption="Flows less than 1 cfs excluded")


# Model Plots -------------------------------------------------------------

S5A_model_greater_than_one <- lm(`PHOSPHATE, TOTAL AS P` ~ Flow, data = filter(S5A_WQ_and_flow,Flow>1))
S5A_model <- lm(`PHOSPHATE, TOTAL AS P` ~ Flow, data = S5A_WQ_and_flow)

G538_model <- lm(`PHOSPHATE, TOTAL AS P` ~ Flow, data = G538_WQ_and_flow)

summary(S5A_model)

#S5A model residual plot
ggplot(data.frame(x = seq(S5A_model$residuals), y = S5A_model$residuals)) +
geom_point(aes(x, y)) +labs(x = "Index", y = "Residuals", title = paste("Residuals of", format(S5A_model$call)))

#S5A model residual plot
ggplot(data.frame(x = seq(G538_model$residuals), y = G538_model$residuals)) +
geom_point(aes(x, y)) +labs(x = "Index", y = "Residuals", title = paste("Residuals of", format(G538_model$call)))


# Time Series Plots -------------------------------------------------------

#Create combined DF of S5A and G538
WQ_Time_Series <- mutate(select(S5A_WQ_and_flow,Date,Flow,`PHOSPHATE, TOTAL AS P`,`TOTAL NITROGEN`),Station="S5A") %>%
rbind(mutate(select(G538_WQ_and_flow,Date,Flow,`PHOSPHATE, TOTAL AS P`,`TOTAL NITROGEN`),Station="G538") ) %>%
mutate(Year=year(Date),Month=month(Date,label = TRUE,abbr = TRUE),yday=yday(Date),mday=mday(Date)) %>%
mutate(datetime =make_datetime(2020, Month, mday, 12,0, 0))  %>%
pivot_longer(names_to="Parameter",values_to="Value",3:4)

#Seasonal WQ at G538 and S5A
ggplot(WQ_Time_Series,aes(datetime,Value,color=as.factor(Station)))+geom_point()+theme_bw()+geom_smooth(fill="grey")+facet_wrap(~Parameter,ncol=1,scales="free_y")+
labs(y= expression(mg~L^-1),x="Date", title = "Seasonal WQ Pattern at G538 and S5A")+
scale_x_datetime(breaks =make_datetime(2020,1:12), labels = month.abb) 

ggsave("Seasonal WQ Pattern.jpg", plot = last_plot(), path ="./Figures/",width = 10.666, height = 8, units = "in", dpi = 300, limitsize = TRUE)

#Seasonal plots
ggplot(S5A_WQ_and_flow,aes(Date,`PHOSPHATE, TOTAL AS P`*1000))+geom_point()+theme_bw()+geom_smooth(method="loess",span=.2)+
geom_line(data=S5A_Flow,aes(Date,Flow/10),color="red")+  scale_y_continuous( sec.axis = sec_axis(~.*10, name = "Flow (CFS)"))+
scale_x_datetime(date_breaks = "3 months",date_labels = "%b%y")+
labs(y= expression(TP~mu~g~L^-1),x="Flow (cfs)", title = "Total Phosphorus vs Flow at S5A from Grab Samples from Jan 2015 to August 2022")

ggplot(mutate(S5A_WQ_and_flow,`Flow Category`=ifelse(Flow>0,"Flow","No Flow")),aes(month(Date,label = TRUE),`PHOSPHATE, TOTAL AS P`))+
geom_jitter(color="light blue")+geom_boxplot(alpha=.5)+theme_bw()+facet_wrap(~`Flow Category`)


#Create DF to plot daily average flow 
G538_Daily_Average_flow_year_month<-G538_Daily_Average_flow %>% 
mutate(Year=year(Day),Month=month(Day,label = TRUE,abbr = TRUE),yday=yday(Day),mday=mday(Day)) %>%
mutate(datetime =make_datetime(2020, Month, mday, 12,0, 0))  
  
#Seasonal flow at G538
ggplot(G538_Daily_Average_flow_year_month,aes(datetime,`Daily Flow`,color=as.factor(Year)))+geom_point()+geom_line()+theme_bw()+
labs(y= "Mean Daily Flow (cfs)",x="Date", title = "G538 Seasonal flow Pattern")+
scale_x_datetime(breaks =make_datetime(2020,1:12), labels = month.abb) 

ggsave("G538 Seasonal flow Pattern.jpg", plot = last_plot(), path ="./Figures/",width = 10.666, height = 6, units = "in", dpi = 300, limitsize = TRUE)



# Test Code ---------------------------------------------------------------



ggplot(S5A_WQ_and_flow,aes(Flow,`PHOSPHATE, ORTHO AS P`))+geom_point()

ggplot(S5A_WQ_and_flow,aes(Flow,`PHOSPHATE, DISSOLVED AS P`))+geom_point()

ggplot(S5A_WQ_and_flow,aes(Flow,(`PHOSPHATE, TOTAL AS P`-`PHOSPHATE, DISSOLVED AS P`)))+geom_point()

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


