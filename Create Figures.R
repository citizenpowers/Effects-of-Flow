rm(list=ls())

library(cowplot)
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
library(pander)
library(nlstools)
library(ggridges)

# Set theme ---------------------------------------------------------------

ggthemr("light",type="outer", layout="scientific")

# Import Combined Data ----------------------------------------------------

S5A_WQ_and_flow <- read_csv("Data/S5A_WQ_and_flow.csv")   #WQ and Flow
G538_WQ_and_flow <- read_csv("Data/G538_WQ_and_flow.csv") #WQ and Flow
S5AE_WQ_and_flow <- read_csv("Data/S5AE_WQ_and_flow.csv")   #WQ and Flow
S5A_Daily_Average_flow <-read_csv("Data/S5A_Daily_Average_flow.csv")  #Daily mean flows
G538_Daily_Average_flow <-read_csv("Data/G538_Daily_Average_flow.csv") #Daily mean flows



# Tidy data ---------------------------------------------------------------

S5A_WQ_and_flow_long <- S5A_WQ_and_flow  %>%
pivot_longer(2:106,names_to = "Parameter",values_to = "Value") %>%
select(-`Date Time`,-Station)  

S5AE_WQ_and_flow_long <- S5AE_WQ_and_flow  %>%
pivot_longer(2:3,names_to = "Parameter",values_to = "Value") %>%
select(-`Date Time`,-STATION)  


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

All_flow <-G538_Daily_Average_flow %>% mutate(Station="G538") %>% bind_rows(S5A_Daily_Average_flow %>% mutate(Station="S5A")) %>%
mutate(Month=month(Day, label = TRUE, abbr =TRUE),Year=year(Day))

ggplot(All_flow, aes(x =Month , y =`Daily Flow`,fill=Station)) +geom_boxplot(position="dodge",outlier.shape = 21)+
labs(title = 'Mean Daily Flow 2015-22 at S5A and G538 ',y="Mean Daily Flow (cfs)")

ggsave("Mean Daily Flow 2015-22 at S5A and G538.jpg", plot = last_plot(), path ="./Figures/",width = 10.666, height = 6, units = "in", dpi = 300, limitsize = TRUE)


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


# Time Series Plots -------------------------------------------------------


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



# Scatter Plots ----------------------------------------------------------

#S5A TPO4
ggplot(filter(WQ_Time_Series,Flow>1,Parameter %in% c("PHOSPHATE, TOTAL AS P","PHOSPHATE, ORTHO AS P")),aes(Flow,Value*1000,color=Station))+geom_point()+theme_bw()+geom_smooth(method="lm",fill="grey")+facet_wrap(~Parameter,ncol=1,scales="free")+
stat_poly_eq(formula = y~x, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),parse = TRUE)+
#geom_line(data=augment(S5A_model_growth,filter(drop_na(S5A_WQ_and_flow,`PHOSPHATE, TOTAL AS P`),Flow>1)),aes(Flow,.fitted))+
labs(y= expression(mu~g~L^-1),x="Flow (cfs)", title = "TP and OPO4 vs Flow at G538, S5A, S5A South",caption="Flows less than 1 cfs excluded")

ggsave("TP and OPO4 vs Flow at G538, S5A, S5A South.jpg", plot = last_plot(), path ="./Figures/",width = 10.666, height = 8, units = "in", dpi = 300, limitsize = TRUE)


#S5A TPO4
p1 <-ggplot(filter(WQ_Time_Series,Flow>1,Parameter=="PHOSPHATE, TOTAL AS P"),aes(Flow,Value,color=Station))+geom_point()+theme_bw()+geom_smooth(method="lm",fill="grey")+
stat_poly_eq(formula = y~x, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),parse = TRUE)+
geom_line(data=augment(S5A_model_growth,filter(drop_na(S5A_WQ_and_flow,`PHOSPHATE, TOTAL AS P`),Flow>1)),aes(Flow,.fitted),color="black")+
labs(y= expression(mg~L^-1))+theme(plot.margin = unit(c(1,0.5,0.5,0.5), "cm"),legend.position="none") 

#S5A TN
p2 <-ggplot(filter(WQ_Time_Series,Flow>1,Parameter=="TOTAL NITROGEN"),aes(Flow,Value,color=Station))+geom_point()+theme_bw()+geom_smooth(method="lm",fill="grey")+
stat_poly_eq(formula = y~x, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),parse = TRUE)+
labs(y= expression(mg~L^-1),x="Flow (cfs)",caption="Flows less than 1 cfs excluded. Logistic Growth Model in Black")+
theme(plot.margin = unit(c(1,0.5,0.5,0.5), "cm"),legend.position="bottom",axis.title.x = element_blank()) 



plot_grid(p1, p2, labels = c('TPO4 vs Flow', 'TN vs Flow'), label_size = 12,ncol = 1,  hjust = -0.5,vjust =2)

ggsave("Water Quality Analytes vs Flow at G538 and S5A.jpg", plot = last_plot(), path ="./Figures/",width = 10.666, height = 8, units = "in", dpi = 300, limitsize = TRUE)

#S5A Total nitrogen
ggplot(filter(S5A_WQ_and_flow,Flow>1),aes(Flow,`TOTAL NITROGEN`))+geom_point()+theme_bw()+geom_smooth(method="lm")+
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

#create models TP
S5A_model_greater_than_one <- lm(`PHOSPHATE, TOTAL AS P` ~ Flow, data = filter(S5A_WQ_and_flow,Flow>1))
S5A_model <- lm(`PHOSPHATE, TOTAL AS P` ~ Flow, data = S5A_WQ_and_flow)
S5A_model_growth = nls(`PHOSPHATE, TOTAL AS P`~ SSlogis(Flow, a,b , c), data = filter(S5A_WQ_and_flow,Flow>1))
G538_model <- lm(`PHOSPHATE, TOTAL AS P` ~ Flow, data = G538_WQ_and_flow)
G538_model_greater_than_one <- lm(`PHOSPHATE, TOTAL AS P` ~ Flow, data = filter(G538_WQ_and_flow,Flow>1))

#create models TN
S5A_model_greater_than_one_TN <- lm(`TOTAL NITROGEN` ~ Flow, data = filter(S5A_WQ_and_flow,Flow>1))
S5A_model_TN <- lm(`TOTAL NITROGEN` ~ Flow, data = S5A_WQ_and_flow)
G538_model_TN <- lm(`TOTAL NITROGEN` ~ Flow, data = G538_WQ_and_flow)
G538_model_greater_than_one_TN <- lm(`TOTAL NITROGEN` ~ Flow, data = filter(G538_WQ_and_flow,Flow>1))

#create DF of model summaries 
#TP models

df_S5A_mod <-data.frame(Name="TP S5A Linear Regression ",Data="All Observations") %>% cbind( glance(S5A_model) )
df_S5A_mod_greater_than_one <-data.frame(Name="TP S5A Linear Regression ",Data="Flow >1") %>% cbind(glance(S5A_model_greater_than_one)) 
df_S5A_mod_NLS_greater_than_one <-data.frame(Name="TP S5A Logistic Growth Model ",Data="Flow >1") %>% cbind(glance(S5A_model_growth )) 
df_G538_mod <-data.frame(Name="TP G538 Linear Regression ",Data="All Observations") %>% cbind(glance(G538_model) )
df_G538_mod_greater_than_one <-data.frame(Name="TP G538 Linear Regression ",Data="Flow >1") %>% cbind(glance(G538_model_greater_than_one) )

All_mods_TP<- bind_rows(df_S5A_mod,df_S5A_mod_greater_than_one,df_S5A_mod_NLS_greater_than_one,df_G538_mod,df_G538_mod_greater_than_one) 

#TN models
df_S5A_mod_TN <-data.frame(Name="TN S5A Linear Regression ",Data="All Observations") %>% cbind( glance(S5A_model_TN) )
df_S5A_mod_greater_than_one_TN <-data.frame(Name="TN S5A Linear Regression ",Data="Flow >1") %>% cbind(glance(S5A_model_greater_than_one_TN)) 
df_G538_mod_TN <-data.frame(Name="TN G538 Linear Regression ",Data="All Observations") %>% cbind(glance(G538_model_TN) )
df_G538_mod_greater_than_one_TN <-data.frame(Name="TN G538 Linear Regression ",Data="Flow >1") %>% cbind(glance(G538_model_greater_than_one_TN) )

All_mods_TN<- bind_rows(df_S5A_mod_TN,df_S5A_mod_greater_than_one_TN,df_G538_mod_TN,df_G538_mod_greater_than_one_TN) 

#combine model summaries

All_mods <-bind_rows(All_mods_TP,All_mods_TN)

#plot residuals
ggplot(augment(S5A_model,drop_na(S5A_WQ_and_flow,`PHOSPHATE, TOTAL AS P`)), aes(x=.fitted, y=.std.resid)) + geom_point() + geom_smooth(color="grey50",fill="grey") + xlab("Fitted Values") + ylab("Studentized Residuals")
ggplot(augment(S5A_model_greater_than_one,filter(drop_na(S5A_WQ_and_flow,`PHOSPHATE, TOTAL AS P`),Flow>1)), aes(x=.fitted, y=.resid)) + geom_point() + geom_smooth(color="grey50",fill="grey") + xlab("Fitted Values") + ylab("Studentized Residuals")
ggplot(augment(S5A_model_growth,drop_na(filter(S5A_WQ_and_flow,Flow>1),`PHOSPHATE, TOTAL AS P`)), aes(x=.fitted, y=.resid)) + geom_point() + geom_smooth(color="grey50",fill="grey") + xlab("Fitted Values") + ylab("Studentized Residuals")

#Plot fitted lines
ggplot(augment(S5A_model,drop_na(S5A_WQ_and_flow,`PHOSPHATE, TOTAL AS P`),se_fit=TRUE),aes(Flow,`PHOSPHATE, TOTAL AS P`))+geom_point()+
geom_line(aes(Flow,.fitted),color="blue")+
geom_ribbon(aes(ymin=.fitted-1.96*.se.fit, ymax=.fitted+1.96*.se.fit), alpha=0.2)+
geom_line(data=augment(S5A_model_greater_than_one,filter(drop_na(S5A_WQ_and_flow,`PHOSPHATE, TOTAL AS P`),Flow>1)),aes(Flow,.fitted),color="red")+
geom_ribbon(data=augment(S5A_model_greater_than_one,filter(drop_na(S5A_WQ_and_flow,`PHOSPHATE, TOTAL AS P`),Flow>1),se_fit=TRUE),aes(ymin=.fitted-1.96*.se.fit, ymax=.fitted+1.96*.se.fit), alpha=0.2)+
geom_line(data=augment(S5A_model_growth,filter(drop_na(S5A_WQ_and_flow,`PHOSPHATE, TOTAL AS P`),Flow>1)),aes(Flow,.fitted),color="black")+
geom_ribbon(data=augment(S5A_model_growth,filter(drop_na(S5A_WQ_and_flow,`PHOSPHATE, TOTAL AS P`),Flow>1),se_fit=TRUE),aes(ymin=.fitted-1.96*.se.fit, ymax=.fitted+1.96*.se.fit), alpha=0.2)
  
#S5A model diagnostic plots plot
par(mfrow=c(2,2)) #change plot format to display all 4 diagnostic plots at once
plot(S5A_model_greater_than_one)
plot(S5A_model)
plot(nlsResiduals(S5A_model_growth), which = 0)  #uses NLStools to create diagnostic plots

tidy(S5A_model)
tidy(S5A_model_growth)
glance(S5A_model_growth)


# Turbidty vs flow ---------------------------------------------------------------

#S5A Turbidity and TSS
ggplot(filter(S5A_WQ_and_flow_long,Flow>-1000,Parameter %in% c("TURBIDITY","TOTAL SUSPENDED SOLIDS")),aes(Flow,Value))+geom_point()+theme_bw()+geom_smooth(method="lm",fill="grey")+facet_wrap(~Parameter)+
stat_poly_eq(formula = y~x, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),parse = TRUE)+
labs(y="Turbidty (NTU), TSS (mg/L)",x="Flow (cfs)", title = "Turbidirty vs Flow at  S5A")

ggsave("TP and OPO4 vs Flow at G538, S5A, S5A South.jpg", plot = last_plot(), path ="./Figures/",width = 10.666, height = 8, units = "in", dpi = 300, limitsize = TRUE)


#S5AE Turbidity and TSS
ggplot(filter(S5AE_WQ_and_flow_long,Flow>-1000,Parameter %in% c("TURBIDITY","TOTAL SUSPENDED SOLIDS")),aes(Flow,Value))+geom_point()+theme_bw()+geom_smooth(method="lm",fill="grey")+facet_wrap(~Parameter)+
stat_poly_eq(formula = y~x, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),parse = TRUE)+
labs(y="Turbidty (NTU), TSS (mg/L)",x="Flow (cfs)", title = "Turbidirty vs Flow at  S5AE")






