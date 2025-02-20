install.packages("survival")
install.packages("survminer")
install.packages("dpylr")
install.packages("ggplot")
install.packages("ggpubr")

library(readxl)
library(survival)
library(survminer)
library(prodlim)
library(Hmisc)
library(pracma)

#Set directory, if don't work, set manually
gbm <- read_excel("Desktop/GBM_patients.xlsx", sheet = "headaches") #import dataset
survfit(Surv(months,event)~MALIG,data=gbm)
#where column headings are “months” “event” “MALIG” (“event” is death (1 is dead 0 is alive)) and 
#“MALIG” is group (2 does have the comorbidity, 1 doesn’t have the comorbidity)

# Tick function/Parameters 6 interval
ticks<-0:37*12 #(start,end,how many ticks inclusive from start to end)
#ticks_mgmt = linspace(0, 210, 36) #(start,end,how many ticks inclusive from start to end)
ticks_str<-paste(ticks)
minorInds<-which(ticks %% 12 != 0) #Ticks on how many interval
ticks_str[minorInds] = ""

# GBM Analysis -------------------------------------------------------
#<gbm_surv_BMI<- survfit(Surv(months,event)~BMI,data=gbm)>

gbm_surv_headache<- survfit(Surv(months,event)~MALIG, data=gbm)

ndiffuse11 = paste("No Headaches/Migraines (n=22)")
ndiffuse22 = paste("Headaches/Migraines (n=846)")

survfit(Surv(months,event)~MALIG,data=gbm) "for mOS" 

gmb_headache_p_val<-surv_pvalue(gbm_surv_headache) #Find P-Value with more decimals
options(digits = 4)
gmb_headache_p_val[2]

gbm_analysis<-ggsurvplot(gbm_surv_headache, xlim=c(0, 204), break.x.by = 6, ylab="Fraction Surviving",
                         xlab="Months",conf.int = TRUE,
                         pval= 0.06,  pval.coord=c(60,.25),pval.size=10,risk.table = FALSE, risk.table.title="",
                         legend.labs=c(ndiffuse11,ndiffuse22), legend.title="",  
                         surv.scale = "percent", palette=c("red", "blue"),
                         title="Headache/Migraine Survival")$plot+
  theme_classic()+
  theme(legend.text = element_text(size = 25))+
  theme(plot.title = element_text(hjust = 0.5, size=26,face = "bold"))+guides(colour = guide_legend(nrow = 2))+
  theme(legend.position = c(.60, 0.75))+
  theme(axis.title.x =element_text(size = 17,face = "bold"))+
  theme(axis.title.y = element_text(size = 17,face = "bold"))+
  theme(axis.text.x =  element_text(size=15))+
  theme(axis.text.y =  element_text(size=15))+
  guides(color = guide_legend(override.aes = list(shape=NA)))+
  scale_x_continuous(breaks=ticks, labels=ticks_str)
gbm_analysis
#Saving R graph
ggsave(gbm_analysis, file="~/Desktop/GBM_curves/headache_delay.tiff",dpi = 200,scale=1.5) #Scale increases to make all data appear on graph
