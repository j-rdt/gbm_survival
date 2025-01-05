# stepping into subpopulations

source("D:/Rwd/GBMSurvey/GBM_survey_quickload.R")
#rm(list=setdiff(ls(), c("ci", "basic", "trt")))
mrns<-na.omit(read.csv("mrns.csv"))
obesity.afib.yes.yes<-mrns$MRN[ci$obesity=="YES" & ci$Afib=="YES"]
obesity.afib.yes.no<-mrns$MRN[ci$obesity=="YES" & ci$Afib=="NO"]
sleep.apnea<-mrns$MRN[ci$sleep.apnea=="YES"]

sai<-mrns$Ascending.MRN.identifier[ci$sleep.apnea=="YES"]
write.csv2(obesity.afib.yes.yes, "obesity_afib_yes_yes.csv", 
           quote=FALSE, 
           row.names = F)
write.csv2(obesity.afib.yes.no, "obesity_afib_yes_no.csv", quote=FALSE, row.names=F)

trtp=data.frame(mrns$MRN, trt)
