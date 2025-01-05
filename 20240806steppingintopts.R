source("D:/Rwd/GBMSurvey/GBM_survey_quickload.R")

mrns<-na.omit(read.csv("mrns.csv"))

oayy<-mrns$MRN[ci$obesity=="YES" & ci$Afib=="YES"]
write.csv2(oayy, "oayy.csv", quote=FALSE, row.names = F)
oayn<-mrns$MRN[ci$obesity=="YES" & ci$Afib=="NO"]
write.csv2(oayn, "oayn.csv", quote=FALSE, row.names = F)
oany<-mrns$MRN[ci$obesity=="NO" & ci$Afib=="YES"]
write.csv2(oany, "oany.csv", quote=FALSE, row.names = F)
oann<-mrns$MRN[ci$obesity=="NO" & ci$Afib=="NO"]
write.csv2(oann, "oann.csv", quote=FALSE, row.names = F)

soayy<-ci$sleep.apnea[ci$obesity=="YES" & ci$Afib=="YES"]
soayn<-ci$sleep.apnea[ci$obesity=="YES" & ci$Afib=="NO"]
soany<-ci$sleep.apnea[ci$obesity=="NO" & ci$Afib=="YES"]
soann<-ci$sleep.apnea[ci$obesity=="NO" & ci$Afib=="NO"]

sum(soayy=="YES") #3/18 17%
sum(soayn=="YES") #31/239 13%
sum(soany=="YES") #2/39 5%
sum(soann=="YES") #17/572 2%
# seems like obesity certainly predisposes one to sleep apnea - nothing new here

# go through flywheel to determine whether oayn and oayy have elevated hct or use cpap
# get hct directly, test cpap matching on oayy closely and then just run it for oayn
# finally, make graphs back in R




















