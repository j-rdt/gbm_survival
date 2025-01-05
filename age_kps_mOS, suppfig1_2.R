subset<-function(data,age){data$Survival.Months[(data$Age<age-5)&(data$Age<age)]}
ages<-c(25,30,35,40,45,50,55,60,65,70,75,80,85,90)
meds<-sapply(ages, 
             function(x){median(subset(data=basic, age=x))})
Model<-lm(meds~ages,data=data.frame(ages,meds))
plot(ages,meds, 
     xlab='Age',
     ylab='Survival Months', 
     main="Median Survival By Age Group")
mtext(paste0("R^2=", round(summary(Model)$r.squared, 3)))
abline(Model)


kpss<-seq(0,100,10)
subsetk<-function(data,kps){data$Survival.Months[data$KPS==kps]}
meds2<-sapply(kpss, 
             function(x){median(subsetk(data=basic, kps=x), na.rm = TRUE)})
#kps_no_na<-na.omit(data.frame(kpss,meds2))
Model2<-lm(meds2~kpss,data=data.frame(kpss,meds2))
plot(kpss,meds2, 
     xlab='KPS',
     ylab='Survival Months', 
     main="Median Survival By Karnofsky Performance Score")
mtext(paste0("R^2=", round(summary(Model2)$r.squared, 4)))
abline(Model2)
