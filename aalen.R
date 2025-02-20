library(survminer)
library(ggfortify)
library(survival)

#returnwd()
#IDH_remove=TRUE
source("00-data-read-clean.R")

df<-data.frame(basic[c(2,3,4,5,7)], trt[c(3,7)], 
               HTN=ci$HTN, HLD=ci$HLD, Resection=trt$Resection, Bevacizumab=trt$Bevacizumab,
               Diabetes=ci$Diab, Obesity=ci$Obesity, Chemo=trt$`Initial.Chemo.w/radiation`,
               Addl_malig.=maligs$Additional.Malignancy, Afib=ci$Afib, 
               COPD=ci$COPD, Apnea=ci$Apnea, Stroke=ci$Stroke)
aa_fit <-aareg(Surv(Survival.Months, Vital.Status) ~ Age + KPS +
                 Resection + Bevacizumab + HTN + HLD + Diabetes + Obesity + Addl_malig.  + Chemo + Afib + Apnea + COPD + Stroke, 
               data = df)
colnames(aa_fit[[4]])<-c("Intercept", "Age", "KPS", "Partial Resection", "Gross Total Resection", 
                         "Bevacizumab", "HTN", "HLD", "Diabetes", "Obesity", "Additional Malignancy", "Chemo", 
                         "Afib", "Apnea", "COPD", "Stroke")


setwd("../final.figures")
tiff("aalen-variables.tiff", height=3600, width = 3600, units="px", res=300)
autoplot(aa_fit, legend=F) + theme(legend.position = "none", 
                                   text = element_text(size = 20))
dev.off()
setwd("../gbm_survival")

