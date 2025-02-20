
#returnwd()
#IDH_remove=TRUE
#source("gbm_survival/GBM_survey_quickload.R")

library(forestploter)
library(survival)
library(survminer)
library(stringr)
library(broom)
options(digits = 2)
df<-data.frame(basic[c(2,3,4,5,7)], trt[c(3,7)], 
               HTN=ci$HTN, HLD=ci$HLD, Apnea=ci$Apnea, Diabetes=ci$Diab, 
               Obesity=ci$Obesity, COPD=ci$COPD, Afib=ci$Afib, 
               Additional.Malignancy=maligs$Additional.Malignancy, 
               chemo=trt$`Initial.Chemo.w/radiation`)

multi<-coxph(Surv(Survival.Months, Vital.Status) ~ +. , data=df) # multivariate, not considering true interactions
# coxph(Surv(Survival.Months, Vital.Status) ~ Age, data=df) # univariate
#multi<-coxph(Surv(Survival.Months, Vital.Status) ~ . , data=df) # this is for specifying interactions

variables<-c("Age", "MGMT Methylation", "KPS", "Partial Resection", 
             "Gross Total Resection", "Bevacizumab", "HTN", "HLD", "Sleep Apnea", 
             "Diabetes", "Obesity", "COPD", "Afib", "Additional Malignancy", "Initial TMZ & Radiation")
varnames<-c("Age", "MGMT.UME", "KPS", "Resection", "Resection", "Bevacizumab", 
            "HTN", "HLD", "Sleep Apnea", "Diabetes", "Obesity", "COPD", 
            "Afib", "Additional Malignancy", "chemo")

n<-c(857, 
     sum(df$MGMT.UME=="Unmethylated", na.rm = TRUE), 
     sum(!is.na(df$KPS)), 
     sum(df$Resection=="Partial"), 
     sum(df$Resection=="Gross Total"), 
     sum(df$Bevacizumab=="Received"), 
     sum(has(df$HTN)), 
     sum(has(df$HLD)), 
     sum(has(df$Apnea)), 
     sum(has(df$Diabetes)), 
     sum(has(df$Obesity), na.rm=TRUE), 
     sum(has(df$COPD)), 
     sum(has(df$Afib)), 
     sum(has(df$Additional.Malignancy)), 
     sum(df$chemo=="Received")
     )
     

cox_df<-data.frame(variable=variables,
                   n=n,
                   blank=paste(rep(" ", 20), collapse = " "),
                   est=exp(coef(multi)),
                   lower=exp(confint(multi)[,1]), 
                   upper=exp(confint(multi)[,2]), 
                   display=paste0(round(exp(coef(multi)), 2), " (",round(exp(confint(multi)[,1]),2),"-", round(exp(confint(multi)[,2]),2), ")"),
                   pval=tidy(multi)$p.value)

names(cox_df)<-c("Variable", "n", "", "est", "lower", "upper", "HR (95% CI)", "P value")

cox_df<-cox_df[order(cox_df$est),]

p<-forest(cox_df[,c(1:3, 7:8)],
            est = cox_df$est,
            lower = cox_df$lower, 
            upper = cox_df$upper,
            sizes = 0.5,
            ci_column = 3,
            ref_line = 1,
            arrow_lab = c("Beneficial", "Detrimental"),
            xlim = c(0.25, 2.5),
            ticks_at = c(0.5, 1, 1.5, 2, 2.5))

# Print plot
setwd("../final.figures")
tiff("5a-multi.tiff", height=1800, width = 2700, units="px", res=300)
plot(p)
dev.off()
