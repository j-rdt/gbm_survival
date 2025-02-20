
library(forestploter)
library(survival)
library(survminer)
library(stringr)
library(broom)
options(digits = 2)

comorb_factor2<-factor(comorb_factor, levels=rev(levels(comorb_factor)))
df<-data.frame(basic[c(2,3)], a=comorb_factor2)

multi<-coxph(Surv(Survival.Months, Vital.Status) ~ +. , data=df, ) # multivariate, not considering true interactions
# coxph(Surv(Survival.Months, Vital.Status) ~ Age, data=df) # univariate
#multi<-coxph(Surv(Survival.Months, Vital.Status) ~ . , data=df) # this is for specifying interactions

variables<-multi$xlevels$a[-1]
#varnames<-c("Age", "MGMT.UME", "KPS", "Resection", "Resection", "Bevacizumab", 
#            "HTN", "HLD", "Sleep Apnea", "Diabetes", "Obesity", "COPD", 
#            "Afib", "Additional Malignancy", "chemo")

n<-list(rev(table(comorb_factor))[-1])

cox_df<-data.frame(n,
                   blank=paste(rep(" ", 20), collapse = " "),
                   est=exp(coef(multi)),
                   lower=exp(confint(multi)[,1]), 
                   upper=exp(confint(multi)[,2]), 
                   display=paste0(round(exp(coef(multi)), 2), " (",round(exp(confint(multi)[,1]),2),"-", round(exp(confint(multi)[,2]),2), ")"),
                   pval=tidy(multi)$p.value)

names(cox_df)<-c("Variable", "n", "", "est", "lower", "upper", "HR (95% CI)", "P value")

cox_df<-cox_df[order(cox_df$est),]
View(cox_df)

p<-forest(cox_df[,c(1:3, 7:8)],
          est = cox_df$est,
          lower = cox_df$lower, 
          upper = cox_df$upper,
          sizes = 0.5,
          ci_column = 3,
          ref_line = 1,
          arrow_lab = c("Beneficial", "Detrimental"),
          xlim = c(0.25, 2.5),
          ticks_at = c(0.5, 1, 1.5, 2, 2.5), 
          )

# Print plot
setwd("../final.figures")
tiff("2e-cox.tiff", height=1800, width = 2700, units="px", res=300)
plot(p)
dev.off()