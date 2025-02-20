

#source("gbm_survival/GBM_survey_quickload.R")
#source("gbm_survival/1a-functions.R")


setwd("../final.figures")
tiff("2d-ncomorbs_surv.tiff", width=3500, height=1650, units="px", res=300)
survplot("Survival by Number of Comorbidities", covs=c("ncomorbs"), 
         data=data.frame(basic[c(2,3)], ncomorbs=comorb_factor), 
         groups=6, reverse=TRUE)
dev.off()

setwd("../gbm_survival")