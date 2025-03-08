# Figure 2, survival plots 

IDH_remove=TRUE
source("00-data-read-clean.R")
source("1a-functions.R")

setwd("../final.figures")
tiff("os.tiff", height=2700, width = 1800, units="px", res=300)
survplot(title="Overall Survival", covs=c(), data=basic, groups=1, 
         pval.coord = c(54,0.60), pval.size=6)
dev.off()

tiff("os-wide.tiff", height=1800, width = 3600, units="px", res=300)
survplot(title="Overall Survival", covs=c(), data=basic, groups=1, 
         pval.coord = c(108,0.75), pval.size=6)
dev.off()

tiff("mgmt.tiff", height=1800, width = 1800, units="px", res=300)
survplot("MGMT Status", covs=c('MGMT.UME'), basic, groups=2, reverse=T, 
         pval.coord = c(60,0.70), pval.size=6, 
         legend.coords=c(0.7,0.8))
dev.off()

tiff("age.tiff", height=1800, width = 1800, units="px", res=300)
survplot("Age", covs=c('Age.factor'), basic, groups=5, 
         pval.coord = c(90,0.62), pval.size=6)
dev.off()

levels(basic$KPS.factor)[levels(basic$KPS.factor)=='KPS_unknown'] <- NA
tiff("kps.tiff", height=1800, width = 1800, units="px", res=300)
survplot("Karnofsky Performance\nScore", covs=c('KPS.factor'), basic, groups=4, reverse=T, 
         pval.coord = c(88,0.62), pval.size=6)
dev.off()

tiff("resect.tiff", height=1800, width = 1800, units="px", res=300)
survplot("Resection Status", covs=c('Resection'), data=data.frame(basic[c(2,3)], trt), groups=3, 
         pval.coord = c(64,0.65), pval.size=6, 
         legend.coords=c(0.7,0.8))
dev.off()

tiff("tmz.tiff", height=1800, width = 1800, units="px", res=300)
survplot("Initial TMZ & Radiation", covs=c('chemorad'), 
         data=data.frame(basic[c(2,3)], chemorad=trt$`Initial.Chemo.w/radiation`), reverse=T, 
         pval.coord = c(62,0.71), pval.size=6, 
         legend.coords=c(0.7,0.8))
dev.off()

tiff("bev.tiff", height=1800, width = 1800, units="px", res=300)
survplot("Bevacizumab", covs=c("bev"), data=data.frame(basic[2:3], bev=trt$Bevacizumab), 
         groups=2, legend.coords=c(0.7,0.8), legend.rows=2, reverse=T, 
         pval.coord = c(61,0.71), pval.size=6)
dev.off()


IDH_remove=FALSE
source("../gbm_survival/00-data-read-clean.R")
setwd("../final.figures")
tiff("idh.tiff", height=1800, width = 1800, units="px", res=300)
survplot("IDH Status", covs=c("IDH.MUT"), basic, legend.rows = 3, groups=3, reverse=T)
dev.off()

setwd("../gbm_survival")


