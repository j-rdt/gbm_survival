

setwd("../final.figures")
tiff("htn.tiff", height=1800, width = 1800, units="px", res=300)
survplot("Hypertension", covs=c('HTN'), data=data.frame(basic[c(2,3)], HTN=sapply(ci$HTN, function(x){paste0(x, "HTN")})))
dev.off()

tiff("hld.tiff", height=1800, width = 1800, units="px", res=300)
survplot("Hyperlipidemia", covs=c('HLD'), data=data.frame(basic[c(2,3)], HLD=sapply(ci$HLD, function(x){paste0(x, "HLD")})))
dev.off()

tiff("diab.tiff", height=1800, width = 1800, units="px", res=300)
survplot("Diabetes", covs=c('T2DM'), data=data.frame(basic[c(2,3)], T2DM=sapply(ci$T2DM, function(x){paste0(x, "T2DM")})))
dev.off()

obesity_data<-data.frame(basic[c(2,3)], Ob=sapply(ci$Obesity, function(x){paste0(x, "Obesity")}))
obesity_data<-obesity_data[!obesity_data$Ob=="NAObesity",]
tiff("obesity.tiff", height=1800, width = 1800, units="px", res=300)
survplot("Obesity", covs=c('Ob'), data=obesity_data)
dev.off()

tiff("copd.tiff", height=1800, width = 1800, units="px", res=300)
survplot("COPD", covs=c('COPD'), data=data.frame(basic[c(2,3)], COPD=sapply(ci$COPD, function(x){paste0(x, "COPD")})))
dev.off()

tiff("afib.tiff", height=1800, width = 1800, units="px", res=300)
survplot("Atrial Fibrillation", covs=c("Afib"), data=data.frame(basic[c(2,3)], Afib=sapply(ci$Afib, function(x){paste0(x, "Afib")})))
dev.off()

tiff("stroke.tiff", height=1800, width = 1800, units="px", res=300)
survplot("Stroke", covs=c('Stroke'), data=data.frame(basic[c(2,3)], Stroke=sapply(ci$Stroke, function(x){paste0(x, "Stroke")})))
dev.off()

malig_data<-data.frame(basic[c(2,3)], malig=sapply(maligs$Additional.Malignancy, function(x){paste0(x, "Malignancy")}))
#obesity_data<-obesity_data[!obesity_data$Ob=="NAObesity",]
tiff("malig.tiff", height=1800, width = 1800, units="px", res=300)
survplot("Additional Malignancy", covs=c('malig'), data=malig_data, legend.coords=c(0.7,0.8), 
         pval.coord = c(63, 0.7))
dev.off()

setwd("../gbm_survival")
