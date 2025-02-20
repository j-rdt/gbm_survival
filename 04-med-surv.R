

setwd("../final.figures")
tiff("htn.tiff", height=1800, width = 1800, units="px", res=300)
survplot("Hypertension", covs=c('HTN'), data=data.frame(basic[c(2,3)], HTN=sapply(ci$HTN, function(x){paste0(x, "HTN")})))
dev.off()

tiff("hld.tiff", height=1800, width = 1800, units="px", res=300)
survplot("Hyperlipidemia", covs=c('HLD'), data=data.frame(basic[c(2,3)], HLD=sapply(ci$HLD, function(x){paste0(x, "HLD")})))
dev.off()

tiff("diab.tiff", height=1800, width = 1800, units="px", res=300)
survplot("Diabetes", covs=c('Diab'), data=data.frame(basic[c(2,3)], Diab=sapply(ci$Diab, function(x){paste0(x, "Diab")})))
dev.off()

obesity_data<-data.frame(basic[c(2,3)], Ob=sapply(ci$Obesity, function(x){paste0(x, "Obesity")}))
obesity_data<-obesity_data[!is.na(basic$BMI.factor),]
tiff("obesity.tiff", height=1800, width = 1800, units="px", res=300)
survplot("Obesity", covs=c('Ob'), data=data.frame(basic[c(2,3)], Ob=ci$Obesity))
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

tiff("malig.tiff", height=1800, width = 1800, units="px", res=300)
survplot("Additional Malignancy", covs=c('malig'), data=data.frame(basic[c(2,3)], malig=maligs$Additional.Malignancy), legend.coords=c(0.7,0.8))
dev.off()

setwd("../gbm_survival")
