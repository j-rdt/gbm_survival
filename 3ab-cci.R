
#returnwd()
#IDH_remove=TRUE
#source("gbm_survival/GBM_survey_quickload.R")

library(stringr)

has<-function(x){
  str_detect(x, "\\+|Y|^1")
}

cci_age_violin<-function(datatable){
  ggplot(datatable, aes(y=CCI, x=Age, fill=CCI)) + 
    stat_boxplot(geom = "errorbar",
                 width = 0.5) + 
    geom_violin() +
    geom_boxplot(width=.25) +
    #  geom_text(data = means, aes(label = Age, y = -10)) +
    #  labs(x=count) +
    theme(plot.title = element_text(hjust = 0.5, size=20))+
    theme(axis.title.x = element_text(size = 12))+
    theme(axis.title.y = element_text(size = 12))+
    theme(axis.text.x =  element_text(size=10))+
    theme(axis.text.y =  element_text(size=10)) +
    scale_y_discrete(limits=rev) +
    xlab("Age") +
    ylab("CCI") +
    scale_fill_manual(values=c("green2", "blue", "grey", "red"))
}

cci_n_violin<-function(datatable){
  ggplot(datatable, aes(y=CCI, x=Number, fill=CCI)) + 
    stat_boxplot(geom = "errorbar",
                 width = 0.5) + 
    geom_violin() +
    geom_boxplot(width=.25) +
    #  geom_text(data = means, aes(label = Age, y = -10)) +
    #  labs(x=count) +
    theme(plot.title = element_text(hjust = 0.5, size=20))+
    theme(axis.title.x = element_text(size = 12))+
    theme(axis.title.y = element_text(size = 12))+
    theme(axis.text.x =  element_text(size=10))+
    theme(axis.text.y =  element_text(size=10)) +
    scale_y_discrete(limits=rev) +
    xlab("Number of Comorbidities") +
    ylab("CCI") +
    scale_fill_manual(values=c("green2", "blue", "grey", "red"))
}

dist<-apply(complete, 1, function(x){sum(has(x), na.rm = T)})

################ CCI ####################

cci<-function(age, mi, chf, pvd, 
             stroke, dem, copd, 
             ra, lupus, RP, 
             pud, ld, hepb, 
             hepc, dbm, 
             ckd,tumor,met){
  cci<-2
  if (age>79){
      cci<-cci+4
    } else if (age>69){
      cci<-cci+3
    } else if (age>59){
      cci<-cci+2
    } else if (age>49){
      cci<-cci+1
    } #anyone younger than 50 gets cci +0
  
  if (has(mi)){cci<-cci+1} 
  if (has(chf)){cci<-cci+1} # congestive heart failure
  if (has(pvd)){cci<-cci+1} #peripheral vascular disease  
  if (has(stroke)){cci<-cci+1} #cerebrovascular accident or TIA
  if (has(dem)){cci<-cci+1}
  if (has(copd)){cci<-cci+1} 
  if (has(ra)|has(lupus)|has(RP)){cci<-cci+1} #connective tissue
  if (has(pud)){cci<-cci+1} #eptic ulcers
  if (has(ld)){
      cci<-cci+3
    } else if (has(hepb)|has(hepc)){
      cci<-cci+1
    } #this is the closest we can get to accurately capturing liver disease
  if (has(dbm)) {cci<-cci+1} # no way to assess end-organ damage from dbm in our data
  #if (has(hp)){cci<-cci+2} #hemiplegia, never collected
  if (has(ckd)) {cci<-cci+2}
  hastumor<-str_detect(tumor, "oma|cancer|tumor")&!(str_detect(tumor, "ymphoma|eukemia")&!str_detect(tumor, "[^\\w\\s]+"))&!str_detect(tumor, "NOT CONFIRMED|large cell lymphoma|")
  if (hastumor&has(met)){
    cci<-cci+6
    } else if (hastumor){
    cci<-cci+2
    }
  if (str_detect(tumor, "eukemia|AML")){cci<-cci+2}
  if (str_detect(tumor, "ymphoma")){cci<-cci+2}
  #if (aids=="YES"){cci<-cci+6} #no patients in our dataset had AIDS

  if (is.null(cci)){cci<-NA}  
  return(cci)
}

testcci<-mapply(cci, basic$Age, ci$MI, ci$CHF, ci$PVD, 
                ci$Stroke, ci$Dementia, ci$COPD, 
                ci$RA, ci$lupus, ci$rheumatic.polymylagia,
                ci$Peptic.Ulcer, ci$Liver.Disease, ci$hep.B, 
                ci$hepatitis.C, ci$T2DM, 
                ci$CKD, maligs$Cancer.type, maligs$`Metastasis.(y/n)`)

cci_categories<-sapply(testcci, function(x){
  if(x<3){
    "2"
  } else if (x==3){
    "3"
  } else if (x==4){
    "4"
  } else if (x>4){
    "\u22655"
  }
})
cci_categories<-factor(cci_categories, levels=c("2", "3", "4", "\u22655"))


setwd("../final.figures")
tiff("3a-cci-surv.tiff", height=1800, width = 2700, units="px", res=300)
survplot("Charlson Comorbidity Index", covs=c('cci_categories'), 
         data=data.frame(basic[c(2,3)], cci_categories), groups=4, 
         pval.coord = c(108,0.65), pval.size=6)
dev.off()

tiff("3a-cci-age.tiff", height=900, width = 2700, units="px", res=300)
cci_age_violin(data.frame(Age=basic$Age, CCI=cci_categories))
dev.off()

tiff("3a-cci-n.tiff", height=900, width = 2700, units="px", res=300)
cci_n_violin(data.frame(Number=dist, CCI=cci_categories))
dev.off()

setwd("../gbm_survival")

########### Removing age from CCI ####################

cci_noage<-function(mi, chf, pvd, 
              stroke, dem, copd, 
              ra, lupus, RP, 
              pud, ld, hepb, 
              hepc, dbm, 
              ckd,tumor,met){
  cci<-2
#  if (age>79){
#    cci<-4
#  } else if (age>69){
#    cci<-3
#  } else if (age>59){
#    cci<-2
#  } else if (age>49){
#    cci<-1
#  } #anyone younger than 50 gets cci 0
  
  if (has(mi)){cci<-cci+1} 
  if (has(chf)){cci<-cci+1} # congestive heart failure
  if (has(pvd)){cci<-cci+1} #peripheral vascular disease  
  if (has(stroke)){cci<-cci+1} #cerebrovascular accident or TIA
  if (has(dem)){cci<-cci+1}
  if (has(copd)){cci<-cci+1} 
  if (has(ra)|has(lupus)|has(RP)){cci<-cci+1} #connective tissue
  if (has(pud)){cci<-cci+1} #eptic ulcers
  if (has(ld)){
    cci<-cci+3
  } else if (has(hepb)|has(hepc)){
    cci<-cci+1
  } #this is the closest we can get to accurately capturing liver disease
  if (has(dbm)) {cci<-cci+1} # no way to assess end-organ damage from dbm in our data
  #if (has(hp)){cci<-cci+2} #hemiplegia, never collected
  if (has(ckd)) {cci<-cci+2}
  hastumor<-str_detect(tumor, "oma|cancer|tumor")&!(str_detect(tumor, "ymphoma|eukemia")&!str_detect(tumor, "[^\\w\\s]+"))&!str_detect(tumor, "NOT CONFIRMED|large cell lymphoma|")
  if (hastumor&has(met)){
    cci<-cci+6
  } else if (hastumor){
    cci<-cci+2
  }
  if (str_detect(tumor, "eukemia|AML")){cci<-cci+2}
  if (str_detect(tumor, "ymphoma")){cci<-cci+2}
  #if (aids=="YES"){cci<-cci+6} #we never collected info on aids
  
  if (is.null(cci)){cci<-NA}  
  return(cci)
}

testccinoage<-mapply(cci_noage, ci$MI, ci$CHF, ci$PVD, 
                ci$Stroke, ci$Dementia, ci$COPD, 
                ci$RA, ci$lupus, ci$rheumatic.polymylagia,
                ci$Peptic.Ulcer, ci$Liver.Disease, ci$hep.B, 
                ci$hepatitis.C, ci$T2DM, 
                ci$CKD, maligs$Cancer.type, maligs$`Metastasis.(y/n)`)

cci_noage_categories<-sapply(testccinoage, function(x){
  if(x<3){
    "2"
  } else if (x==3){
    "3"
  } else if (x==4){
    "4"
  } else if (x>4){
    "\u22655"
  }
})
cci_noage_categories<-factor(cci_noage_categories, levels=c("2", "3", "4", "\u22655"))

setwd("../final.figures")
tiff("3b-cci-noage-surv.tiff", height=1800, width = 2700, units="px", res=300)
survplot("Age-independent CCI", covs=c('cci_noage_categories'), 
         data=data.frame(basic[c(2,3)], cci_noage_categories), groups=4, 
         pval.coord = c(108,0.65), pval.size=6)
dev.off()

tiff("3b-cci-age.tiff", height=900, width = 2700, units="px", res=300)
cci_age_violin(data.frame(Age=basic$Age, CCI=cci_noage_categories))
dev.off()

tiff("3b-cci-n.tiff", height=900, width = 2700, units="px", res=300)
cci_n_violin(data.frame(Number=dist, CCI=cci_noage_categories))
dev.off()

setwd("../gbm_survival")