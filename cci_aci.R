library(stringr)

has<-function(x){
  str_detect(x, "\\+|Y|^1")
}

testcci<-mapply(cci, basic$Age, ci$MI, ci$CHF, ci$PVD, 
                ci$Stroke, ci$Dementia, ci$COPD, 
                ci$RA, ci$lupus, ci$rheumatic.polymylagia,
                ci$Peptic.Ulcer, ci$Liver.Disease, ci$hep.B, 
                ci$hepatitis.C, ci$Diab, 
                ci$CKD, maligs$Cancer.type, maligs$`Metastasis.(y/n)`)

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
  #if (aids=="YES"){cci<-cci+6} #we never collected info on aids

  if (is.null(cci)){cci<-NA}  
  return(cci)
}

testcci<-mapply(cci, basic$Age, ci$MI, ci$CHF, ci$PVD, 
                ci$Stroke, ci$Dementia, ci$COPD, 
                ci$RA, ci$lupus, ci$rheumatic.polymylagia,
                ci$Peptic.Ulcer, ci$Liver.Disease, ci$hep.B, 
                ci$hepatitis.C, ci$Diab, 
                ci$CKD, maligs$Cancer.type, maligs$Metastasis..y.n.)

cci_categories<-sapply(testcci, function(x){
  if(x<3){
    "2"
  } else if (x==3){
    "3"
  } else if (x==4){
    "4"
  } else if (x>4){
    "5+"
  }
})

#cci_categories<-sapply(testcci, function(x){
#  if(x<1){
#    "0"
#  } else if (x==1){
#    "1"
#  } else if (x==2){
#    "2"
#  } else {
#    "3+"
#  }
})



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
                ci$hepatitis.C, ci$Diab, 
                ci$CKD, maligs$Cancer.type, maligs$`Metastasis.(y/n)`)

cci_noage_categories<-sapply(testccinoage, function(x){
  if(x<3){
    "2"
  } else if (x==3){
    "3"
  } else if (x==4){
    "4"
  } else if (x>4){
    "5+"
  }
})
cci_noage_categories<-unname(cci_noage_categories)

#cci_noage_categories<-sapply(testccinoage, function(x){
##  if(x<1){
#    "0"
#  } else if (x==1){
#    "1"
#  } else if (x>1){
#    "2+"
#  }
#})
#cci_noage_categories<-unname(cci_noage_categories)


######### Arditi scoring ##########

aci<-function(htn, hld, cad, oa, ob, afib){
  aci<-0
  if(has(htn)){aci<-aci+2}
  if(has(hld)&!has(htn)){aci<-aci+1}
  #if(has(hld)){aci<-aci+1}
  if(has(cad)){act<-aci+1}
  if(has(cad)&!has(hld)){aci<-aci+1}
  if(has(oa)&has(cad)){aci<-aci+2}
  if(has(oa)){aci<-aci+1}
  #if(has(ob)&has(afib)){aci<-aci+1}
  if(has(afib)){aci<-aci+1}
  return(aci)
}

aci_score<-mapply(aci, ci$HTN, ci$HLD, ci$CAD, ci$OA, ci$Obesity, ci$Afib)

#hist(as.numeric(aci_score))
aci_score

aci_cat<-sapply(aci_score, function(x){
  if (x==0){
    "0"
  } else if (x==1){
    "1"
  } else if (x==2){
    "2"
  } else if (x>2){
    "3+"
#  } else if (x>3){
#    "4+"
  }
})
table(basic$Age.factor, aci_cat)

survplot("Arditi score", covs=c('aci_score'), data=data.frame(basic[c(2,3)], aci_score=aci_cat), groups=4)

hmm<-sapply(names(ci)[-1], function(x){sapply(names(ci)[-1], function(y){chisq.test(ci[x],ci[y])})})

for(comorb1 in names(ci)){
  for (comorb2 in names(ci)){
    if (comorb1!=comorb2){
      chisq.test()
    }
  }
}