### GBM survey study

#here::i_am("../final.figures")

library(readxl)
library(dplyr)
library(stringr)
library(rms)
library(cluster)
library(data.table)
library(tidyr)
library(ggplot2)
library(ggsurvfit)

has<-function(x){
  str_detect(x, "\\+|Y|^1")
}

factorPlus<-function(x){
  factor(x, levels=c("-", "+"))
}

nastri<-function(dfdollar, what="both"){
  if (what=="both"){
    str_detect(dfdollar, "NA\\.|\\.NA")
  } else if (what=="IDH") {
    str_detect(dfdollar, "\\.NA")
  } else if (what=="MGMT") {
    str_detect(dfdollar, "NA\\.")
  } else {
    print("Invalid input to 'what', returning complete indices")
    rep(1:length(dfdollar))
  }
}


#getwd()
#setwd(".")
#returnwd<-function(){setwd("C:/Users/jardi/Brown Dropbox/Jonathan Arditi/Documents/RwdLocal/GBMSurvey")}
file<-"../GBM_analysis_no_dup.xlsx"

#'
cgn<-as.data.frame(read_xlsx(file, sheet=5)) # 1 = NO; 2=YES
names(cgn)<-str_replace_all(names(cgn), " ", ".")
cg<-cgn
cgn[-1]<-apply(cgn[-1], 2, function(x){replace(x, x==1, 0)})
cgn[-1]<-apply(cgn[-1], 2, function(x){replace(x, x==2, 1)})

cg[-1]<-apply(cg[-1], 2, function(x){replace(x, x==1, "-")})
cg[-1]<-apply(cg[-1], 2, function(x){replace(x, x==2, "+")})
#rm(cgn)

ci<-as.data.frame(read_xlsx(file, sheet=6)) #normal 
names(ci)<-str_replace_all(names(ci), " ", ".")
bmi<-ci[2]
ci[-c(1,2)]<-apply(ci[-c(1,2)], 2, function(x){replace(x, x==1, "+")}) # this is correct, it is different than cg
ci[-c(1,2)]<-apply(ci[-c(1,2)], 2, function(x){replace(x, x==0, "-")})


trt<-as.data.frame(read_xlsx(file, sheet=4)) # empty, 0 = NO; 1, date = yes; # cycles is an int
names(trt)<-str_replace_all(names(trt), " ", ".")

trt[-1]<-apply(trt[-1], 2, function(x){replace(x, x>43, 1)})

extents<-which(str_detect(names(trt), "extent"))
trt[extents]<-
  apply(trt[extents], 2, function(x){
    replace(x, x==2, "Gross Total")})
trt[extents]<-
  apply(trt[extents], 2, function(x){
    replace(x, x==1, "Partial")})

names(trt)[3]<-"Resection"

names(trt)[7]<-"Bevacizumab"
trt[7]<-replace(trt[7], is.na(trt[7])|trt[7]!=1, "Not Received")
trt[7]<-replace(trt[7], trt[7]==1, "Received")

trt[3]<-replace(trt[3], is.na(trt[3])|trt[3]==0, "Biopsy Only")
#sum(trt[2]==1&is.na(trt[3]))
#trt[3]<-replace(trt[3], trt[2]==0&trt[3]==0, "Biopsy Only")
trt[11]<-replace(trt[11], trt[10]==1&is.na(trt[11]), "Biopsy Only")
trt[15]<-replace(trt[15], trt[14]==1&is.na(trt[15]), "Biopsy Only")

trt[7]<-replace(trt[7], is.na(trt[7])|trt[7]==0, "-")
trt[7]<-replace(trt[7], trt[7]==1, "+")

trt[2]<-replace(trt[2], trt[2]==0, "-")
trt[2]<-replace(trt[2], trt[2]==1, "+")
trt[4]<-replace(trt[4], trt[4]==0|is.na(trt[4]), "Not Received")
trt[4]<-replace(trt[4], trt[4]==1, "Received")
trt[10]<-replace(trt[10], trt[10]==0, "-")
trt[10]<-replace(trt[10], trt[10]==1, "+")
trt[14]<-replace(trt[14], trt[14]==0, "-")
trt[14]<-replace(trt[14], trt[14]==1, "+")
#trt[trt[2]=="-"|is.na(trt[2]),3]<-"Biopsy Only"

#which(str_detect(names(trt), "cycle|MRN"))

trt[2:7]<-apply(trt[2:7], 2, factor)
trt[9:21]<-apply(trt[9:21], 2, factor)
trt[23:60]<-apply(trt[23:60], 2, factor)
trt[62:74]<-apply(trt[62:74], 2, factor)

basic<-as.data.frame(read_xlsx(file, sheet=3))
names(basic)<-str_replace_all(names(basic), " ", ".")
basic<-basic[-8]

basic[5]<-replace(basic[5], basic[5]==0, NA)
basic[5]<-replace(basic[5], basic[5]==1, "Methylated") #Methylated
basic[5]<-replace(basic[5], basic[5]==2, "Unmethylated") # Unmethylated

basic[6]<-replace(basic[6], basic[6]==0|is.na(basic[6]), "Unkown")
basic[6]<-replace(basic[6], basic[6]==1, "WT") # WT
basic[6]<-replace(basic[6], basic[6]==2, "Mutant") #Mutant
#basic[is.na(basic[6]), 6]<-0

#bmi<-bmi[idh_not_mut]
#bmi<-bmi[!is.na(bmi)]

basic[7]<-replace(basic[7], basic[7]=="NA", NA)
names(basic)[5:6]<-c("MGMT.UME", "IDH.MUT")

#for sleep apnea and osa merge
ci$Apnea<-mapply(function(x,y){if ((x=="+" | y=="+") & (!is.na(x)) & (!is.na(y))) {"+"} else {"-"}}, ci$Apnea, ci$OSA)

maligs<-as.data.frame(read_xlsx("../maligs_no_dup.xlsx", sheet=1))
names(maligs)<-str_replace_all(names(maligs), " ", ".")
maligs[is.na(maligs)]<-0
maligs<-maligs[-868,]

maligs$Additional.Malignancy<-sapply(maligs$Additional.Malignancy, function(x){if(x){"+"}else{"-"}})

#finally factorizing certain string columns
basic$Age.factor<-cut(basic$Age, breaks=c(-1, 50, 60, 70, 80, Inf), 
                      labels=c("<50", "50-59", "60-69", "70-79", "\u226580"), 
                      right=FALSE)
basic$KPS<-as.numeric(basic$KPS)
basic$KPS[is.na(basic$KPS)]<-NA
basic$KPS.factor<-factor(cut(basic$KPS, breaks=c(1, 40, 60, 80, Inf), 
                      labels=c("10-40", "50-60", "70-80", "90-100")), 
                      exclude=NULL)
trt$Resection<-factor(trt$Resection, levels=c("Biopsy Only", "Partial", "Gross Total"))
basic$MGMT.UME<-factor(basic$MGMT.UME, levels=c("Unmethylated", "Methylated"))
trt$Bevacizumab<-factor(trt$Bevacizumab, levels = c("Not Received", "Received"))
trt$`Initial.Chemo.w/radiation`<-factor(trt$`Initial.Chemo.w/radiation`, levels=c("Not Received", "Received"))
ci$HTN<-factorPlus(ci$HTN)
ci$HLD<-factorPlus(ci$HLD)
ci$Diab<-factorPlus(ci$Diab)

ci$Obesity<-ifelse(ci$BMI>29.5, "+", "-")
ci$Obesity<-factorPlus(ci$Obesity)

ci$CAD<-factorPlus(ci$CAD)
ci$Stroke<-factorPlus(ci$Stroke)
ci$Afib<-factorPlus(ci$Afib)
ci$OSA<-factorPlus(ci$OSA)
ci$COPD<-factorPlus(ci$COPD)
maligs$Additional.Malignancy<-factorPlus(maligs$Additional.Malignancy)


if(!exists("IDH_remove")){
  IDH_remove<-TRUE
}

if(IDH_remove){
    idh_not_mut<-basic[6]!="Mutant"
    basic<-basic[idh_not_mut,]
    basic<-basic[rowSums(is.na(basic)) != ncol(basic), ]
    trt<-trt[idh_not_mut,]
    trt<-trt[rowSums(is.na(trt)) != ncol(trt), ]
    ci<-ci[idh_not_mut,]
    ci<-ci[rowSums(is.na(ci)) != ncol(ci), ]
    cg<-cg[idh_not_mut,]
    cg<-cg[rowSums(is.na(cg)) != ncol(cg), ]
    cgn<-cgn[idh_not_mut,]
    cgn<-cgn[rowSums(is.na(cgn)) != ncol(cgn), ]
    maligs<-maligs[idh_not_mut,]
    maligs<-maligs[rowSums(is.na(maligs)) != ncol(maligs), ]
}

#setwd("./final.figures")
#this.dir <- dirname(parent.frame(2)$ofile)
