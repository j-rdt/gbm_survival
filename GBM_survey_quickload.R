### GBM survey study

library(readxl)
library(dplyr)
library(stringr)
library(rms)
library(cluster)
library(data.table)
library(tidyr)
library(ggplot2)

has<-function(x){
  str_detect(x, "\\+|Y|^1")
}



getwd()
setwd("D:/Rwd/GBMSurvey")
file<-"GBM_analysis_no_dup.xlsx"

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
    replace(x, x==1, "Total Resection")})
trt[extents]<-
  apply(trt[extents], 2, function(x){
    replace(x, x==2, "Partial Resection")})

trt[3]<-replace(trt[3], trt[2]==1&is.na(trt[3]), "No Resection")
trt[3]<-replace(trt[3], trt[2]==0&trt[3]==0, "No Resection")
trt[11]<-replace(trt[11], trt[10]==1&is.na(trt[11]), "No Resection")
trt[15]<-replace(trt[15], trt[14]==1&is.na(trt[15]), "No Resection")

trt[2]<-replace(trt[2], trt[2]==0, "-")
trt[2]<-replace(trt[2], trt[2]==1, "+")
trt[4]<-replace(trt[4], trt[4]==0, "-")
trt[4]<-replace(trt[4], trt[4]==1, "+")
trt[10]<-replace(trt[10], trt[10]==0, "-")
trt[10]<-replace(trt[10], trt[10]==1, "+")
trt[14]<-replace(trt[14], trt[14]==0, "-")
trt[14]<-replace(trt[14], trt[14]==1, "+")
trt[trt[2]=="-"|is.na(trt[2]),3]<-"No Resection"

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

#basic[6]<-replace(basic[6], basic[6]==0, NA)
#basic[6]<-replace(basic[6], basic[6]==1, "WT") # WT
#basic[6]<-replace(basic[6], basic[6]==2, "Mutant") #Mutant
basic[is.na(basic[6]), 6]<-0
idh_not_mut<-basic[6]!=2
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
#bmi<-bmi[idh_not_mut]
#bmi<-bmi[!is.na(bmi)]

basic[7]<-replace(basic[7], basic[7]=="NA", NA)
names(basic)[5:6]<-c("MGMT.UME", "IDH.MUT")

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
#for sleep apnea and osa merge
ci$Apnea<-mapply(function(x,y){if ((x=="+" | y=="+") & (!is.na(x)) & (!is.na(y))) {"+"} else {"-"}}, ci$Apnea, ci$OSA)

#ci<-as.data.frame(sapply(names(ci)[-c(1,2)], function(x){replace(ci[[x]], ci[[x]]=="+", paste0("+", str_replace(x, "\\.", " ")))}))
#ci<-as.data.frame(sapply(names(ci), function(x){replace(ci[[x]], ci[[x]]=="-", paste0("-", str_replace(x, "\\.", " ")))}))


#basic$Survival.Months<-as.numeric(basic$Survival.Months)

maligs<-as.data.frame(read_xlsx("maligs_no_dup.xlsx", sheet=1))
names(maligs)<-str_replace_all(names(maligs), " ", ".")
maligs[is.na(maligs)]<-0
maligs<-maligs[-868,]
maligs<-maligs[idh_not_mut,]
maligs<-maligs[rowSums(is.na(maligs)) != ncol(maligs), ]
maligs$Additional.Malignancy<-sapply(maligs$Additional.Malignancy, function(x){if(x){"+"}else{"-"}})

#newm<-cbind(basic$Survival.Months, 
#            basic$Vital.Status,
#            basic$Age, 
#            ci$BMI,
#            basic$MGMT,
#            basic$IDH,
#            trt$Initial.Max.Resection., 
#            trt$Initial.Chemo.w.radiation, 
#           cg[-c(1,16)],
#            ci[c(24,25,107)])
#newm<-replace(newm, is.na(newm), "NODATA")
#names(newm)[1:8]<-c("Survival.Months", 
#                    "Vital.Status", 
#                    "Age",
#                    "BMI",
#                    "MGMT",
#                    "IDH",
#                    "InitMaxRes",
#                    "InitChemoRad")

#counts<-sapply(newm[-c(1:8)], function(x){sum(x=="YES")})

#newm<-newm[c(1:8, which(counts>30)+8)]

#newm$Age<-sapply(newm$Age, function(x){
#  if (x<40){
#    "<40"
#  } else if (40<=x&x<60){
#    "40-60"
#  } else {">60"}
#})

#Age<-newm$Age

#newm$Age<-sapply(newm$Age, function(x){
#  if (x<40){
#    "<40"
#  } else if (40<=x&x<50){
#    "40-49"
#  } else if (50<=x&x<60){
#    "50-59"
#  } else if (60<=x&x<70){
#    "60-69"
#  } else if (70<=x&x<80){
#    "70-79"
#  } else {"80+"}
#})


#BMI<-newm$BMI
# these breaks are quartile divisions
#newm$BMI<-sapply(newm$BMI, function(x){
#  if (is.na(x)){
#    NA
#  } else if (x<20){
#    "<20"
#  } else if (20<=x&x<25){
#    "20-25"
#  } else if (25<=x&x<30){
#    "25-30"
#  } else if (30<=x&x<35){
#    "30-35"
#  } else if (35<=x&x<40){
#    "35-40"
#  } else if (x=="NODATA"){
#    "NODATA"
#  } else {"40+"}
#})

#newm2<-newm
#newm2[-c(1,2,3,4)]<-lapply(newm2[-c(1,2,3,4)], factor)
