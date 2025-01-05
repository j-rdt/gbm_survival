#20240410 lab meeting work 

source("D://Rwd/GBMSurvey/GBM_survey_quickload.R")
#library(survivalAnalysis)
library(igraph)
library(ggsurvfit)

#basic$Age.factor<-sapply(basic$Age, function(x){
#  if (x<50){
#    "<50"
#  } else if (49<x&x<60) {
#    "50-59"
#  } else if (59<x&x<70) {
#    "60-69"
#  } else if (69<x&x<80) {
#    "70-79"
#  } else if (x>79) {
#    "\u226580"
#  }
#}
#)
#basic$Age.factor<-factor(basic$Age.factor, levels=c("<50", "50-59", "60-69", "70-79", "\u226580"))
Age.factor<-cut(basic$Age, breaks=c(-1, 50, 60, 70, 80, Inf), labels=c("<50", "50-59", "60-69", "70-79", "\u226580"))

basic$KPS<-as.numeric(basic$KPS)
#basic$KPS.factor<-sapply(basic$KPS, function(x){
#  if (!is.na(x)){
#    if (x<50){
#      "10-40"
#    } else if (49<x&x<69) {
#      "50-60"
#    } else if (69<x&x<89) {
#      "70-80"
#    } else if (89<x&x<101) {
#      "90-100"
#    } 
#  } else {NA}
#}
#)
#basic$KPS.factor<-factor(basic$KPS.factor, levels=c("10-40", "50-60", "70-80", "90-100"))

basic$KPS.factor<-cut(basic$KPS, breaks=c(-1, 40, 60, 80, Inf), labels=c("10-40", "50-60", "70-80", "90-100"))

bmi<-bmi[idh_not_mut]
basic$BMI.factor<-sapply(bmi, function(x){
  if (!is.na(x)){
    if (x<25){
      "10-24.99"
    } else if (x<29.99) {
      "25-29.99"
    } else if (x<34.99) {
      "30-34.99"
    } else if (x<39.99) {
      "35-39.99"
    } else {
      "40+"
    }
  } else {NA}
}
  )

#trt$extent...1.subtotal..2..gross.near.gross.[trt$Initial.Max.Resection.=="NO"]<-"No Resection"
#trt$extent...1.subtotal..2..gross.near.gross.[trt$extent...1.subtotal..2..gross.near.gross.=="Gross/Near Gross"]<-"Total Resection"
#trt$extent...1.subtotal..2..gross.near.gross.[trt$extent...1.subtotal..2..gross.near.gross.=="Subtotal"]<-"Partial Resection"
#trt$extent...1.subtotal..2..gross.near.gross.[trt$extent...1.subtotal..2..gross.near.gross.=="Unknown"]<-NA

trt$`Initial.Chemo.w/radiation`[trt$`Initial.Chemo.w/radiation`=="+"]<-"Received"
trt$`Initial.Chemo.w/radiation`[trt$`Initial.Chemo.w/radiation`=="-"]<-"Not Received"

cgn<-cbind(cgn, sapply(maligs$Additional.Malignancy, function(x){if(x=="+"){1}else{0}}))
cg<-cbind(cg, maligs$Additional.Malignancy)
count<-apply(cgn[-1], 1, sum)

basic$count<-count
basic$count[basic$count>2]<-"3+"
basic$count[basic$count<3]<-"0-2"

#ci<-as.data.frame(sapply(names(ci)[-c(1,2)], function(x){replace(ci[[x]], ci[[x]]=="YES", paste0("+", str_replace(x, "\\.", " ")))}))
#ci<-as.data.frame(sapply(names(test), function(x){replace(ci[[x]], ci[[x]]=="NO", paste0("-", str_replace(x, "\\.", " ")))}))


names(trt)[1:6]
names(trt)[2]<-"Init.Resection"
names(trt)[4]<-"Init.Chemo.Rad"
names(ci)[16]<-"Diabetes"

all_pcomorbs_n<-c(Age="Age.factor", 
                  #KPS="KPS", 
                  Resection="Init.Resection",
                  Chemo="Init.Chemo.Rad", 
                  HTN="Hyptertension", 
                  HLD="Hyperlipidemia", 
                  obesity="Obesity", 
                  GERD="GERD", 
                  Diabetes="Diabetes", 
                  osteoarthritis="Osteoarthritis", 
                  Depression="Depression", 
                  CAD="CAD",
                  Hypothyroidism="Hypothyroidism", 
                  BPH="BPH", 
                  Afib="Afib", 
                  Asthma="Asthma", 
                  sleep.apnea="Sleep Apnea", 
                  Stroke="Stroke", 
                  tobacco.use.disorder="Tobacco Use Dis.",
                  COPD="COPD",
                  gout="Gout",
                  nephrolithiasis="Nephrolithiasis",
                  MI="MI",
                  CKD="CKD",
                  alcohol.use.disorder="Alcohol Use Dis.",
                  dysrhythmia="Dysrhythmia",
                  Maligs="Additional.Malignancy",
                  comorbidities="comorb_factor", 
                  CCI="cci_categories",
                  CCI_noage="cci_noage_categories"
                  )
all_pcomorbs<-c(Age="Age.factor", 
                #KPS="KPS", 
                Resection="Init.Resection",
                Chemo="Init.Chemo.Rad", 
                Maligs="Additional.Malignancy",
                HTN="HTN", 
                HLD="HLD", 
                  obesity="Obesity", 
                #  GERD="GERD", 
                  Diabetes="Diabetes", 
                #  osteoarthritis="osteoarthritis", 
                #  Depression="Depression", 
                #  CAD="CAD",
                #  Hypothyroidism="Hypothyroidism", 
                #  BPH="BPH", 
                  Afib="Afib", 
                #  Asthma="Asthma", 
                #  sleep.apnea="sleep.apnea", 
                  Stroke="Stroke",#, 
                #  tobacco.use.disorder="tobacco.use.disorder",
                  COPD="COPD"
                #  gout="gout",
                #  nephrolithiasis="nephrolithiasis",
                #  MI="MI",
                #  CKD="CKD",
                #  alcohol.use.disorder="alcohol.use.disorder",
                #  dysrhythmia="dysrhythmia"
                #comorbidities="comorb_factor",
                #CCI="cci_categories"
                #CCI_noage="cci_noage_categories"
                )
fac_ref<-c(Age="<50", 
           #KPS="KPS", 
           Resection="-",
           Chemo="-", 
           HTN="-", 
           HLD="-", 
           obesity="-", 
           GERD="-", 
           Diabetes="-", 
           Osteoarthritis="-", 
           Depression="-", 
           CAD="-",
           Hypothyroidism="-", 
           BPH="-", 
           Afib="-", 
           Asthma="-", 
           sleep.apnea="-", 
           Stroke="-", 
           tobacco.use.disorder="-",
           COPD="-",
           gout="-",
           nephrolithiasis="-",
           MI="-",
           CKD="-",
           alcohol.use.disorder="-",
           dysrhythmia="-",
           Maligs="-", 
           comorbidities="0",
           CCI="2",
           CCI_noage="2"
)

comorb.factor<-function(x){
  if (x<2){
    "0-1"
  } else if (1<x&x<4) {
    "2-3"
  } else if (3<x&x<6) {
    "4-5"
  } else if (5<x&x<8) {
    "6-7"
  } else if (7<x&x<10){
    "8-9"
  } else {
    "10+"
  }
}

#comorb_factor<-as.factor(number_of_comorbidities)
comorb_factor<-sapply(number_of_comorbidities, comorb.factor)

analyse_multivariate(data.frame(basic, trt, ci, maligs, comorb_factor, cci_categories, cci_noage_categories),
                     vars(Survival.Months, Vital.Status),
                     covariates = lapply(all_pcomorbs, function(x){expr(!!x)}), # covariates expects a list
                     covariate_name_dict = all_pcomorbs_n,
                     reference_level_dict = fac_ref) %>%
  forest_plot(factor_labeller = all_pcomorbs_n,
              endpoint_labeller = c(Survival.Months="Survival Months"),
              orderer = ~order(abs(HR), decreasing = T),
              labels_displayed = c("endpoint","factor", "n"),
              ggtheme = ggplot2::theme_bw(base_size = 10), 
              HR_x_breaks=c(0.25,0.5,1,2,3,5),
              HR_x_limits=c(0.1,6))

purrr::map(sapply(all_pcomorbs, function(x){expr(!!x)}), function(by)
{
  analyse_multivariate(data.frame(basic, ci),
                       vars(Survival.Months, Vital.Status),
                       covariates = list(by), # covariates expects a list
                       covariate_name_dict = all_pcomorbs_n,
                       reference_level_dict = fac_ref 
                       )
}) %>%
  forest_plot(factor_labeller = all_pcomorbs_n,
              endpoint_labeller = c(Survival.Months="Survival Months"),
              orderer = ~order(abs(log(HR)), decreasing = T),
              labels_displayed = c("endpoint","factor", "n"),
              ggtheme = ggplot2::theme_bw(base_size = 10))


surv_me<-function(data, covs, groups=2){
  if (groups==1){
    form<-formula("Surv(Survival.Months, Vital.Status) ~ 1")
  } else {
    form<-formula(paste("Surv(Survival.Months, Vital.Status) ~", str_flatten(covs, collapse=" + ")))
  }
  do.call(survfit2, args=list(formula=form, data=data))
}

survplot<-function(title, data, covs, groups=2, legend.coords=c(0.8,0.8), legend.rows=6, reverse=F, letter="", pval.coord=c(-0.5,0)) {
  #groups=length(covs)*2
  if (groups==1){
    form<-formula("Surv(Survival.Months, Vital.Status) ~ 1")
  } else {
    form<-formula(paste("Surv(Survival.Months, Vital.Status) ~", str_flatten(covs, collapse=" + ")))
  }
    test2<-do.call(survfit2, args=list(formula=form, data=data))
  
  #lens<-test2$strata
  #x<-test2$time[1:lens[1]]
  #y<-test2$time[(lens[1]+1):length(test2$time)]
  #ll<-min(lens)
  # Tick function/Parameters 6 interval
  ticks<-0:37*12 #(start,end,how many ticks inclusive from start to end)
  #ticks_mgmt = linspace(0, 210, 36) #(start,end,how many ticks inclusive from start to end)
  ticks_str<-paste(ticks)
  minorInds<-which(ticks %% 12 != 0) #Ticks on how many interval
  ticks_str[minorInds]<-""
  
  if(groups>1){
  tbls<-lapply(covs, function(cov){table(data[names(data)==cov])})
  labels<-lapply(tbls, function(x){
    sapply(1:length(x), function(i){paste(names(x[i]), x[i], sep=", n=")})})
  } else {
    labels<-covs
  }
  
  if(groups==1){
    palette<-c("red")
  } else if (groups==2){
    palette<-c("blue", "red")
  } else if (groups==3){
      palette<-c("red", "blue", "black")
  } else if (groups==4){
    palette<-c("green2", "blue", "black", "red")
  } else if (groups==5){
    palette<-c("green2", "blue", "purple", "black", "red")
  } else if (groups==6){
    palette<-c("yellow2", "green2", "blue", "purple", "black", "red")
  }
  
  if(reverse){palette<-rev(palette)}
    
  obj<-survminer::ggsurvplot(test2, 
                  xlim=c(0, 144), 
                  break.x.by = 6, 
                  ylab="Fraction Surviving",
                  xlab="Months",
                  conf.int = TRUE,
                  pval= T,  
                  pval.coord=pval.coord,
                  pval.size=3,
                  #risk.table = T, risk.table.title="",
                  legend=legend.coords,
                  legend.labs=unlist(labels), 
                  legend.title="", 
                  surv.scale = "percent", 
                  palette=palette,
                  #conf.int.fill="red", only used to force univariate plot to display CI, weird bug
                  #title=paste("Survival:", covars, "| Proportional=", verdict)
                  title=title)
  p<-obj$plot
  p<-p+
    theme(legend.text = element_text(size = 12))+
    theme(plot.title = element_text(hjust = 0.5, size=20))+
    guides(colour = guide_legend(nrow=legend.rows))+
    theme(axis.title.x =element_text(size = 12))+
    theme(axis.title.y = element_text(size = 12))+
    theme(axis.text.x =  element_text(size=10))+
    theme(axis.text.y =  element_text(size=10))+
    guides(color = guide_legend(override.aes = list(shape=NA)))+
    scale_x_continuous(breaks=ticks, labels=ticks_str) +
    geom_text(label=letter, aes(hjust=0, vjust=1))
  return(p)
}
# for these plots, I used maxheight=1400 and width=2470
survplot("Overall Survival", covs=c(), basic, groups=1, letter="A")

survplot("MGMT Status", covs=c('MGMT.UME'), basic, )
#survplot("IDH Status", covs=c("IDH.MUT"), basic, legend.rows = 2)
survplot("Age", covs=c('Age.factor'), basic, groups=5)
survplot("Karnofsky Performance Score", covs=c('KPS.factor'), basic, groups=4, reverse=T)

survplot("Resection Status", covs=c('extent...1.subtotal..2..gross.near.gross.'), data=data.frame(basic[c(2,3)], trt), groups=3)
survplot("Initial TMZ & Radiation", covs=c('Initial.Chemo.w.radiation'), data=data.frame(basic[c(2,3)], trt), reverse=T)
survplot("Bevacizumab", covs=c("Received.Avastin"), data=data.frame(basic[2:3], trt), groups=2, legend.coords=c(0.7,0.8), legend.rows=2, reverse=T)

survplot("Hypertension", covs=c('HTN'), data=data.frame(basic[c(2,3)], HTN=sapply(ci$HTN, function(x){paste0(x, "HTN")})))
survplot("Hyperlipidemia", covs=c('HLD'), data=data.frame(basic[c(2,3)], HLD=sapply(ci$HLD, function(x){paste0(x, "HLD")})))
survplot("Diabetes", covs=c('Diab'), data=data.frame(basic[c(2,3)], Diab=sapply(ci$Diab, function(x){paste0(x, "Diab")})))
survplot("COPD", covs=c('COPD'), data=data.frame(basic[c(2,3)], COPD=sapply(ci$COPD, function(x){paste0(x, "COPD")})))

survplot("Stroke", covs=c('Stroke'), data=data.frame(basic[c(2,3)], Stroke=sapply(ci$Stroke, function(x){paste0(x, "Stroke")})))


obesity_data<-data.frame(basic[c(2,3)], Ob=sapply(ci$Obesity, function(x){paste0(x, "Obesity")}))
obesity_data<-obesity_data[!is.na(basic$BMI.factor),]
survplot("Obesity", covs=c('Ob'), data=obesity_data)
survplot("BMI", covs=c("BMI"), data=data.frame(basic[c(2,3)], BMI=basic$BMI.factor), groups=5)

survplot("Osteoarthritis", covs=c('OA'), data=data.frame(basic[c(2,3)], OA=sapply(ci$OA, function(x){paste0(x, "OA")})))
survplot("Atrial Fibrillation", covs=c("Afib"), data=data.frame(basic[c(2,3)], Afib=sapply(ci$Afib, function(x){paste0(x, "Afib")})))

survplot("Coronary Artery Disease", covs=c('CAD'), data=data.frame(basic[c(2,3)], CAD=sapply(ci$CAD, function(x){paste0(x, "CAD")})))

addl_maligs<-cbind(basic[2], basic[3], maligs[9])
addl_maligs$Additional.Malignancy<-sapply(addl_maligs$Additional.Malignancy, function(x){if(x=="+"){"Additional Malignancies Present"}else{"No Additional Malignancies"}})
survplot("Additional Malignancy", covs=c('Additional.Malignancy'), data=addl_maligs, legend.coords=c(0.7,0.8)) # ?????

#######
# Use the ana-comorb-hist.R file for this one
survplot("Survival by Number of Comorbidities", covs=c("comorb_factor"), groups=6, data=data.frame(basic[c(2,3)], comorb_factor))
#######
survplot("Charlson Comorbidity Index", covs=c('cci_categories'), data=data.frame(basic[c(2,3)], cci_categories), groups=3)
survplot("Age-independent CCI", covs=c('cci_noage_categories'), data=data.frame(basic[c(2,3)], cci_noage_categories), groups=3)

obaf<-subset.make(big, subset.find(big, "Obesity & Afib"))
survplot("Obesity & Afib", covs=c('Obesity', 'Afib'), data=data.frame(basic[c(2,3)], obaf), groups=4)


setwd("final.figures")
tiff("copd.tiff", height=1800, width = 1800, units="px", res=300)
# insert ggplot code
dev.off()
