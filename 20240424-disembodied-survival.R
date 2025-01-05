#20240410 lab meeting work 

source("GBM_survey_quickload.R")
library(survivalAnalysis)
library(igraph)
library(ggsurvfit)

basic$Age.factor<-sapply(basic$Age, function(x){
  if (x<40){
    "<40"
  } else if (39<x&x<50) {
    "40-49"
  } else if (49<x&x<60) {
    "50-59"
  } else if (59<x&x<70) {
    "60-69"
  } else if (69<x&x<80) {
    "70-79"
  } else if (x>79) {
    "80+"
  }
}
)

all_pcomorbs_n<-c(HTN="Hyptertension", 
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
                  dysrhythmia="Dysrhythmia"
                  )
all_pcomorbs<-c(HTN="HTN", 
                #  HLD="HLD", 
                #  obesity="obesity", 
                #  GERD="GERD", 
                #  Diabetes="Diabetes", 
                #  osteoarthritis="osteoarthritis", 
                #  Depression="Depression", 
                  CAD="CAD"#,
                #  Hypothyroidism="Hypothyroidism", 
                #  BPH="BPH", 
                #  Afib="Afib", 
                #  Asthma="Asthma", 
                #  sleep.apnea="sleep.apnea", 
                #  Stroke="Stroke"#, 
                #  tobacco.use.disorder="tobacco.use.disorder",
                #  COPD="COPD",
                #  gout="gout",
                #  nephrolithiasis="nephrolithiasis",
                #  MI="MI",
                #  CKD="CKD",
                #  alcohol.use.disorder="alcohol.use.disorder",
                #  dysrhythmia="dysrhythmia"
                )
fac_ref<-c(HTN="NO", 
           HLD="NO", 
           obesity="NO", 
           GERD="NO", 
           Diabetes="NO", 
           osteoarthritis="NO", 
           Depression="NO", 
           CAD="NO",
           Hypothyroidism="NO", 
           BPH="NO", 
           Afib="NO", 
           Asthma="NO", 
           sleep.apnea="NO", 
           Stroke="NO", 
           tobacco.use.disorder="NO",
           COPD="NO",
           gout="NO",
           nephrolithiasis="NO",
           MI="NO",
           CKD="NO",
           alcohol.use.disorder="NO",
           dysrhythmia="NO"
)

analyse_multivariate(surv20,
                     vars(Survival.Months, Vital.Status),
                     covariates = lapply(all_pcomorbs, function(x){expr(!!x)}), # covariates expects a list
                     covariate_name_dict = all_pcomorbs_n,
                     reference_level_dict = fac_ref) %>%
  forest_plot(factor_labeller = all_pcomorbs_n,
              endpoint_labeller = c(Survival.Months="Survival Months"),
              orderer = ~order(abs(log(HR)), decreasing = T),
              labels_displayed = c("endpoint","factor", "n"),
              ggtheme = ggplot2::theme_bw(base_size = 10))

purrr::map(sapply(all_pcomorbs, function(x){expr(!!x)}), function(by)
{
  analyse_multivariate(surv20,
                       vars(Survival.Months, Vital.Status),
                       covariates = list(by), # covariates expects a list
                       covariate_name_dict = all_pcomorbs_n,
                       reference_level_dict = fac_ref)
}) %>%
  forest_plot(factor_labeller = all_pcomorbs_n,
              endpoint_labeller = c(Survival.Months="Survival Months"),
              orderer = ~order(abs(log(HR)), decreasing = T),
              labels_displayed = c("endpoint","factor", "n"),
              ggtheme = ggplot2::theme_bw(base_size = 10))


survplot<-function(title, data, groups=4, legend.coords=c(0.5,1), legend.rows=1, ...) {
  covs<-c(...)
  #groups=length(covs)*2
  if (groups==1){
    form<-formula("Surv(Survival.Months, Vital.Status) ~ 1")
  } else {
    form<-formula(paste("Surv(Survival.Months, Vital.Status) ~", str_flatten(covs, collapse=" + ")))
  }
    test2<-do.call(survfit2, args=list(formula=form, data=data))
  
  lens<-test2$strata
  x<-test2$time[1:lens[1]]
  y<-test2$time[(lens[1]+1):length(test2$time)]
  ll<-min(lens)
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
    palette<-c("black")
  } else if (groups==2){
    palette<-c("blue", "red")
  } else if (groups==3){
      palette<-c("red", "black", "blue")
  } else if (groups==4){
    palette<-c("red", "black", "blue", "green")
  } else if (groups==5){
    palette<-c("red", "purple", "black", "blue", "green")
  } else if (groups==6){
    palette<-c("purple", "blue", "green", "yellow", "orange", "red")
  }
    
  obj<-survminer::ggsurvplot(test2, 
                  xlim=c(0, 204), 
                  break.x.by = 6, 
                  ylab="Fraction Surviving",
                  xlab="Months",
                  conf.int = TRUE,
                  pval= T,  
                  pval.coord=c(-0.5,0),
                  pval.size=4,
                  #risk.table = T, risk.table.title="",
                  legend=legend.coords,
                  legend.labs=unlist(labels), 
                  legend.title="", 
                  surv.scale = "percent", palette=palette,
                  #title=paste("Survival:", covars, "| Proportional=", verdict)
                  title=title)
  p<-obj$plot
  p<-p+
    theme(legend.text = element_text(size = 15))+
    theme(plot.title = element_text(hjust = 0.5, size=20))+
    guides(colour = guide_legend(nrow=legend.rows))+
    theme(axis.title.x =element_text(size = 12))+
    theme(axis.title.y = element_text(size = 12))+
    theme(axis.text.x =  element_text(size=10))+
    theme(axis.text.y =  element_text(size=10))+
#    guides(color = guide_legend(override.aes = list(shape=NA)))+
    scale_x_continuous(breaks=ticks, labels=ticks_str)
  return(p)
}

survplot("MGMT Status", basic, groups=2, legend.coords=c(0.7,0.5), legend.rows=2,'MGMT.UME')
survplot("IDH Status", basic, groups=2, legend.coords=c(0.8,0.5), legend.rows=2, 'IDH.MUT')
survplot("Age", basic, groups=6, legend.coords=c(0.8,0.8), legend.rows=6, 'Age.factor')


tiff("ana-age.tiff", height=maxheight, width = maxheight*ratio, units="px", res=300)
# insert ggplot code
dev.off()




