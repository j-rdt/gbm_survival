# cox regression
source("GBM_survey_quickload.R")
# before we do a cox regression we must validate that our data pass the assumptions of the test
#### a) that the effect (survival difference) of covariates are constant over time
####    this is called proportionality
#### b) that the effect of combining covariates is additive 
####    this means that, for example, a person with HTN and diabetes has an expected decreased survival
####    equal to the expected decreased survival of one person with hypertension plus one person with diabetes
####    (you are adding the effects, instead of multiplying, exponentiating, etc)

#### We can assess assumption A by constructing kaplan-meier curves
#### If the curve with the comorbidity intersects the curve without it, 
#### then that comorbidity violates proportionality
ts<-as.list(names(newm4)[1:2])
factor2_comorbs<-as.list(names(newm2)[-c(1,2,3,4)])
factor6_comorbs<-as.list(names(newm2)[c(3,4)])
all_pcomorbs_n<-c(Age="Age", 
                   MGMT="MGMT", 
                   IDH="IDH", 
                   InitMaxRes="Initial Maximal Resection", 
                   InitChemoRad="Initial Chemo and Radiation", 
                   cerebrovascular="Cerebrovascular diseases", 
                   conductive="Conductive diseases", 
                   COPD="COPD",
                   coronary.vessels="Coronary vessel disorders", 
                   diabetes="Diabetes", 
                   kidney.diease="Kidney diseases", 
                   HLD="Hyperlipidemia", 
                   HTN="Hypertension", 
                  obesity="obesity")
all_pcomorbs<-c(#Age="Age", 
                  #MGMT="MGMT", 
                  #IDH="IDH", 
                  #InitMaxRes="InitMaxRes", 
                  #InitChemoRad="InitChemoRad", 
                  cerebrovascular="cerebrovascular", 
                  #conductive="conductive", 
                  #COPD="COPD",
                  #coronary.vessels="coronary.vessels", 
                  #diabetes="diabetes", 
                  #kidney.diease="kidney.diease", 
                  #HLD="HLD", 
                  HTN="HTN" 
                  #obesity="obesity", 
                  #diabetes="diabetes"
                  )
fac_ref<-c(MGMT="Unmethylated", 
                IDH="Mutant", 
                InitMaxRes="NO", 
                InitChemoRad="NO", 
                cerebrovascular="NO", 
                conductive="NO", 
                COPD="NO",
                coronary.vessels="NO", 
                diabetes="NO", 
                kidney.disease="NO", 
                HLD="NO", 
                HTN="NO", 
                obesity="NO")

curveintersect<-function(a,b,m){
  a<-a[1:m]
  b<-b[1:m]
  diff<-a-b
  any(diff[1:(m-1)]*diff[2:m]<0)
}

#library(survivalAnalysis)

#test<-analyse_survival(newm2, ts, by=MGMT.UME)
library(survival)

assess_proportions<-function(covars, plt=T, fn=2){

covars<-c("ch")
form<-formula(paste("Surv(Survival.Months, Vital.Status) ~", str_flatten(covars, collapse=" + ")))
test2<-do.call(survfit2, args=list(formula=form, data=newm2))

lens<-test2$strata
x<-test2$time[1:lens[1]]
y<-test2$time[(lens[1]+1):length(test2$time)]
ll<-min(lens)
verdict<-curveintersect(x,y,ll)
print(verdict)

if(plt){

tbls<-lapply(covars, function(cov){table(newm2[names(newm2)==cov])})
labels<-lapply(tbls, function(x){
  sapply(1:length(x), function(i){paste(names(x[i]), x[i], sep=", n=")})})

# Tick function/Parameters 6 interval
ticks<-0:37*12 #(start,end,how many ticks inclusive from start to end)
#ticks_mgmt = linspace(0, 210, 36) #(start,end,how many ticks inclusive from start to end)
ticks_str<-paste(ticks)
minorInds<-which(ticks %% 12 != 0) #Ticks on how many interval
ticks_str[minorInds]<-""

if(fn==2){palette<-c("red", "blue")}else if (fn==4){palette<-c("red", "blue", "yellow", "black")}

obj<-ggsurvplot(test2, 
              xlim=c(0, 204), 
              break.x.by = 6, 
              ylab="Fraction Surviving",
              xlab="Months",
              conf.int = TRUE,
              pval= T,  
              pval.coord=c(60,.25),
              #pval.size=10,
              #risk.table = T, risk.table.title="",
              legend.labs=unlist(labels), 
              legend.title="",  
              surv.scale = "percent", palette=palette,
              #title=paste("Survival:", covars, "| Proportional=", verdict)
              title=paste("Survival:", "Cerebrovascular & HTN"))
p<-obj$plot+
  theme_classic()+
  theme(legend.text = element_text(size = 25))+
  theme(plot.title = element_text(hjust = 0.5, size=26,face = "bold"))+
  guides(colour = guide_legend(nrow = 2))+
  theme(legend.position = c(.60, 0.75))+
  theme(axis.title.x =element_text(size = 17,face = "bold"))+
  theme(axis.title.y = element_text(size = 17,face = "bold"))+
  theme(axis.text.x =  element_text(size=15))+
  theme(axis.text.y =  element_text(size=15))+
  guides(color = guide_legend(override.aes = list(shape=NA)))+
  scale_x_continuous(breaks=ticks, labels=ticks_str)

}

if(plt){
  plot(p)
  sav<-readline("Save image with current dimensions?")
  if(sav=="t"){ggsave(paste(covars,"jpeg", sep="."),p,device="jpeg", dpi = 200,scale=1.5)}
  rm(sav)
}
return(verdict)
}
assessment<-sapply(factor2_comorbs, assess_proportions)
names(assessment)<-factor2_comorbs

a2<-sapply(factor6_comorbs, function(x) {assess_proportions(x, T, 6)})








purrr::map(sapply(all_pcomorbs, function(x){expr(!!x)}), function(by)
{
  analyse_multivariate(newm2,
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


analyse_multivariate(newm2,
                     vars(Survival.Months, Vital.Status),
                     covariates = lapply(all_pcomorbs, function(x){expr(!!x)}), # covariates expects a list
                     covariate_name_dict = all_pcomorbs_n,
                     reference_level_dict = fac_ref) %>%
forest_plot(factor_labeller = all_pcomorbs_n,
            endpoint_labeller = c(Survival.Months="Survival Months"),
            orderer = ~order(abs(log(HR)), decreasing = T),
            labels_displayed = c("endpoint","factor", "n"),
            ggtheme = ggplot2::theme_bw(base_size = 10))

