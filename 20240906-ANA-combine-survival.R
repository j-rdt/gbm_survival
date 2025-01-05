library(survivalAnalysis)
library(igraph)
library(ggsurvfit)

survplotcombn<-function(title="None", covs, data, groups=4, legend.coords=c(0.8,0.8), legend.rows=6){
  #return(covs)
  #stop()
  flatcovs<-str_flatten(covs, collapse=" & ")
  #return(flatcovs)
  #stop()
  subs<-subset.make(data, subset.find(data, flatcovs))
  name<-flatcovs
  notNA<-apply(is.na(subs), 1, any)
  subs<-subs[!notNA,]
  data2<-unite(subs, name, sep=" ")
  subs<-cbind(data[!notNA,1:2],data2)
  #colnames(subs)[1:2]<-c("Survival.Months", "Vital.Status")
  
  form<-formula(paste("Surv(Survival.Months, Vital.Status) ~", "name"))
  test2<-do.call(survfit2, args=list(formula=form, data=subs))
  
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
  
  tbls<-table(subs$name)
  labels<-lapply(1:length(tbls), function(i){paste(names(tbls[i]), tbls[i], sep=", n=")})
  
  #labels<-lapply()
  
  if(groups==1){
    palette<-c("black")
  } else if (groups==2){
    palette<-c("blue", "red")
  } else if (groups==3){
    palette<-c("red", "black", "blue")
  } else if (groups==4){
    palette<-c("green", "blue", "black", "red")
  } else if (groups==5){
    palette<-c("green", "blue", "purple", "black", "red")
  } else if (groups==6){
    palette<-c("yellow", "green", "blue", "purple", "black", "red")
  }
  
  if (title=="None"){
    title<-str_flatten(covs, collapse=" & ")
  }
  
  obj<-survminer::ggsurvplot(test2, 
                             xlim=c(0, 144), 
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
survplotcombn("Atrial Fibrillation & Obesity", 
              covs=c("Afib", "Obesity"), 
              data=data.frame(basic[c(2,3)], Afib=ci$Afib, Obesity=ci$Obesity))

survplot("Atrial Fibrillation", covs=c("Afib"), data=data.frame(basic[c(2,3)], Afib=ci$Afib))

survplotcombn("Hypertension & Hyperlipidemia", 
              covs=c("HTN", "HLD"), 
              data=data.frame(basic[c(2,3)], HTN=ci$HTN, HLD=ci$HLD))
survplot("Hyperlipidemia", covs=c("HLD"), data=data.frame(basic[c(2,3)], HLD=ci$HLD))
survplot("Hypertension", covs=c("HTN"), data=data.frame(basic[c(2,3)], HTN=ci$HTN))


survplotcombn("Diabetes & Hypertension", big, groups=4, legend.coords=c(0.8,0.8), legend.rows=4, 'HTN', 'Diab')
survplotcombn("Coronary Artery Disease & Osteoarthritis", big, groups=4, legend.coords=c(0.8,0.8), legend.rows=4, 'CAD', 'OA')
survplotcombn("Coronary Artery Disease & Hyperlipidemia", big, groups=4, legend.coords=c(0.8,0.8), legend.rows=4, 'HLD', 'CAD')

tiff("cadhld.tiff", height=1400, width = 2470, units="px", res=300)
# insert ggplot code
dev.off()
