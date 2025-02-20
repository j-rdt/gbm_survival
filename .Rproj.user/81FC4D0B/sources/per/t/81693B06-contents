# fig 2 functions

##library(ggsurvfit)

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
                             pval= F,  
                             #pval.coord=pval.coord,
                             #pval.size=3,
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
    scale_x_continuous(breaks=ticks, labels=ticks_str)
  return(p)
}
