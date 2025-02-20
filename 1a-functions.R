# fig 2 functions

##library(ggsurvfit)

survplot<-function(title, data, covs, groups=2, 
                   legend.coords=c(0.8,0.8), legend.rows=6, 
                   reverse=F, letter="", 
                   pval.coord=c(96,0.71), pval.size=6,
                   legend.text=18, title.size=30, 
                   axis.title.x=18, axis.title.y=18, 
                   axis.text.x=15, axis.text.y=15) {
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
                             pval.size=pval.size,
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
    theme(legend.text = element_text(size = legend.text))+
    theme(plot.title = element_text(hjust = 0.5, size=title.size))+
    guides(colour = guide_legend(nrow=legend.rows))+
    theme(axis.title.x =element_text(size = axis.title.x))+
    theme(axis.title.y = element_text(size = axis.title.y))+
    theme(axis.text.x =  element_text(size=axis.text.x))+
    theme(axis.text.y =  element_text(size=axis.text.y))+
    guides(color = guide_legend(override.aes = list(shape=NA)))+
    scale_x_continuous(breaks=ticks, labels=ticks_str)
  return(p)
}

survplotcombn<-function(title="None", covs, data, groups=4, legend.coords=c(0.8,0.8), legend.rows=6, 
                        pval.coord=c(96,0.71), pval.size=6,
                        legend.text=18, title.size=30, 
                        axis.title.x=18, axis.title.y=18, 
                        axis.text.x=15, axis.text.y=15){
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
                             pval.coord=pval.coord,
                             pval.size=pval.size,
                             #risk.table = T, risk.table.title="",
                             legend=legend.coords,
                             legend.labs=unlist(labels), 
                             legend.title="", 
                             surv.scale = "percent", palette=palette,
                             #title=paste("Survival:", covars, "| Proportional=", verdict)
                             title=title)
  p<-obj$plot
  p<-p+
    theme(legend.text = element_text(size = legend.text))+
    theme(plot.title = element_text(hjust = 0.5, size=title.size))+
    guides(colour = guide_legend(nrow=legend.rows))+
    theme(axis.title.x =element_text(size = axis.title.x))+
    theme(axis.title.y = element_text(size = axis.title.y))+
    theme(axis.text.x =  element_text(size=axis.text.x))+
    theme(axis.text.y =  element_text(size=axis.text.y))+
    guides(color = guide_legend(override.aes = list(shape=NA)))+
    scale_x_continuous(breaks=ticks, labels=ticks_str)
  return(p)
}
