# functions only 

source("E:/Rwd/GBMSurvey/GBM_survey_quickload.R")

nvar<-function(inds, data){
  return(list(inds=inds, num=length(inds), data=data))
}

getelems<-function(l,n){
  sapply(l, `[[`, n)
}
#newm3<-newm2[-c(2,3)]
#######################################     1

subset.make<-function(...){
  out<-newm2[c(...)]
}

subset.find<-function(stri){
  which(str_detect(colnames(newm2), str_replace(stri, " & ", "|")))
}

subset.test<-function(...){
  data<-subset.make(...)
  name<-str_flatten(colnames(data),collapse=" & ")
  notNA<-apply(is.na(data), 1, any)
  data<-data[!notNA,]
  data<-unite(data, name)
  data<-cbind(newm2[!notNA,1],data)
  colnames(data)[1]<-"Survival.Months"
  means<-aggregate(Survival.Months ~ name, data, mean)
  groups<-length(means[[2]])
  dominance<-sum(max(means[[2]])-means[[2]])/groups-1
  counts<-aggregate(Survival.Months ~ name, data, length)
  unevenness<-sd(counts[[2]])
  means[[2]]<-paste("mean=",round(means[[2]], digits=1),"\n n=",counts[[2]],sep="")
  name2<-str_replace(name, " & ", "_")
  colnames(data)[2]<-name2
  form<-paste("Survival.Months", name2, sep=" ~ ")
  test<-kruskal.test(formula(form), data=as.matrix(data))
  kruskal.p<-test$p.value
  kruskal.chi<-test$statistic[[1]]
  kruskal.df<-test$parameter[[1]]
  list(name=name, 
       groups=groups,
       kruskal.chi=kruskal.chi,
       kruskal.df=kruskal.df,
       kruskal.p=kruskal.p,
       dominance=dominance,
       unevenness=unevenness)
}

subset.plot<-function(..., save=F, print=T){
  if(any(sapply(c(...), is.character))){
    data<-subset.make(subset.find(...))
  } else {
    data<-subset.make(...)
  }
  #data<-subset.make(subset.find(stri))
  name<-str_flatten(colnames(data),collapse=" & ")
  notNA<-apply(is.na(data), 1, any)
  data<-data[!notNA,]
  data<-unite(data, name)
  data<-cbind(newm2[!notNA,1],data)
  colnames(data)[1]<-"Survival.Months"
  means<-aggregate(Survival.Months ~ name, data, mean)
  groups<-length(means[[2]])
  #  dominance<-sum(max(means[[2]])-means[[2]])/groups-1
  counts<-aggregate(Survival.Months ~ name, data, length)
  #  unevenness<-sd(counts[[2]])
  means[[2]]<-paste("m=",round(means[[2]], digits=1),"\n n=",counts[[2]],sep="")
  name2<-str_replace(name, " & ", "_")
  #  colnames(data)[2]<-name2
  form<-paste("Survival.Months", "name", sep=" ~ ")
  test<-kruskal.test(formula(form), data=as.matrix(data))
  kruskal.p<-test$p.value
  title<-paste("Grouped Survival:", name, "\n kruskal.p =", signif(kruskal.p, digits = 3))
  p<-ggplot(data, aes(x=name, y=Survival.Months)) + 
    geom_boxplot() +
    stat_summary(fun=mean, geom="point", shape=8, size=1) + 
    geom_text(data = means, aes(label = Survival.Months, y = -10)) +
    labs(title=title, x=name) +
    theme_classic()
  #name2<-str_replace(name, " & ", "_")
  if (save) {ggsave(paste(name2, "box.jpg", sep="_"), p, "jpeg")}
  if(print) {p}
  
}

subset.analyze<-function(..., print=T, save=T, ret=T){
  if(any(sapply(c(...), is.character))){
    data<-subset.make(subset.find(...))
  } else {
    data<-subset.make(...)
  }
  #data<-subset.make(subset.find(stri))
  name<-str_flatten(colnames(data),collapse=" & ")
  notNA<-apply(is.na(data), 1, any)
  data<-data[!notNA,]
  data<-unite(data, name)
  data<-cbind(newm2[!notNA,1],data)
  colnames(data)[1]<-"Survival.Months"
  means<-aggregate(Survival.Months ~ name, data, mean)
  groups<-length(means[[2]])
  #  dominance<-sum(max(means[[2]])-means[[2]])/groups-1
  counts<-aggregate(Survival.Months ~ name, data, length)
  #  unevenness<-sd(counts[[2]])
  means[[2]]<-paste("m=",round(means[[2]], digits=1),"\n n=",counts[[2]],sep="")
  name2<-str_replace(name, " & ", "_")
  #  colnames(data)[2]<-name2
  form<-paste("Survival.Months", "name", sep=" ~ ")
  test<-kruskal.test(formula(form), data=as.matrix(data))
  kruskal.p<-test$p.value
  title<-paste("Grouped Survival:", name, "\n kruskal.p =", signif(kruskal.p, digits = 3))
  p<-ggplot(data, aes(x=name, y=Survival.Months)) + 
    geom_boxplot() +
    stat_summary(fun=mean, geom="point", shape=8, size=1) + 
    geom_text(data = means, aes(label = Survival.Months, y = -10)) +
    labs(title=title, x=name) +
    theme_classic()
  #name2<-str_replace(name, " & ", "_")
  if(print) {  
    plot(p)
    save<-readline("Save image with current dimensions?")=="y"
  }
  if (save) {ggsave(paste(name2, "box.jpg", sep="_"), p, "jpeg")}
  colnames(data)[2]<-name2
  if (ret) {return(factor(data[[2]]))}
}
HTN_HLD<-subset.analyze("HTN & HLD")
HTN_HLD<-revalue(HTN_HLD, c("NO_NO"="NEITHER", "NO_YES"="HLD", "YES_NO"="HTNwoHLD", "YES_YES"="HLD"))
newm2$HTN_HLD<-HTN_HLD

analyse_survival(newm2,
                     vars(Survival.Months, Vital.Status),
                     by=HTN_HLD
                     reference_level_dict = fac_ref
                     ) %>%
  forest_plot(factor_labeller = all_pcomorbs_n,
              use_one_hot = T,
              endpoint_labeller = c(Survival.Months="Survival Months"),
              orderer = ~order(abs(log(HR)), decreasing = T),
              labels_displayed = c("endpoint","factor", "n"),
              ggtheme = ggplot2::theme_bw(base_size = 10))

analyse_multivariate(newm2,
                     vars(Survival.Months, Vital.Status),
                     covariates = vars(HTN, HLD, obesity, diabetes), # covariates expects a list
                     covariate_name_dict = all_pcomorbs_n,
                     reference_level_dict = fac_ref) %>%
  forest_plot(factor_labeller = all_pcomorbs_n,
              endpoint_labeller = c(Survival.Months="Survival Months"),
              orderer = ~order(abs(log(HR)), decreasing = T),
              labels_displayed = c("endpoint","factor", "n"),
              ggtheme = ggplot2::theme_bw(base_size = 10))


newmage[4:12]<-sapply(newmage[4:12], as.vector)


map(sapply(colnames(newmage[4:12]), function(x){expr(!!x)}), function(by)
{
  analyse_multivariate(newmage,
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
