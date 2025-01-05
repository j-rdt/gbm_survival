source("E:/Rwd/GBMSurvey/GBM_survey_quickload.R")

nvar<-function(inds, data){
  return(list(inds=inds, num=length(inds), data=data))
}

getelems<-function(l,n){
  sapply(l, `[[`, n)
}
#newm3<-newm2[-c(2,3)]
#######################################     1

subset.make<-function(data, ...){
  out<-data[c(...)]
  out
}

subset.find<-function(data, stri){
  which(str_detect(colnames(data), str_replace(stri, " & ", "|")))
}

subset.test<-function(data, ...){
  subs<-subset.make(data, ...)
  name<-str_flatten(colnames(subs),collapse=" & ")
  notNA<-apply(is.na(subs), 1, any)
  data<-subs[!notNA,]
  data2<-unite(subs, name)
  subs<-cbind(data[!notNA,1],data2)
  colnames(subs)[1]<-"Survival.Months"
  means<-aggregate(Survival.Months ~ name, subs, mean)
  groups<-length(means[[2]])
  dominance<-sum(max(means[[2]])-means[[2]])/groups-1
  counts<-aggregate(Survival.Months ~ name, subs, length)
  unevenness<-sd(counts[[2]])
  means[[2]]<-paste("mean=",round(means[[2]], digits=1),"\n n=",counts[[2]],sep="")
  name2<-str_replace(name, " & ", "_")
  colnames(subs)[2]<-name2
  form<-paste("Survival.Months", name2, sep=" ~ ")
  test<-kruskal.test(formula(form), data=as.matrix(subs))
  kruskal.p<-test$p.value
  kruskal.chi<-test$statistic[[1]]
  kruskal.df<-test$parameter[[1]]
  #list(name=name, 
  #     groups=groups,
  #     kruskal.chi=kruskal.chi,
  #     kruskal.df=kruskal.df,
  #     kruskal.p=kruskal.p,
  #     dominance=dominance,
  #    unevenness=unevenness)
  return(data2[[1]])
}

subset.plot<-function(data, ..., save=F, print=T){
  if(any(sapply(c(...), is.character))){
    subs<-subset.make(data, subset.find(data, ...))
  } else {
    subs<-subset.make(data, ...)
  }
  #data<-subset.make(subset.find(stri))
  name<-str_flatten(colnames(subs),collapse=" & ")
  notNA<-apply(is.na(subs), 1, any)
  subs<-subs[!notNA,]
  subs<-unite(subs, name)
  subs<-cbind(data[!notNA,1],subs)
  colnames(subs)[1]<-"Survival.Months"
  means<-aggregate(Survival.Months ~ name, subs, mean)
  groups<-length(means[[2]])
#  dominance<-sum(max(means[[2]])-means[[2]])/groups-1
  counts<-aggregate(Survival.Months ~ name, subs, length)
#  unevenness<-sd(counts[[2]])
  means[[2]]<-paste("m=",round(means[[2]], digits=1),"\n n=",counts[[2]],sep="")
  name2<-str_replace(name, " & ", "_")
#  colnames(data)[2]<-name2
  form<-paste("Survival.Months", "name", sep=" ~ ")
  test<-kruskal.test(formula(form), data=as.matrix(subs))
  kruskal.p<-test$p.value
  title<-paste("Grouped Survival:", name, "\n kruskal.p =", signif(kruskal.p, digits = 3))
  p<-ggplot(subs, aes(x=name, y=Survival.Months)) + 
    geom_boxplot() +
    stat_summary(fun=mean, geom="point", shape=8, size=1) + 
    geom_text(data = means, aes(label = Survival.Months, y = -10)) +
    labs(title=title, x=name) +
    theme_classic()
  #name2<-str_replace(name, " & ", "_")
  if (save) {ggsave(paste(name2, "box.jpg", sep="_"), p, "jpeg")}
  if(print) {p}
  
}
combos<-combn(2:21,2)
test<-mapply(subset.test, combos[1,], combos[2,])
test2<-data.frame(test)
test2<-t(test2)
test2<-data.frame(test2)
test2<-sapply(test2, unlist)
test2<-data.frame(test2)
test2[2:7]<-sapply(test2[2:7], as.numeric)
test2<-cbind(test2, 1:length(test2[,1]))
colnames(test2)[8]<-"label"
max(test2$dominance)

p<-ggplot(test2, aes(dominance, -log(kruskal.p), color = unevenness)) +
  geom_text((aes(label=label))) + 
  geom_hline(yintercept = 4.605, linetype="dashed") +
  geom_vline(xintercept = 13, linetype="dashed")
  
p

test2_coh1<-test2[-log(test2$kruskal.p)>4.605 & test2$dominance>13,]
test2_coh2<-test2[-log(test2$kruskal.p)>47 & test2$dominance<=13,]

setwd("E:/Rwd/GBMSurvey/20240306plots")

sapply(test2_coh1$name, function(x){subset.plot(x, save=T, print=F)})

setwd("E:/Rwd/GBMSurvey/20240228plots/lowp")

sapply(test2_coh2$name, function(x){subset.plot(x, save=T, print=F)})
######################################     2

newcols<-sapply(2:19, function(lx){
  sapply(2:19, function(ly){
    if(lx==ly){NA} else {
      e<-as.data.frame(mapply(function(x,y){paste(x, y, sep=":")}, 
                 newm3[lx], newm3[ly]))
      names(e)<-mapply(function(x,y){paste(x, y, sep=":")}, 
                       colnames(newm3)[lx], colnames(newm3[ly]))
      e
      }
    }, simplify = F)
  }, simplify = F)
newcols<-newcols[-which(sapply(newcols, is.null))]

combin.test<-function(obj){
  thing<-lapply(2:, function(x){
    name<-colnames(newm3)[x]
    form<-paste("Survival.Months", name, sep=" ~ ")
    test<-kruskal.test(formula(form), data=as.matrix(newm2))
    list(p.value=test$p.value, name=name)
  })
}