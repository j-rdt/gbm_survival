source("D:/Rwd/GBMSurvey/GBM_survey_quickload.R")
library(igraph)
library(psych)
library(survivalAnalysis)
library(ggsurvfit)
source("D:/Rwd/res/center_igraph_edges.R")
source("D:/Rwd/res/num-to-colors.R")

subset.make<-function(data, ...){
  out<-data[c(...)]
  out
}

subset.find<-function(data, stri){
  which(str_detect(colnames(data), str_replace(stri, " & ", "|")))
}

subset.diff<-function(data, ..., print=F, method="disproportionality"){
  #subs<-subset.make(data, subset.find(data, ...))
  subs<-subset.make(data, ...)
  name<-str_flatten(colnames(subs),collapse=" & ")
  notNA<-apply(is.na(subs), 1, any)
  subs<-subs[!notNA,]
  data2<-unite(subs, name, sep=" ")
  subs<-cbind(data[!notNA,1:2],data2)
  colnames(subs)[1:2]<-c("Vital.Status", "Survival.Months")
  medians<-aggregate(Survival.Months ~ name, subs, median)
  counts<-table(subs$name)
  form<-paste("Survival.Months + Vital Status", "name", sep=" ~ ")
  #test<-kruskal.test(formula(form), data=as.matrix(subs))
  #kruskal.p<-test$p.value
  cox_ref<-names(counts)[str_detect(names(counts), "\\-.*\\-")]
  test<-analyse_survival(subs, 
                         vars(Survival.Months, Vital.Status), 
                         by=name, 
                         cox_reference_level = cox_ref)
  logrank.p<-pluck_survival_analysis(test,"p")
  if (logrank.p>=0.05){
    return(0)
  } else if (any(counts<12)){
    return(0)
  } else if (length(medians[[2]])<4) {
    if(print) print(paste("Length less than 4: ", ...))
    return(0)
  } else {
    #both<-medians$Survival.Months[medians$name=="YES_YES"]
    #ny<-medians$Survival.Months[medians$name=="NO_YES"]
    #yn<-medians$Survival.Months[medians$name=="YES_NO"]
    #dyn<-yn-both
    #dny<-ny-both
    coefs<-exp(test$coxph$coefficients)
    HRyy<-coefs[which(str_detect(names(coefs), "\\+.*\\+"))]
    HRyn<-coefs[which(str_detect(names(coefs), "\\+.*\\-"))]
    HRny<-coefs[which(str_detect(names(coefs), "\\n.*\\+"))]
    if(method=="relative-detriment") return(round(unname(HRyy/harmonic.mean(c(HRny,HRyn))), digits=2))
    if (method=="disproportionality") return(round(HRyy-HRyn-HRny, digits=2))
  }
}

subset.count<-function(x1, x2){
  total<-sum(has(ci20[[x1]]) & has(ci20[[x2]]))
  return(if(total>10){total}else{0})
}

big<-data.frame(basic[2:3], ci)
rm(basic)
rm(ci)

big<-big[!names(big)=="other"]
sums<-sapply(names(big[-c(2,3)]), function(x){sum(str_detect(big[[x]], "\\+"))})
names(sums)<-colnames(big)[3:length(names(big))]
sums20<-sums[sums>25] # why
ll<-length(sums20)
sums20<-sums20[order(names(sums20))]
ci20<-big[colnames(big) %in% names(sums20)]
ci20<-ci20[order(names(ci20))]
ci20<-ci20[order(sums20, decreasing = T)]
sums20<-sums20[order(sums20, decreasing = T)]

surv20<-cbind(basic[2:3], ci20)



#adj<-sapply(3:(ll+2), function(x){sapply(3:(ll+2), function(y){subset.diff(surv20,x,y, method="relative-detriment")})})
adj<-sapply(names(ci20), function(x){sapply(names(ci20), function(y){subset.count(x,y)})})

colnames(adj)<-colnames(ci20)

network<-graph_from_adjacency_matrix(adj, mode="undirected", diag=F, weighted=T, add.colnames = T)


plot.igraph(network, rescale=F, layout=layout.circle, 
     vertex.label=paste(names(sums20), sums20, sep="\n"),
     #vertex.label= sums20,
     vertex.size=logb(sums20, b=3)*2, 
#     edge.label=E(network)$weight, 
     edge.color=num_to_rb(E(network)$weight, 
                          min(E(network)$weight), 
                          max(E(network)$weight)),
     edge.label.color=num_to_rb(E(network)$weight, 
                                min(E(network)$weight), 
                                max(E(network)$weight)),
#             edge.label.x=center_igraph_edges(network, "x"),
#             edge.label.y=center_igraph_edges(network, "y"),
     vertex.label.cex=1.5,
vertex.label.family="sans",
#     edge.label.cex=0.4,
     ylim=c(-1,1), xlim=c(-1,1), asp=1, margins=10
)

edgesByWeight<-as_edgelist(network)[order(E(network)$weight, decreasing = T),]
edgesRanked<-apply(edgesByWeight, c(1,2), function(x){names(ci20)[x]})
edgesRanked<-cbind(edgesRanked, E(network)$weight[order(E(network)$weight, decreasing = T)])


tiff("ana-network.tiff", height=5600, width = 5600, units="px", res=300)
# insert ggplot code
dev.off()

subset.plot(big, subset.find(big, "Obesity & Afib"))

