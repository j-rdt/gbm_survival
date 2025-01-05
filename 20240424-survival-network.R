#### survival network 20240424

source("E:/Rwd/GBMSurvey/GBM_survey_quickload.R")
rm(list=setdiff(ls(), c("ci", "basic")))
library(igraph)
source("E:/Rwd/res/center_igraph_edges.R")
source("E:/Rwd/res/num-to-colors.R")

ci<-ci[!names(ci)=="other"]
sums<-sapply(3:138, function(x){sum(ci[x]=="YES")})
names(sums)<-colnames(ci)[3:138]
sums20<-sums[sums>25]
ll<-length(sums20)
sums20<-sums20[order(names(sums20))]
ci20<-ci[colnames(ci) %in% names(sums20)]
ci20<-ci20[order(names(ci20))]
ci20<-ci20[order(sums20, decreasing = T)]
sums20<-sums20[order(sums20, decreasing = T)]

surv20<-cbind(basic[2:3], ci20)

rm(ci)

subset.make<-function(data, ...){
  out<-data[c(...)]
  out
}

subset.find<-function(data, stri){
  which(str_detect(colnames(data), str_replace(stri, " & ", "|")))
}

subset.diff<-function(data, ...){
  #if greater = T, this function will calculate the difference in median survival 
  # between the group with BOTH conditions and the single-condition group with the greater survival.
  # if great = F it does the same calculation with the single-condition group with the lesser survival
  # if greater = "both", calculate both and multiply them
  #subs<-subset.make(data, subset.find(data, ...))
  subs<-subset.make(data, ...)
  name<-str_flatten(colnames(subs),collapse=" & ")
  notNA<-apply(is.na(subs), 1, any)
  subs<-subs[!notNA,]
  data2<-unite(subs, name)
  subs<-cbind(data[!notNA,2],data2)
  colnames(subs)[1]<-"Survival.Months"
  medians<-aggregate(Survival.Months ~ name, subs, median)
  counts<-aggregate(Survival.Months ~ name, subs, length)
  form<-paste("Survival.Months", "name", sep=" ~ ")
  test<-kruskal.test(formula(form), data=as.matrix(subs))
  kruskal.p<-test$p.value
  if (kruskal.p>=0.05){
    return(0)
  } else if (any(counts[[2]]<12)){
    return(0)
  } else if (length(medians[[2]])<4) {
    print(paste("Length less than 4: ", ...))
    return(0)
    } else {
    both<-medians$Survival.Months[medians$name=="YES_YES"]
    ny<-medians$Survival.Months[medians$name=="NO_YES"]
    yn<-medians$Survival.Months[medians$name=="YES_NO"]
    dyn<-yn-both
    dny<-ny-both
    if(dny<0 & dyn <0){
      return(-1*abs(dyn*dny)) # any survival benefit compared to yes_yes is negative
    } else {
      return(dyn*dny)
    }
  }
}

subset.plot<-function(data, ..., save=F, print=T){
  subs<-subset.make(data, subset.find(data, ...))
  name<-str_flatten(colnames(subs),collapse=" & ")
  notNA<-apply(is.na(subs), 1, any)
  subs<-subs[!notNA,]
  data2<-unite(subs, name)
  subs<-cbind(data[!notNA,2],data2)
  colnames(subs)[1]<-"Survival.Months"
  medians<-aggregate(Survival.Months ~ name, subs, median)
  
  groups<-length(medians[[2]])
  counts<-aggregate(Survival.Months ~ name, subs, length)
  medians[[2]]<-paste("md=",round(medians[[2]], digits=1),"\n n=",counts[[2]],sep="")
  name2<-str_replace(name, " & ", "_")
  form<-paste("Survival.Months", "name", sep=" ~ ")
  test<-kruskal.test(formula(form), data=as.matrix(subs))
  kruskal.p<-test$p.value
  title<-paste("Grouped Survival:", name, "\n kruskal.p =", signif(kruskal.p, digits = 3))
  p<-ggplot(subs, aes(x=name, y=Survival.Months)) + 
    geom_boxplot() +
    stat_summary(fun=mean, geom="point", shape=8, size=1) + 
    geom_text(data = medians, aes(label = Survival.Months, y = -10)) +
    labs(title=title, x=name) +
    theme_classic()
  #name2<-str_replace(name, " & ", "_")
  if (save) {ggsave(paste(name2, "box.jpg", sep="_"), p, "jpeg")}
  if(print) {p}
}

adj<-sapply(3:(ll+2), function(x){sapply(3:(ll+2), function(y){subset.diff(surv20,x,y)})})
colnames(adj)<-colnames(ci20)
#adj[adj<25]<-0

hist(adj[upper.tri(adj)], main = "Histogram of group differences", 
     xlab="Group Difference", 
     sub="= (NO_YES-YES_YES)*(YES_NO-YES_YES)", 
     breaks=100)

summary(as.vector(adj[upper.tri(adj)]))


#seems like we want anything greater than 0

#adj[abs(adj)<=10]<-0

network<-graph_from_adjacency_matrix(adj, mode="undirected", diag=F, weighted=T, add.colnames = T)


plot(network, rescale=F, layout=layout.circle, 
        vertex.label=paste(names(sums20), sums20, sep="\n"),
        vertex.size=logb(sums20, b=2), 
        edge.label=E(network)$weight, 
        edge.color=num_to_rb(E(network)$weight, 
                             min(E(network)$weight), 
                             max(E(network)$weight)),
        edge.label.color=num_to_rb(E(network)$weight, 
                                   min(E(network)$weight), 
                                   max(E(network)$weight)),
#        edge.label.x=center_igraph_edges(network, "x"),
#        edge.label.y=center_igraph_edges(network, "y"),
        vertex.label.cex=0.5,
        edge.label.cex=0.4,
        ylim=c(-1,1), xlim=c(-1,1), asp=0
)


subset.plot(surv20, "CAD & HTN") # this is interesting!
#subset.plot(surv20, "GERD & Anxiety") # interesting
#subset.plot(surv20, 'GERD & BPH')
#subset.plot(surv20, "obesity & Stroke") #this is bullshit

edgesByWeight<-as_edgelist(network)[order(E(network)$weight, decreasing = T),]
edgesRanked<-apply(edgesByWeight, c(1,2), function(x){names(ci20)[x]})
edgesRanked<-cbind(edgesRanked, E(network)$weight[order(E(network)$weight, decreasing = T)])

