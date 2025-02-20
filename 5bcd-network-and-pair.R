library(igraph)
library(psych)
library(survivalAnalysis)
library(ggsurvfit)
library(survival)
library(stringr)
library(broom)
library(forestploter)
source("../res/center_igraph_edges.R")
source("../res/num-to-colors.R")
source("00-data-read-clean.R")
source("1a-functions.R")

options(scipen = 999) # turn off scientific notation


rankings<-apply(ci, 2, function(x){sum(has(x), na.rm = TRUE)})
rankings<-c(rankings, Addl.Maligs=sum(has(maligs$Additional.Malignancy)))
ranked_list<-rankings[order(rankings, decreasing = TRUE)]
ranked_list<-ranked_list[1:25]
ranked_List<-ranked_list[-c(4,11,19,22,23,24)]

#returnwd()
#IDH_remove=TRUE
#source("gbm_survival/GBM_survey_quickload.R")

disproportionality<-function(x,y,z){
  if(x>y&x>z){
    return(x/max(y,z))
  } else if (x<y&x<z) {
      return(x/min(y,z))
  } else {
      warning("Disproportionality is non-uniform")
      return(0)
    }
}

subset.make<-function(data, ...){
  out<-data[c(...)]
  out
}

subset.find<-function(data, stri){
  which(str_detect(colnames(data), str_replace(stri, " & ", "|")))
}

subset.diff<-function(data, 
                      ..., 
                      Survival.Months=basic$Survival.Months, 
                      Vital.Status=basic$Vital.Status,
                      print=F){
  #subs<-subset.make(data, subset.find(data, ...))
  subs<-subset.make(data, ...)
  name<-str_flatten(colnames(subs),collapse=" & ")
  #notNA<-apply(is.na(subs), 1, any)
  #subs<-subs[!notNA,]
  data2<-unite(subs, name, sep=" ")
  subs<-cbind(Vital.Status, Survival.Months, data2)
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
    disproportionality<-HRyy/max(HRny,HRyn)
    return(round(unname(disproportionality), digits=2))
  }
}

subset.count<-function(x1, x2){
  total<-sum(has(big[[x1]]) & has(big[[x2]]), na.rm=TRUE)
  return(if(total>10){total}else{0})
}

big<-data.frame(ci[names(ci) %in% names(ranked_List)], Addl.Maligs=maligs$Additional.Malignancy)
#rm(basic)
#rm(ci)

#big<-big[-c(1,2)]
#rankings<-apply(big, 2, function(x){sum(has(x), na.rm = TRUE)})
#ranked_list<-rankings[order(rankings, decreasing = TRUE)]
big<-big[order(apply(big, 2, function(x){sum(has(x), na.rm = TRUE)}), decreasing = TRUE)]
#sums<-sapply(names(big[-c(2,3)]), function(x){sum(has(x))})
#names(sums)<-colnames(big)[3:length(names(big))]
ll<-length(names(big))
#sums20<-sums20[order(names(sums20))]
#ci20<-big[colnames(big) %in% names(sums20)]
#ci20<-ci20[order(names(ci20))]
#ci20<-ci20[order(sums20, decreasing = T)]
#sums20<-sums20[order(sums20, decreasing = T)]

#surv20<-cbind(basic[2:3], ci20)



adj<-sapply(1:ll, function(x){sapply(1:ll, function(y){subset.diff(data=big,x,y)})})
#adj<-sapply(names(big), function(x){sapply(names(big), function(y){subset.count(x,y)})})

colnames(adj)<-colnames(big)

network<-graph_from_adjacency_matrix(adj, mode="undirected", diag=F, weighted=T, add.colnames = T)

radian.rescale <- function(x, start=0, direction=1) {
  c.rotate <- function(x) (x + start) %% (2 * pi) * direction
  c.rotate(scales::rescale(x, c(0, 2 * pi), range(x)))
}

lab.locs <- radian.rescale(x=1:length(ranked_List), direction=-1, start=0)
lab.locs[11]<- lab.locs[11]+0.5

lab.dist<-rep(1.5,19)
lab.dist[6]<- lab.dist[6]+0.5
lab.dist[12]<- lab.dist[12]+0.25
lab.dist[11]<- lab.dist[11]+0.5
lab.dist[15]<- lab.dist[15]+0.25
lab.dist[17]<- lab.dist[17]+0.5

my_labels<-names(ranked_List)
my_labels[16]<-"Tobacco"
my_labels[5]<-"T2DM"
my_labels[6]<-"Additional \n Malignancy"
my_labels[7]<-"Osteoarthritis"
my_labels[8]<-"Depression"
my_labels[11]<-"Hypo-\n thyroid"
my_labels[15]<-"Sleep \n Apnea"

setwd("../final.figures")
tiff("5b-network.tiff", height=5600, width = 5600, units="px", res=300)
plot.igraph(network, rescale=F, layout=layout.circle, 
     vertex.label=paste(my_labels, ranked_List, sep="\n"),
     #vertex.label= sums20,
     vertex.size=logb(ranked_list, b=3), 
#     edge.label=E(network)$weight, 
     edge.color=num_to_rb(E(network)$weight, 
                          min(E(network)$weight), 
                          max(E(network)$weight)),
     edge.label.color=num_to_rb(E(network)$weight, 
                                min(E(network)$weight), 
                                max(E(network)$weight)),
#             edge.label.x=center_igraph_edges(network, "x"),
#             edge.label.y=center_igraph_edges(network, "y"),
     vertex.label.cex=3,
vertex.label.dist=lab.dist,
vertex.label.degree=lab.locs,
vertex.label.family="sans",
#     edge.label.cex=0.4,
     ylim=c(-1.1,1.1), xlim=c(-1.1,1.1), asp=1, margins=10
)
dev.off()
setwd("../gbm_survival")

edgesByWeight<-as.data.frame(as_edgelist(network)[order(E(network)$weight, decreasing = T),])
edgesByWeight[3]<-sapply(edgesByWeight[1], function(x){names(big)[x]})
edgesByWeight[4]<-sapply(edgesByWeight[2], function(x){names(big)[x]})
edgesByWeight[5]<-E(network)$weight[order(E(network)$weight, decreasing = TRUE)]

#edgesRanked<-apply(edgesByWeight, c(1,2), function(x){names(big)})
#edgesRanked<-cbind(edgesRanked, E(network)$weight[order(E(network)$weight, decreasing = T)])

#subset.plot(data.frame(Survival.Months=basic$Survival.Months, Vital.Status=basic$Vital.Status, big), subset.find(big, "Obesity & Afib")+2)

#data<-data.frame(Survival.Months=basic$Survival.Months, Vital.Status=basic$Vital.Status, big)

obaf<-subset.make(ci, subset.find(ci, "Obesity & Afib"))
obaf<-data.frame(basic[c(2,3)], obaf)
obaf$Obesity<-paste(obaf$Obesity, "Obesity")
obaf$Afib<-paste(obaf$Afib, "Afib")
obaf<-obaf[!str_detect(obaf$Obesity, "NA"),]
setwd("../final.figures")
tiff("5c-surv.tiff", height=1800, width = 2700, units="px", res=300)
survplotcombn("Atrial Fibrillation & Obesity", 
              covs=c("Afib", "Obesity"), 
              data=obaf, pval.coord=c(91, 0.65),)
dev.off()
setwd("../gbm_survival")

fastpaircox2<-function(data, variables){
  covs<-variables
#return(covs)
#stop()
  flatcovs<-str_flatten(covs, collapse=" & ")
  #return(flatcovs)
#stop()
  dims<-subset.find(data, flatcovs)
  data<-data[!is.na(data[dims[1]]),]
  data[dims[1]]<-sapply(data[dims[1]], function(x){paste0(names(data[dims[1]]), x)})
  data<-data[!is.na(data[dims[2]]),]
  data[dims[2]]<-sapply(data[dims[2]], function(x){paste0(names(data[dims[2]]), x)})
  subs<-subset.make(data, dims)
  Group<-flatcovs
  notNA<-apply(is.na(subs), 1, any)
  subs<-subs[!notNA,]
  data2<-unite(subs, Group, sep=" ")
  subs<-cbind(data[!notNA,1:2],data2)
  
  uni<-coxph(Surv(Survival.Months, Vital.Status) ~ Group, data=subs)
  
  variables<-str_remove(names(uni$coefficients), "Group")
#  
  n<-sapply(variables, function(x){sum(subs$Group==x)})
#  
  cox_df<-data.frame(variable=variables,
                     n=n,
                     blank=paste(rep(" ", 20), collapse = " "),
                     est=exp(coef(uni)),
                     lower=exp(confint(uni)[,1]), 
                     upper=exp(confint(uni)[,2]), 
                     display=paste0(round(exp(coef(uni)), 2), " (",round(exp(confint(uni)[,1]),2),"-", round(exp(confint(uni)[,2]),2), ")"),
                     pval=print_pvals(tidy(uni)$p.value))
  
  names(cox_df)<-c("Variable", "n", "", "est", "lower", "upper", "HR (95% CI)", "P value")
  
  cox_df<-cox_df[order(cox_df$est),]
#View(cox_df)
  
p<-forest(cox_df[,c(1:3, 7:8)],
            est = cox_df$est,
            lower = cox_df$lower, 
            upper = cox_df$upper,
            sizes = 0.5,
            ci_column = 3,
            ref_line = 1,
            arrow_lab = c("Beneficial", "Detrimental"),
            xlim = c(0.25, 4.1),
            ticks_at = c(0.5, 1, 2,3,4))
  
  return(p)
}

setwd("../final.figures")
tiff("5d-cox.tiff", height=1800, width = 2700, units="px", res=300)
fastpaircox2(data.frame(basic[c(2,3)], ci), "Obesity & Afib")
dev.off()
setwd("../gbm_survival")

