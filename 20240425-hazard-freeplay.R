## trying to understand hazards 20240425

subs<-subset.make(surv20, subset.find(surv20, "obesity & Afib"))
name<-str_flatten(colnames(subs),collapse=" & ")
notNA<-apply(is.na(subs), 1, any)
subs<-subs[!notNA,]
data2<-unite(subs, name)
subs<-cbind(surv20[!notNA,1:2],data2)
colnames(subs)[1:2]<-c("Vital.Status", "Survival.Months")
medians<-aggregate(Survival.Months ~ name, subs, median)
counts<-table(subs$name)
form<-paste("Survival.Months + Vital Status", "name", sep=" ~ ")



analyse_survival(subs, 
                 vars(Survival.Months, Vital.Status), 
                 by=name, 
                 cox_reference_level = "NO_NO") %>%
  forest_plot(
              endpoint_labeller = c(Survival.Months="Survival Months"),
              orderer = ~order(abs(log(HR)), decreasing = T),
              labels_displayed = c("endpoint","factor", "n"),
              ggtheme = ggplot2::theme_bw(base_size = 10))

adj<-sapply(3:(ll+2), function(x){sapply(3:(ll+2), function(y){subset.diff(surv20,x,y)})})
colnames(adj)<-colnames(ci20)

hist(adj[upper.tri(adj)], main = "Histogram of RD", 
     xlab="RD", 
     sub="= HRyy/harmonic.mean(c(HRny,HRyn))", 
     breaks=100)

summary(as.vector(adj[upper.tri(adj)]))

hist(adj[upper.tri(adj) & adj!=0], main = "Histogram of disproportionality !=0", 
     xlab="Disproportionality", 
     sub="= HRyy-HRyn-HRnn", 
     breaks=100)
