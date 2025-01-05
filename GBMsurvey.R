### GBM survey study

library(ARTool)
library(xlsx)
library(dplyr)
library(stringr)
library(rms)
library(cluster)
library(data.table)
library(tidyr)
library(ggplot2)

getwd()
setwd("E:/Rwd/GBMSurvey")



#'
cgn<-read.xlsx("GBM_analysis.xlsx", 5) # 1 = NO; 2=YES
cg<-cgn
cgn[-1]<-apply(cgn[-1], 2, function(x){replace(x, x==1, 0)})
cgn[-1]<-apply(cgn[-1], 2, function(x){replace(x, x==2, 1)})

cg[-1]<-apply(cg[-1], 2, function(x){replace(x, x==1, "NO")})
cg[-1]<-apply(cg[-1], 2, function(x){replace(x, x==2, "YES")})
#cg[-1]<-lapply(cg[-1], as.factor)

ci<-read.xlsx("GBM_analysis.xlsx", 6) #normal 
ci[-c(1,2)]<-apply(ci[-c(1,2)], 2, function(x){replace(x, x==1, "YES")})
ci[-c(1,2)]<-apply(ci[-c(1,2)], 2, function(x){replace(x, x==0, "NO")})

#bigi<-cbind(basic[-1], trt[-1], ci[-1])

#ci[-1]<-apply(ci[-1], 2, function(x){replace(x, x==2, 0)})

trt<-read.xlsx("GBM_analysis_nodates.xlsx", 4) # empty, 0 = NO; 1, date = yes; # cycles is an int
#trt[2]<-replace(trt[2], trt[2]>0, 1)
trt[-1]<-apply(trt[-1], 2, function(x){replace(x, x>43, 1)})
#trt<-apply(trt, 2, function(x){replace(x, class(x)=="Date"&x==0, numeric(0))})
#trt<-as.data.frame(apply(trt, 2, function(x){replace(x, str_detect(x, "-"), 1)}))
extents<-which(str_detect(names(trt), "extent"))
trt[extents]<-
  apply(trt[extents], 2, function(x){
    replace(x, x==1, "Subtotal")})
trt[extents]<-
  apply(trt[extents], 2, function(x){
    replace(x, x==2, "Gross/Near Gross")})

trt[3]<-replace(trt[3], trt[2]==1&is.na(trt[3]), "Unknown")
trt[3]<-replace(trt[3], trt[2]==0&trt[3]==0, "Unknown")
trt[11]<-replace(trt[11], trt[10]==1&is.na(trt[11]), "Unknown")
trt[15]<-replace(trt[15], trt[14]==1&is.na(trt[15]), "Unknown")

trt[2]<-replace(trt[2], trt[2]==0, "NO")
trt[2]<-replace(trt[2], trt[2]==1, "YES")
trt[4]<-replace(trt[4], trt[4]==0, "NO")
trt[4]<-replace(trt[4], trt[4]==1, "YES")
trt[10]<-replace(trt[10], trt[10]==0, "NO")
trt[10]<-replace(trt[10], trt[10]==1, "YES")
trt[14]<-replace(trt[14], trt[14]==0, "NO")
trt[14]<-replace(trt[14], trt[14]==1, "YES")

which(str_detect(names(trt), "cycle|MRN"))

trt[2:7]<-apply(trt[2:7], 2, factor)
trt[9:21]<-apply(trt[9:21], 2, factor)
trt[23:60]<-apply(trt[23:60], 2, factor)
trt[62:74]<-apply(trt[62:74], 2, factor)

basic<-read.xlsx("GBM_analysis.xlsx", 3)
basic<-basic[-7]
#basic[5]<-replace(basic[5], basic[5]==0|is.na(basic[5]), "Unknown")
basic[5]<-replace(basic[5], basic[5]==0, NA)
basic[5]<-replace(basic[5], basic[5]==1, "NO") #Methylated
basic[5]<-replace(basic[5], basic[5]==2, "YES") # Unmethylated
#basic[6]<-replace(basic[6], basic[6]==0|is.na(basic[6]), "Unknown")
basic[6]<-replace(basic[6], basic[6]==0, NA)
basic[6]<-replace(basic[6], basic[6]==1, "NO") # WT
basic[6]<-replace(basic[6], basic[6]==2, "YES") #Mutant
names(basic)[5:6]<-c("MGMT.UME", "IDH.MUT")


#basic[7]
#basic[7]<-as.data.frame(mapply(function(x,y){paste(x, y, sep=".")}, 
#                 basic[5], basic[6], USE.NAMES = FALSE))
#names(basic)[7]<-"MGMT.IDH"
#basic<-basic[-c(5,6)] #need to write a function that replaces strings containing NA with NA 
nastri<-function(dfdollar, what="both"){
  if (what=="both"){
    str_detect(dfdollar, "NA\\.|\\.NA")
  } else if (what=="IDH") {
    str_detect(dfdollar, "\\.NA")
  } else if (what=="MGMT") {
    str_detect(dfdollar, "NA\\.")
  } else {
    print("Invalid input to 'what', returning complete indices")
    rep(1:length(dfdollar))
    }
}
#basic2<-basic[!nastri(basic$MGMT.IDH),]
#basic$MGMT.IDH<-factor(basic$MGMT.IDH)
basic$Survival.Months<-as.numeric(basic$Survival.Months)
#basic<-apply(basic, 2, function(x){replace(x, is.na(x), "Unknown")})

#basic<-droplevels(basic[basic$MGMT!='Unknown',])
#basic<-droplevels(basic[basic$IDH!='Unknown',])

################################################
##### ARTOOL #####################################
################################################

#newm<-newm[-2]
#str(newm)
#form<-paste("Survival.Months ~", str_flatten(names(newm)[-1], collapse="*"))
form<-formula(form)

artm<-art(formula(form), data=newm3)

test<-data.frame(basic$Survival.Months, 
      basic$MGMT.IDH, 
      as.factor(trt$Initial.Max.Resection.), 
      as.factor(cg$metabolic.syndrome), 
      as.factor(cg$upper.GI))

test<-test[!nastri(basic$MGMT.IDH),]

names(test)<-c("Survival.Months", "MGMT.IDH", 
               "InitMaxResect", 
               "MetabolicSyn", "UpperGI")

test<-test[!is.na(test$InitMaxResect),]#&!is.na(test$InitChemoWRad),]
test<-na.omit(test)
#form<-paste("Survival.Months ~", str_flatten(names(test)[-1], collapse="*"))
form<-paste("Survival.Months ~", str_flatten(names(test)[c(2,4)], collapse="*"))
form<-formula(form)

artm<-art(form, data=test)

for (i in 2:4){
  for (j in 3:5){
    if (i!=j){
      print(names(test)[i])
      print(names(test)[j])
      #print(chisq.test(table(as.list(test[i], as.list(test[j])))))
      print(fisher.test(test[c(i,j)]))
    }
  }
}

#####################################################################
##### Clustering ####################################################
#####################################################################


setwd("E:/Rwd/GBMSurvey/20240221plots")

#cgn<-sapply(cg, function(x){sum(x=="YES")})
#cin<-sapply(ci, function(x){sum(x=="YES")})

newm<-cbind(basic$Survival.Months, 
            basic$Age, 
            ci$BMI,
            basic$MGMT,
            basic$IDH,
            trt$Initial.Max.Resection., 
            trt$Initial.Chemo.w.radiation, 
            cg[-c(1,16)],
            ci[c(24,25,107)])
#newm<-replace(newm, is.na(newm), "NODATA")
names(newm)[1:7]<-c("Survival.Months", 
                    "Age",
                    "BMI",
                    "MGMT.UME",
                    "IDH.MUT",
                    "InitMaxRes",
                    "InitChemoRad")

counts<-sapply(newm[-c(1:7)], function(x){sum(x=="YES")})

title<-"Patients by Comorbidity"
p<-ggplot(data=data.frame(x=names(counts), y=counts), aes(x=reorder(x,-y),y)) + 
  geom_bar(stat="identity") + 
  labs(title=title, 
       y="Number of Patients", x=NULL) +
  geom_text(aes(label=y), vjust=-0.3, size=3.5) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
#  theme_minimal()
plot(p)
ggsave(paste(title, "bar.jpg", sep="_"), p, "jpeg")

newm<-newm[c(1:7, which(counts>30)+7)]

newm$Age<-sapply(newm$Age, function(x){
  if (x<40){
    "<40"
  } else if (40<=x&x<50){
    "40-49"
  } else if (50<=x&x<59){
    "50-59"
  } else if (60<=x&x<69){
    "60-69"
  } else if (70<=x&x<79){
    "70-79"
  } else if (80<=x&x<89){
    "80-89"
    } else {">90"}
     })


newm$BMI<-sapply(newm$BMI, function(x){
  if (is.na(x)){
    NA
  } else if (x<20){
    "<20"
  } else if (20<=x&x<24){
    "20-24"
  } else if (24<=x&x<28){
    "24-28"
  } else if (28<=x&x<32){
    "28-32"
  } else if (32<=x&x<36){
    "32-36"
  } else if (x=="NODATA"){
    "NODATA"
  } else {">36"}
})

newm2<-newm
newm2[-1]<-lapply(newm2[-1], factor)

newm2<-na.omit(newm)
lvls<-c("YES", "NO")
newm2[-c(1,2,3)]<-lapply(newm2[-c(1,2,3)], factor, levels=lvls)
newm3<-newm2[-c(2,3)]

newm<-replace(newm, is.na(newm), "NODATA")
lvls<-c("YES", "NO", "NODATA")
newm[-c(1,2,3)]<-lapply(newm[-c(1,2,3)], factor, levels=lvls)

rowpts<-newm2
rowcomorbs<-transpose(newm2[-1])
rowcomorbs<-as.data.frame(lapply(rowcomorbs, as.factor))
rownames(rowcomorbs)<-colnames(newm)[-1]
rowcomorbs<-rowcomorbs[-c(1,2,3,4,5,6),]

distp <- daisy(rowpts[-c(1,2,3,4,5,6,7)], metric = "gower")
distc <- daisy(as.data.frame(rowcomorbs), metric = "gower")

cls <- hclust(distp)
cls2<-cutree(cls, 1:4)


title<-"Similarities among comorbidities"
cls <- hclust(distc)
jpeg(paste(title, "jpg", sep="."), width=9.56, height=9.57, units = "in", res=565)
plot(cls, main=title, xlab="Comorbidity", ylab="Distance")
dev.off()

library(ggplot2)
plotpts<-rowpts[-c(1,2,3,4,5,6,7)]
plotpts<-plotpts[cls$order]
col.order<-colnames(rowpts[-c(1,2,3,6,7)])[cls$order]
plotpts<-melt(data.frame(pt=1:dim(plotpts)[1], plotpts), id.vars = 'pt')
title<-"comorbidities count"
p<-ggplot(plotpts, aes(variable, pt)) + 
  geom_tile(aes(fill=value), colour = "white") + 
  scale_fill_manual(values=c("green", "red", "black")) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
plot(p)
ggsave(paste(title, "heatmap.jpg", sep="_"), p, "jpeg")



dist_mat<-as.matrix(distc)
dist_mat<-dist_mat[col.order,col.order]
dist_melt<-melt(dist_mat)
title<-"Dissimilarity Matrix of Comorbidities"
p<-ggplot(dist_melt, aes(Var1, Var2, fill=value)) + 
  geom_tile() + 
  labs(title=title, 
       y=NULL, x=NULL) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
plot(p)
ggsave(paste(title, "dismat.jpg", sep="_"), p, "jpeg")

###################################################################

install.packages("npmv")
library(npmv)
getwd()
form<-paste(colnames(newm2)[1], 
            str_flatten(colnames(newm2)[4:6], collapse=" * "), 
            sep=" ~ ")
test<-ssnonpartest(formula(form), data=newm2)
test<-nonpartest(formula(form), data=newm2, releffects=T)








cls2<-kmeans(distp, 4)

k.max <- 25
wss <- sapply(1:k.max, 
              function(k){kmeans(distp, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

cls5<-kmeans(distp, 5, nstart=50,iter.max = 15 )
cls2<-kmeans(distp, 2, nstart=50,iter.max = 15 )
cls3<-kmeans(distp, 3, nstart=50,iter.max = 15 )
cls4<-kmeans(distp, 4, nstart=50,iter.max = 15 )
cls6<-kmeans(distp, 6, nstart=50,iter.max = 15 )



colns<-colnames(rowpts)

rowpts2<-cbind(cls2$cluster,
               cls3$cluster,
               cls4$cluster,
               cls5$cluster,
               cls6$cluster, rowpts)

boxplot(cbind(rowpts2$Survival.Months[rowpts2$`cls6$cluster`==1], 
              rowpts2$Survival.Months[rowpts2$`cls6$cluster`==2],
              rowpts2$Survival.Months[rowpts2$`cls6$cluster`==3],
              rowpts2$Survival.Months[rowpts2$`cls6$cluster`==4], 
              rowpts2$Survival.Months[rowpts2$`cls6$cluster`==5],
              rowpts2$Survival.Months[rowpts2$`cls6$cluster`==6]))

for (i in 1:5)  {
  for (j in (i+1):6) {
    if (i != j) {
      print(paste(i, " ", j))
      print(t.test(rowpts2$Survival.Months[rowpts2$`cls6$cluster`==i], rowpts2$Survival.Months[rowpts2$`cls6$cluster`==j])$p.value)      }
  } 
    }
}


boxplot(cbind(rowpts2$Survival.Months[rowpts2$`cls5$cluster`==1], 
              rowpts2$Survival.Months[rowpts2$`cls5$cluster`==2],
              rowpts2$Survival.Months[rowpts2$`cls5$cluster`==3],
              rowpts2$Survival.Months[rowpts2$`cls5$cluster`==4], 
              rowpts2$Survival.Months[rowpts2$`cls5$cluster`==5]))

boxplot(cbind(rowpts2$Survival.Months[rowpts2$`cls4$cluster`==1], 
              rowpts2$Survival.Months[rowpts2$`cls4$cluster`==2],
              rowpts2$Survival.Months[rowpts2$`cls4$cluster`==3],
              rowpts2$Survival.Months[rowpts2$`cls4$cluster`==4]))


boxplot(cbind(rowpts2$Survival.Months[rowpts2$`cls3$cluster`==1], 
              rowpts2$Survival.Months[rowpts2$`cls3$cluster`==2],
              rowpts2$Survival.Months[rowpts2$`cls3$cluster`==3]))

boxplot(cbind(rowpts2$Survival.Months[rowpts2$`cls2$cluster`==1], 
              rowpts2$Survival.Months[rowpts2$`cls2$cluster`==2]))



















rowptsnum<-

rowpts2 %>% mutate(Cluster2=rowpts2$`cls2$cluster`) %>% group_by(Cluster2) 

plot(cls2)

c5<-newmmm[newmmm$`cls5$cluster`==5,]
c4<-newmmm[newmmm$`cls5$cluster`==4,]
c3<-newmmm[newmmm$`cls5$cluster`==3,]
c2<-newmmm[newmmm$`cls5$cluster`==2,]
c1<-newmmm[newmmm$`cls5$cluster`==1,]

sink("c5.txt")
print(summary(c5))
sink()
sink("c4.txt")
print(summary(c4))
sink()
sink("c3.txt")
print(summary(c3))
sink()
sink("c2.txt")
print(summary(c2))
sink()
sink("c1.txt")
print(summary(c1))
sink()


compare_them <- function(d1,d2,d3,d4,d5) {
  sum1 <- apply(d1,2,summary) %>% data.frame() 
  sum2 <- apply(d2,2,summary) %>% data.frame()
  sum3 <- apply(d3,2,summary) %>% data.frame() 
  sum4 <- apply(d4,2,summary) %>% data.frame()
  sum5 <- apply(d5,2,summary) %>% data.frame() 
  
  names(sum1) <- paste0(names(sum1),"1")
  names(sum2) <- paste0(names(sum2),"2")
  names(sum3) <- paste0(names(sum1),"3")
  names(sum4) <- paste0(names(sum1),"4")
  names(sum5) <- paste0(names(sum1),"5")
  
  final <- cbind(sum1,sum2,sum3,sum4,sum5)
  
  final1 <- t(final) 
  
  final2 <- final1[order(row.names(final1)), ]
  
  final_1 <- t(final2) %>% data.frame()
  final_1
}


