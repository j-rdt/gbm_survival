# library
library(ggplot2)
library(dichromat)

has<-function(x){
  str_detect(x, "\\+|Y|^1")
}

# dataset:
#dist<-apply(ci, 1, function(x){sum(has(x))})
complete<-cbind(ci[-c(1,2)], maligs$Additional.Malignancy)
#dist<-apply(complete, 1, function(x){sum(has(x))})
#data<-data.frame(table(dist))
#data<-data.frame(basic$Age.factor, dist)
#data<-table(data)
#data<-melt(data)

dist<-apply(complete, 1, function(x){sum(has(x), na.rm = T)})
data=table(dist)
median(dist) #
number_of_comorbidities<-dist

comorb.factor<-function(x){
  if (x<2){
    "0-1"
  } else if (1<x&x<4) {
    "2-3"
  } else if (3<x&x<6) {
    "4-5"
  } else if (5<x&x<8) {
    "6-7"
  } else if (7<x&x<10){
    "8-9"
  } else {
    "\u226510"
  }
}

#comorb_factor<-as.factor(number_of_comorbidities)
comorb_factor<-sapply(number_of_comorbidities, comorb.factor)
comorb_factor<-factor(comorb_factor, levels=c("0-1", "2-3", "4-5", "6-7", "8-9", "\u226510"))

#Age<-basic$Age
#data<-data.frame(table(comorb_factor))

#colfunc<-colorRampPalette(c("red","yellow","springgreen","royalblue"))
colfunc<-colorRampPalette(c("red","black"))

# basic histogram
ggplot(data.frame(data), aes(y=Freq, x=dist, fill=Freq)) + 
  geom_bar(position="dodge", stat="identity", width = 0.9) +
  theme(
    axis.text.x = element_text(
      hjust = 1,
      size = 12
    )
  ) + scale_fill_gradient(high = "red", low="black", guide = NULL) +
  geom_text(aes(label=Freq), position=position_dodge(width=0.9), vjust=-0.25) +
  xlab("Number of Comorbidities") +
  ylab("Number of Patients")

maxheight=1400

length<-1500
setwd("../final.figures")
tiff("2b-hist.tiff", height=maxheight, width = length, units="px", res=300)
ggplot(data.frame(data), aes(y=Freq, x=dist, fill=Freq)) + 
  geom_bar(position="dodge", stat="identity", width = 0.9) +
  theme(
    axis.text.x = element_text(
      hjust = 1,
      size = 12
    )
  ) + scale_fill_gradient(high = "red", low="black", guide = NULL) +
  geom_text(aes(label=Freq), position=position_dodge(width=0.9), vjust=-0.25) +
  xlab("Number of Comorbidities") +
  ylab("Number of Patients")
dev.off()

setwd("../gbm_survival")
#zeros<-complete[dist==0,]
#View(zeros)

#dist.as.factor<-function(x){
#  if (x<2){
#    "0-1"
#  } else if (1<x&x<4) {
#    "2-3"
#  } else if (3<x&x<6) {
#    "4-5"
#  } else if (5<x&x<8) {
#    "6-7"
#  } else {
#    "8+"
#  }
#}
# dist.fac<-sapply(dist, dist.as.factor)
 