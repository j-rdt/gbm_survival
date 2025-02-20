# before this, create age.factor and count from 20240906 ANA plot production

count<-apply(cgn[-1], 1, sum)
basic$count<-count

library(ggplot2)
library(viridis)
library(hrbrthemes)

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
comorb_factor<-factor(comorb_factor, levels=c("\u226510", "8-9", "6-7", "4-5", "2-3", "0-1"))

agecount<-as.data.frame(table(basic$Age.factor, comorb_factor))
agecount$Freq2<-sapply(1:30, function(i){agecount$Freq[i]/sum(agecount$Freq[agecount$Var1==agecount$Var1[i]])})

#colors<-c("#FF0000", "#E00000", "#C70000", "#A80000", "#8F0000", "#700000", "#570000", "#380000", "#1F0000", "#000000")
#colors<-RColorBrewer::Spectral
#levels=0:9
# Grouped


maxheight=1400
dim=dev.size()
ratio=dim[1]/dim[2]

setwd("../final.figures")
tiff("2c-colors.tiff", height=1800, width = 1800, units="px", res=300)
ggplot(agecount, aes(y=Freq2, x=Var1, fill=comorb_factor)) + 
  geom_bar(position="stack", stat="identity", width = 0.9) +
  theme(
    axis.text.x = element_text(
      angle = 0,
      size = 12
    )#,
    #    axis.title.x=element_blank(),
  ) +
  guides(fill=guide_legend(title="Number of\nComorbidities", angle=0)) + 
  #scale_fill_manual(values=colors, breaks=levels) +
  scale_fill_brewer(
    name = waiver(),
    type = "seq",
    palette = "Spectral",
    direction = 1,
    aesthetics = "fill"
  )+
  geom_text(aes(label=ifelse(Freq>4,as.character(round(Freq2,2)), "")), position = position_stack(vjust = 0.5)) +
  ylab("Proportion of Patients") +
  xlab("Patient Age")
dev.off()

setwd("../gbm_survival")
