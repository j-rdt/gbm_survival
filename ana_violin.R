data=data.frame(Age=basic$Age, 
                CCI=cci_categories)

dist<-apply(ci, 1, function(x){sum(has(x))})
data=data.frame(Number=dist, 
                CCI=cci_no_age)

#means<-sapply(unique(cci_categories), function(x){median()})


ggplot(data, aes(y=CCI, x=Age, fill=CCI)) + 
  stat_boxplot(geom = "errorbar",
               width = 0.5) + 
  geom_violin() +
  geom_boxplot(width=.25) +
#  geom_text(data = means, aes(label = Age, y = -10)) +
#  labs(x=count) +
  theme(plot.title = element_text(hjust = 0.5, size=20))+
  theme(axis.title.x = element_text(size = 12))+
  theme(axis.title.y = element_text(size = 12))+
  theme(axis.text.x =  element_text(size=10))+
  theme(axis.text.y =  element_text(size=10)) +
  scale_y_discrete(limits=rev) +
  xlab("Age") +
  ylab("CCI") +
  scale_fill_manual(values=c("#7071ff", "grey", "#ff7070"))

tiff("cci-v-age.tiff", height=600, width = 1800, units="px", res=300)
# insert ggplot code
dev.off()

n_comorb<-sapply(dist, function(x){
  if (x<1){
    "0"
  } else if (x<2){
    "1"
  } else if (x<3) {
    "2"
  } else {
    "3+"
  }
})
