
counts<-table(basic$count)
means<-aggregate(Survival.Months ~ count, basic, median)
means[[2]]<-paste("median=",round(means[[2]], digits=1),"\n n=",counts,sep="")

ggplot(basic, aes(x=count, y=Survival.Months)) + 
  stat_boxplot(geom = "errorbar",
               width = 0.25) + 
  geom_boxplot() +
  geom_text(data = means, aes(label = Survival.Months, y = -10)) +
  labs(x=count) +
  theme(plot.title = element_text(hjust = 0.5, size=20))+
  theme(axis.title.x = element_text(size = 12))+
  theme(axis.title.y = element_text(size = 12))+
  theme(axis.text.x =  element_text(size=10))+
  theme(axis.text.y =  element_text(size=10)) +
  ylab("Survival Months") +
  xlab("Number of Comorbidities")

tiff("ana-comorbs-2-box.tiff", height=1400, width = 1600, units="px", res=300)
# insert ggplot code
dev.off()
