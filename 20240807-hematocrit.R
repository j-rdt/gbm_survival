#geom_violin()

oayy<-read.csv('20240807results/oayy_result.csv')
oayn<-read.csv('20240807results/oayn_result.csv')
oany<-read.csv('20240807results/oany_result.csv')

yy<-data.frame(as.numeric(oayy$X.1), rep('Both', length(oayy$X.1)))
names(yy)<-c('Hematocrit', 'Group')
yn<-data.frame(as.numeric(oayy$X.1), rep('Obesity', length(oayn$X.1))) 
names(yn)<-c('Hematocrit', 'Group')
ny<-data.frame(as.numeric(oany$X.1), rep('Afib', length(oany$X.1))) 
names(ny)<-c('Hematocrit', 'Group')

gtable<-rbind(yy,yn,ny)
gtable<-gtable[-is.na(gtable$Hematocrit),]

library(ggstatsplot)

plt <- ggbetweenstats(
  data = gtable,
  x = Group,
  y = Hematocrit, 
  p.adjust.method = 'BY',
  results.subtitle=FALSE, 
  title="Hematocrit values",
  digits=1,
)
plt
# hematocrit values need to be edited: all blanks need to be filled in directly from the medical record
