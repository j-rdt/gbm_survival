data<-cbind(basic[c(2,3)], ci)

split_test<-function(data){
  
  hmm<-lapply(3:length(colnames(data)), function(i){
    group<-colnames(data)[i]
    #print(group)
    form<-formula(paste("Surv(Survival.Months, Vital.Status) ~", group))
    test<-survival::survdiff(data=data, formula=form)
    c(group,
      as.double(test$pvalue),
      as.numeric(test$n[1][[1]]),
      as.numeric(test$n[2][[1]]), 
      as.numeric(test$chisq))
  })
  hmm<-as.data.frame(hmm)
  hmm<-t(hmm)
  hmm<-as.data.frame(hmm)
  yum<-hmm$V1
  hmm<-unname(hmm)
  hmm<-as.data.frame(apply(hmm[-1], 2, as.numeric))
  rownames(hmm)<-yum
  hmm$V5<-abs(as.numeric(hmm$V2)-as.numeric(hmm$V3))
  
  return(hmm)
}

level1<-split_test(ci)
View(level1)

data<-data
htnp<-ci[ci$htn=="+HTN",]
htnn<-ci[ci$htn=="-HTN",]
level2<-split_test(cbind(basic[c(2,3),],htnp))
