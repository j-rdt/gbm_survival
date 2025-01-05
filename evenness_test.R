mynames<-names(ci)[-1]
mynames<-names(ci)[!names(ci)=="other"]

#

chi_p<-data.frame(c1=rep(mynames, each=136), 
                  c2=rep(mynames, 136))
chi_p$pval<-mapply(function(x,y){
  chisq.test(ci[[x]], ci[[y]])$p.value}, 
              chi_p$c1, 
              chi_p$c2)

chi_p$std<-mapply(function(x,y){
  sd(table(ci[[x]], ci[[y]]))}, 
  chi_p$c1, 
  chi_p$c2)

chi_p$evenness<-mapply(function(x,y){
  tab<-table(ci[[x]], ci[[y]])
  abs(tab[1,1]-tab[1,2])+
    abs(tab[1,1]-tab[2,1])+
    abs(tab[1,1]-tab[2,2])+
    abs(tab[1,2]-tab[2,1])+
    abs(tab[1,2]-tab[2,1])+
    abs(tab[1,2]-tab[2,2])+
    abs(tab[2,1]-tab[2,2])}, 
  chi_p$c1, 
  chi_p$c2)

repeats<-c()
for(i in 1:length(chi_p$c1)){
  repeats[i]<-(chi_p$c1[i] %in% chi_p$c2[1:i]&chi_p$c2[i] %in% chi_p$c1[1:i])
}

2*sum(repeats)

chi_p<-chi_p[!repeats,]
