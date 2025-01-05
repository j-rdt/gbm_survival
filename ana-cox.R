fastcox<-function(all_pcomorbs_vec){
  all_pcomorbs_n<-c(HTN="Hyptertension", 
                    HLD="Hyperlipidemia", 
                    obesity="Obesity", 
                    GERD="GERD", 
                    Diabetes="Diabetes", 
                    osteoarthritis="Osteoarthritis", 
                    Depression="Depression", 
                    CAD="CAD",
                    Hypothyroidism="Hypothyroidism", 
                    BPH="BPH", 
                    Afib="Afib", 
                    Asthma="Asthma", 
                    sleep.apnea="Sleep Apnea", 
                    Stroke="Stroke", 
                    tobacco.use.disorder="Tobacco Use Dis.",
                    COPD="COPD",
                    gout="Gout",
                    nephrolithiasis="Nephrolithiasis",
                    MI="MI",
                    CKD="CKD",
                    alcohol.use.disorder="Alcohol Use Dis.",
                    dysrhythmia="Dysrhythmia"
  )
  fac_ref<-c(HTN="NO", 
             HLD="NO", 
             obesity="NO", 
             GERD="NO", 
             Diabetes="NO", 
             osteoarthritis="NO", 
             Depression="NO", 
             CAD="NO",
             Hypothyroidism="NO", 
             BPH="NO", 
             Afib="NO", 
             Asthma="NO", 
             sleep.apnea="NO", 
             Stroke="NO", 
             tobacco.use.disorder="NO",
             COPD="NO",
             gout="NO",
             nephrolithiasis="NO",
             MI="NO",
             CKD="NO",
             alcohol.use.disorder="NO",
             dysrhythmia="NO"
  )
  purrr::map(sapply(all_pcomorbs_vec, function(x){expr(!!x)}), function(by)
  {
    analyse_multivariate(surv20,
                         vars(Survival.Months, Vital.Status),
                         covariates = list(by), # covariates expects a list
                         covariate_name_dict = all_pcomorbs_n,
                         reference_level_dict = fac_ref)
  }) %>%
    forest_plot(factor_labeller = all_pcomorbs_n,
                endpoint_labeller = c(Survival.Months="Survival Months"),
                orderer = ~order(abs(log(HR)), decreasing = T),
                labels_displayed = c("endpoint","factor", "n"),
                ggtheme = ggplot2::theme_bw(base_size = 10))
  
}
fastpaircox<-function(data, ...){
  
  covs<-c(...)
  #return(covs)
  #stop()
  flatcovs<-str_flatten(covs, collapse=" & ")
  #return(flatcovs)
  #stop()
  subs<-subset.make(data, subset.find(data, flatcovs))
  Condition<-flatcovs
  notNA<-apply(is.na(subs), 1, any)
  subs<-subs[!notNA,]
  data2<-unite(subs, Condition, sep=" ")
  subs<-cbind(data[!notNA,1:2],data2)
  counts<-table(data2$Condition)
  
  all_pcomorbs_n<-c(Condition="monkeys"
  )
  all_pcomorbs<-c(Condition="Condition"
  )
  fac_ref<-c(Condition=names(counts)[str_detect(names(counts), "\\-.*\\-")]
  )
  purrr::map(sapply(all_pcomorbs, function(x){expr(!!x)}), function(by)
  {
    analyse_multivariate(subs,
                         vars(Survival.Months, Vital.Status),
                         covariates = list(by), # covariates expects a list
                         covariate_name_dict = all_pcomorbs_n,
                         reference_level_dict = fac_ref)
  }) %>%
    forest_plot(factor_labeller = all_pcomorbs_n,
                endpoint_labeller = c(Survival.Months="OS"),
                label_headers = c(factor = "Subgroup"),
                orderer = ~order(abs(log(HR)), decreasing = T),
                labels_displayed = c("factor"),
                ggtheme = ggplot2::theme_bw(base_size = 10), 
                title=Condition
    )
  
}
#fastcox(c(obesity="obesity", Afib="Afib"))



tiff("oac.tiff", height=700, width=1750, units="px",res=300)
fastpaircox(big, "Obesity", "Afib")
dev.off()

tiff("htnhldc.tiff", height=700, width=1750, units="px",res=300)
fastpaircox(big, "HTN", "HLD")
dev.off()

tiff("oacadc.tiff", height=700, width=1750, units="px",res=300)
fastpaircox(big, "OA", "CAD")
dev.off()

tiff("htndiabc.tiff", height=700, width=1750, units="px",res=300)
fastpaircox(big, "HTN", "Diab")
dev.off()

tiff("hldcadc.tiff", height=700, width=1750, units="px",res=300)
fastpaircox(big, "HLD", "CAD")
dev.off()

tiff("oac.tiff", height=700, width=1750, units="px",res=300)
fastpaircox(big, "Obesity", "Afib")
dev.off()


