# epistasis cox for ana

subset.make<-function(data, ...){
  out<-data[c(...)]
  out
}

subset.find<-function(data, stri){
  which(str_detect(colnames(data), str_replace(stri, " & ", "|")))
}

fastpaircox<-function(data, ...){
  
  covs<-c(...)
  flatcovs<-str_flatten(covs, collapse=" & ")
  subs<-subset.make(data, subset.find(data, flatcovs))
  assign(flatcovs, flatcovs)
  notNA<-apply(is.na(subs), 1, any)
  subs<-subs[!notNA,]
  data2<-unite(subs, get(flatcovs))
  subs<-cbind(data[!notNA,1:2],data2)
  
  all_pcomorbs_n<-c(name="group"
  )
  all_pcomorbs<-c(name="name"
  )
  fac_ref<-c(name="NO_NO"
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
                endpoint_labeller = c(Survival.Months="Survival Months"),
                orderer = ~order(abs(log(HR)), decreasing = T),
                labels_displayed = c("endpoint","factor"),
                ggtheme = ggplot2::theme_bw(base_size = 10), 
                title=name
    )
  
}