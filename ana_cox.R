# cox for ana

#multivariate
analyse_multivariate(data.frame(Survival.Months=basic$Survival.Months, 
                                Vital.Status=basic$Vital.Status, 
                                Age=basic$Age, 
                                #KPS=basic$KPS.factor, 
                                Chemo=trt$Initial.Chemo.w.radiation, 
                                Res=trt$Initial.Max.Resection.,
                                HTN=ci$HTN, 
                                HLD=ci$HLD, 
                                CAD=ci$CAD, 
                                Diabetes=ci$Diab, 
                                Stroke=ci$Stroke, 
                                Obesity=ci$Obesity, 
                                Afib=ci$Afib),
                     time_status=vars(Survival.Months, Vital.Status),
                     covariates = vars(Age, Chemo, Res,HTN),#lapply(all_pcomorbs, function(x){expr(!!x)}), # covariates expects a list
                     #covariate_name_dict = all_pcomorbs_n,
                     reference_level_dict = c()) %>%
  forest_plot(factor_labeller = c(Age="Age", KPS="KPS", Chemo="Chemo", Res="Res", HTN="HTN", HLD="HLD", CAD="CAD", Diabetes="Diabetes", Stroke="Stroke", Obesity="Obesity", Afib="Afib"),
              endpoint_labeller = c(Survival.Months="Survival Months"),
              orderer = ~order(abs(log(HR)), decreasing = T),
              labels_displayed = c("endpoint","factor", "n"),
              ggtheme = ggplot2::theme_bw(base_size = 10))



# multiple univariate next to each other
purrr::map(vars(Age, MGMT, IDH, KPS, BMI), function(by)
{
  analyse_multivariate(data.frame(Survival.Months=basic$Survival.Months, 
                                  Vital.Status=basic$Vital.Status, 
                                  Age=basic$Age, 
                                  MGMT=basic$MGMT.UME, 
                                  IDH=basic$IDH.MUT, 
                                  KPS=basic$KPS.factor, 
                                  BMI=basic$BMI.factor),
                       vars(Survival.Months, Vital.Status),
                       covariates = list(by), # covariates expects a list
                       covariate_name_dict = c(Age='Age', MGMT='MGMT', IDH='IDH', KPS='KPS', BMI='BMI'),
                       reference_level_dict = c('MGMT'=""))
}) %>%
  forest_plot(factor_labeller = c(Age='Age', MGMT='MGMT', IDH='IDH', KPS='KPS', BMI='BMI'),
              endpoint_labeller = c(Survival.Months="Survival Months"),
              orderer = ~order(abs(log(HR)), decreasing = T),
              labels_displayed = c("endpoint","factor", "n"),
              ggtheme = ggplot2::theme_bw(base_size = 10))
