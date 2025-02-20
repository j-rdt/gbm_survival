# long plot, redone to reflect Justin's changes in categories

library(ggplot2)
library(viridis)
library(hrbrthemes)

Group<-c(rep("Metabolic", 4), 
         rep("Neurologic / Psychiatric", 8), 
         rep("Cardiovascular", 7), 
         rep("Respiratory", 4), 
         rep("GI / GU", 6), 
         rep("Endocrine", 3), 
         rep("Other", 6)
)

Condition<-c("Type 2 Diabetes", 
             "Hypertension", 
             "Hyperlipidemia", 
             "Obesity", 
             "Stroke", 
             "Seizure", 
             "Depression", 
             "Degenerative/Inflammatory",
             "Substance Use Disorders",
             "Other psychiatric", 
             "Other neuro",
             "Headache/Migraine",
             "Congestive Heart Failure", 
             "Atrial Fibrillation", 
             "Coronary Artery Disease", 
             "Anemia", 
             "DVT/PE", 
             "Myocardial Infarction",
             "Other cardiovascular", 
             "COPD", 
             "Asthma", 
             "Sleep Apnea",
             "Other respiratory",
             "GI Inflamatory Disorders", 
             "GERD", 
             "Other GI",
             "Kidney Disease",
             "Other GU",
             "Other renal",
             "Hypothyroidism", 
             "Hyperthyroidism", 
             "Other endocrine",
             "Gout", 
             "Osteoarthritis", 
             "Additional Malignancies",
             "Other Musculoskeletal", 
             "Infectious Disease", 
             "Autoimmune")

Condition<-factor(Condition, levels = Condition)

Number<-c(sum(has(ci$Diab)), #"Type 2 Diabetes", 
             sum(has(ci$HTN)),#"Hypertension", 
             sum(has(ci$HLD)), #"Hyperlipidemia", 
             sum(has(ci$Obesity), na.rm=TRUE),#"Obesity", 
             sum(has(ci$Stroke)),#"Stroke", 
             sum(has(ci$Epi)|has(ci$Conv)),#"Seizure", 
             sum(has(ci$Dep)),#"Depression", 
             sum(has(ci$ALS)|has(ci$MS)|has(ci$Dementia)|has(ci$`Parkinson's`)),#"Neurodegenerative/ \n Neuroinflammation",
             sum(has(ci$AUD)|has(ci$cocaine.abuse)|has(ci$Opiod.Abuse)|has(ci$tobacco.use.disorder)),#"Substance Use Disorders"
             sum(has(ci$PTSD)|has(ci$Anxiety)|has(ci$Bipolar)|has(ci$Schizophrenia)),#"Other psychiatric", 
             sum(has(ci$Intellectual.Disability)|has(ci$Polyneuropathy)|has(ci$TBI)|
                   has(ci$`bell's`)|has(ci$Cerebral.Aneurysm)|has(ci$Cerebral.Palsy)|
                   has(ci$Encephalopathy)|has(ci$Epidural.Hemorrhage)|has(ci$Hydrocephalus)|
                   has(ci$Meningitis)|has(ci$Narcolepsy)|has(ci$Neuropathy)|has(ci$Subarachnoid.Hemorrhage)|
                   has(ci$Subdural.Hemorrhage)|has(ci$Neurofibromatosis.type.1)),#"Other neuro"
             sum(has(ci$`Headache/Migraine`)),#"Headache/Migraine",
             sum(has(ci$CHF)),#"Congestive Heart Failure", 
             sum(has(ci$Afib)),#"Atrial Fibrillation", 
             sum(has(ci$CAD)),#"Coronary Artery Disease", 
             sum(has(ci$Anemia)),#"Anemia", 
             sum(has(ci$DVT)|has(ci$PE)),#"DVT / PE", 
             sum(has(ci$MI)),#"Myocardial Infarction",
             sum(has(ci$AV.Block)|has(ci$Cardiomegaly)|has(ci$valve.disorder)|has(ci$Valve)|
                   has(ci$LBBB)|has(ci$PVD)|has(ci$PulmHD)|has(ci$Sinus.tach)|has(ci$Thrombocytopenia)|
                   has(ci$AortAn)|has(ci$AortSten)|has(ci$Cardiac.Arrest)|has(ci$CMP)|has(ci$Dysrhythmia)|
                   has(ci$Endocarditis)|has(ci$heart.malposition)|has(ci$heart.neoplasm.benign)|
                   has(ci$Hypercoagulable)|has(ci$Hypotension)|has(ci$Murmur)|has(ci$Pericarditis)|
                   has(ci$Portal.Vein.Thrombosis)),#"Other cardiovascular", 
             sum(has(ci$COPD)),#"COPD", 
             sum(has(ci$Asthma)),#"Asthma", 
             sum(has(ci$Apnea)),#"Sleep Apnea",
             sum(has(ci$asbestosis)|has(ci$pulm.fail)|has(ci$pulmonary.nodule)|
                   has(ci$pulm..fibrosis)),#"Other respiratory"
             sum(has(ci$Divert)|has(ci$GI.Hem)|has(ci$IBS)|has(ci$celiac.disease)|
                   has(ci$`chron's`)|has(ci$Gastric.Ulcer)|has(ci$Gastritis)|
                   has(ci$infl.colon.polyps)|has(ci$Peptic.Ulcer)|has(ci$ulcer..colitis)|
                   has(ci$Esophagitis)|has(ci$`hemorrhoids/rectal.bleed`)|has(ci$rectal.prolapse)),#"GI Inflamatory Disorders", 
             sum(has(ci$GERD)),#"GERD", 
             sum(has(ci$hep.B)|has(ci$hepatitis.C)|has(ci$Liver.Disease)|
                   has(ci$Pancreatitis)|has(ci$Splenomegaly)),#"Other GI",
             sum(has(ci$CKD)|has(ci$Kidney.Fail)),#"Kidney Disease",
             sum(has(ci$BPH)|has(ci$UTI)|has(ci$Bladder)|has(ci$inguinal.hernia)|
                   has(ci$Nephrolithiasis)|has(ci$ovarian.cysts)|has(ci$urinary.retention)),#"Other GU",
             sum(has(ci$Kidney.Cyst)|has(ci$glomerulonephritis)),#"Other renal",
             sum(has(ci$Hypothyroid)),#"Hypothyroidism", 
             sum(has(ci$Thyrotoxosis)),#"Hyperthyroidism", #previously I used hyperparathyroidism here because I did not know what thyrotoxicosis was
             sum(has(ci$Hypoglyc)|has(ci$SIADH)|has(ci$T1DM)|has(ci$Acromegaly)|
                   has(ci$Hyperparathyroidism)|has(ci$Hypogonadism)|has(ci$Hypoparathyroidism)|
                   has(ci$Pituitary.Disorder)),#"Other endocrine",
             sum(has(ci$Gout)),#"Gout", 
             sum(has(ci$OA)),#"Osteoarthritis", 
             sum(has(maligs$Additional.Malignancy)),#"Additional Malignancies",
             sum(has(ci$Fib)|has(ci$Rosacea)|has(ci$Sciatica)|has(ci$cervicalgia)|
                   has(ci$disc.disorder)|has(ci$`kyphosis/scoliosis`)|has(ci$`mylagia/myositis/myopathy`)|
                   has(ci$Spinal.Stenosis)),#"Musculoskeletal", 
             sum(has(ci$babeosis)|has(ci$Immunosupression)|has(ci$lyme.disease)|has(ci$Osteomyelitis)|
                   has(ci$Pneumonia)|has(ci$Sepsis)),#"Infectious Disease", 
             sum(has(ci$lupus)|has(ci$T1DM)|has(ci$Psoriasis)|has(ci$Psoriatic.arthritis.mutilans)|
                   has(ci$rheumatic.heart.disease)|has(ci$rheumatic.polymylagia)|has(ci$RA)|
                   has(ci$`chron's`)|has(ci$celiac.disease)|has(ci$ulcer..colitis)|has(ci$MS)) #"Autoimmune"
             )

colors<-log(Number)







data <- data.frame(Group,Condition,Number)
data$Group<-factor(data$Group, levels=unique(data$Group))



maxheight=1400
length=2500
#dim=dev.size()
#ratio=dim[1]/dim[2]

setwd("../final.figures")
tiff("2a-long-one.tiff", height=maxheight, width = length, units="px", res=300)
# Grouped
ggplot(data, aes(group=Group, y=Number, x=Condition, fill=colors)) + 
  geom_bar(position="dodge", stat="identity", width = 0.9) +
  facet_grid( ~Group, scales='free', space='free') +
  theme(
    axis.text.x = element_text(
      angle = 65,
      hjust = 1,
      size = 12
    ),
    axis.title.x=element_blank(),
  ) + scale_fill_gradient(high = "red", low="black", guide = NULL) +
  geom_text(aes(label=Number), size= 3, position=position_dodge(width=0.9), vjust=-0.25) +
  ylab("Number of Patients")
dev.off()

setwd("../gbm_survival")
