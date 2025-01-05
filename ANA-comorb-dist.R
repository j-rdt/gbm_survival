library(ggplot2)
library(viridis)
library(hrbrthemes)

Group<-c(rep("Metabolic", 4), 
          rep("Neurologic", 4), 
          rep("Cardiovascular", 6), 
          rep("Respiratory", 3), 
          rep("GI & Renal", 3), 
          rep("Endocrine", 2), 
          rep("Musculo-\nskeletal", 2)
          )

Condition<-c("Type 2 Diabetes", "Hypertension", "Hyperlipidemia", "Obessity", 
              "Stroke", "Seizure", "Depression", "Headache/Migraine",
              "Congestive Heart Failure", "Atrial Fibrillation", "Coronary Artery Disease", "Anemia", "Deep Vein Thrombosis", "Myocardial Infarction", 
              "COPD", "Asthma", "Sleep Apnea", 
              "GI Inflamatory Disorders", "GERD", "Kidney Disease", 
              "Hypothyroidism", "Hyperthyroidism", 
              "Gout", "Osteoarthritis")

Number<-c(142, 501, 395, 257,
          44,20,110, 22,
          19,57,95,22,20,33,
          38,54,53,
          53,190, 39,
          81,18,
          36,114)

fills<-c(1, 2, 3, 4,
          1,2, 3,4,
          1,2,3,4,5,6
          1,2,3
          1, 2,3,
          1,2
          1,2)

data <- data.frame(Group,Condition,Number)
data$Group<-factor(data$Group, levels=unique(data$Group))

# Grouped
ggplot(data, aes(group=Group, y=Number, x=Condition, fill=Number)) + 
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
  
maxheight=1400
dim=dev.size()
ratio=dim[1]/dim[2]

tiff("ana-comorb-n.tiff", height=maxheight, width = maxheight*ratio, units="px", res=300)
# insert ggplot code
dev.off()
