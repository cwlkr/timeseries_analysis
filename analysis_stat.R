


features = fread("extracted_features.csv")

ggplot(features, aes(y = baseline.before, x = Stimulation_treatment)) + 
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
