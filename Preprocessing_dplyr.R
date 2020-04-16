#install.packages("tidyverse")
library(dplyr)
dataset_6 = read.csv("AFLOW_with_grun.csv")
dataset_6$ï..compound=NULL
dataset_6$Bravias_lattice=NULL
cor(dataset_6[,1],dataset_6[,11])
cor_matrix = cor(dataset_6)
cor_imp_features = c("energy_atom","ratio_atoms_cell_by_cell_vol"
                     ,"ratio_oxygen_by_transition_metal_atom","electronic_energy_band_gap"
                     ,"mass_density")
cor_imp_matrix = cor_matrix[cor_imp_features,cor_imp_features]

# Melt the correlation matrix
library(reshape2)
get_upper_tri <- function(cor_imp_matrix){
  cor_imp_matrix[lower.tri(cor_imp_matrix)]<- NA
  return(cor_imp_matrix)
}

upper_tri <- get_upper_tri(cor_imp_matrix)
upper_tri
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Heatmap
library(ggplot2)
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()





library(corrplot)
plot.new();
dev.off()
corrplot(cor_imp_matrix, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

dataset_sorted = dataset_6 %>%
  arrange(dataset_6$AGL_thermal_conductivity)
#Run for Low kl dataset
dataset_low_kl= dataset_sorted[1:138,]
dataset_sorted=dataset_sorted[1:314,]

dataset_sorted$Thermal_Conductivity= ifelse(dataset_sorted$AGL_thermal_conductivity<5,"Low","Medium")
#dataset_sorted$...11=NULL
dataset_sorted[241:314,12]="High"
attach(dataset_sorted)
names(dataset_sorted)
keeps = c("mass_density","ratio_oxygen_by_transition_metal_atom","ratio_atoms_cell_by_cell_vol","electronic_energy_band_gap"
          ,"energy_atom","point_group","c.a_ratio","AGL_bulk_mod","Thermal_Conductivity")
dataset_class = dataset_sorted[,keeps]
attach(dataset_class)
write.csv(dataset_class,"AFLOW_Classification.csv")
#sapply(dataset_class, class)


# summarize the class distribution
percentage <- prop.table(table(dataset_class$Thermal_Conductivity)) * 100
cbind(freq=table(dataset_class$Thermal_Conductivity), percentage=percentage)


