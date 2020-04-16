dataset = Aflow_4
dataset = na.omit(dataset)
names(dataset)
keeps = c("mass_density","ratio_oxygen_by_transition_metal_atom","Bravias_lattice","ratio_atoms_cell_by_cell_vol","electronic_energy_band_gap"
          ,"energy_atom","point_group","c/a_ratio","AGL_bulk_mod","AGL_thermal_conductivity")
dataset_6 = dataset[,keeps]
dataset_6$Bravias_lattice = as.factor(dataset_6$Bravias_lattice)
write.csv(dataset_6,"AFLOW_6.csv")
library(caret)
dmy <- dummyVars(" ~ .", data = dataset_6, fullRank = T)
combi_encoded <- data.frame(predict(dmy, newdata = dataset_6))
write.csv(combi_encoded,"One_Hot_Encoded_AFlow4.csv")
dataset_5 = combi_encoded
#write.csv(dataset_1,"AFLOW_3.csv")

#dataset_1$space_group= as.factor(dataset_1$space_group)
#dataset_1$point_group= as.factor(dataset_1$point_group)
train = sample(1:nrow(dataset_6),0.8*nrow(dataset_6))
test = -train
train_set = dataset_6[train,]
test_set = dataset_6[test,]
y_test = test_set$AGL_thermal_conductivity
test_set$AGL_thermal_conductivity = NULL
attach(dataset_6)
linear.model1 = lm(AGL_thermal_conductivity~.,data = train_set)
summary(linear.model1)
model1.predictions = predict(linear.model1,test_set)
postResample(model1.predictions,y_test)
#Multiple Linear Model has an accuracy of 20.3%


#Best Subset selection
library(leaps)
regfit.full = regsubsets(dataset_6$AGL_thermal_conductivity~.,method = "forward", data = dataset_6,nvmax = 5)
summary(regfit.full)
names(regfit.full)
regfit.full$r