dataset_6 = read.csv("AFLOW_6.csv")
data = dataset_6
data$Bravias_lattice=NULL
data$X=NULL
data= data.frame(data)
X_scaled=data[,1:8]
y_scaled =data[,9]
max = apply(X_scaled, 2 , max)
min = apply(X_scaled, 2 , min)
scaled = as.data.frame(scale(X_scaled, center = min, scale = max - min))

dataset_auto=cbind(scaled,y_scaled)

library(h2o)
h2o.init(max_mem_size = "5g")

features <- as.h2o(scaled)

# Train an autoencoder
ae1 <- h2o.deeplearning(
  x = seq_along(features),
  training_frame = features,
  autoencoder = TRUE,
  hidden = 5,
  activation = 'Tanh',
  sparse = TRUE
)

# Extract the deep features
ae1_codings <- h2o.deepfeatures(ae1, features, layer = 1)
View(ae1_codings)
dataset_autoencoded = cbind(as.data.frame(ae1_codings),y_scaled)
#write.csv(dataset_autoencoded,"dataset_autoencoded2.csv")

dataset_autoencoded[dataset_autoencoded$y_scaled==215.7260000,]= NaN
dataset_autoencoded= na.omit(dataset_autoencoded)
write.csv(dataset_autoencoded,"dataset_autoencoded5.csv")


########## Building Models  ##########
library(caret)
index =createDataPartition(dataset_autoencoded$y_scaled,p=0.80,list = FALSE)
y_train = dataset_autoencoded[index]
y_test =dataset_autoencoded$y_scaled[-index]


train_cset = dataset_autoencoded[index,]
test_cset = dataset_autoencoded[-index,]
y_train =train_cset[,6]
y_test =test_cset[,6]
#caret_reg_train_autoencod = cbind(train_cset,y_train)
#Feature selection using rfe in caret
control <- rfeControl(functions = rfFuncs,
                      method = "repeatedcv",
                      repeats = 3,
                      verbose = FALSE)
#outcomeName<- ''
#predictors<-names(train_cset)[!names(train_cset) %in% outcomeName]
Thermal_Conductivity_Pred_Profile <- rfe(train_cset[,1:2], y_train,
                                         rfeControl = control)

Thermal_Conductivity_Pred_Profile
#The top 5 variables (out of 8):
#energy_atom, AGL_bulk_mod,electronic_energy_band_gap
#, point_group, ratio_oxygen_by_transition_metal_atom
fitControl <- trainControl(method = "repeatedcv",number = 5,
                           repeats = 5)
modelLookup(model='cubist')
#grid <- expand.grid(committees = c(1, 10, 50, 100),
 #                   neighbors = c(0, 1, 5, 9)) 
grid <- expand.grid(n.trees=c(10,20,50,100,500,1000),shrinkage=c(0.01,0.05,0.1,0.5),n.minobsinnode = c(3,5,10),interaction.depth=c(1,5,10))

# training the model
model_cubist<-train(train_cset,y_train,method='cubist',trControl=fitControl,tuneGrid=grid)

# summarizing the model
print(model_gbm)

lm.model = lm(y_scaled~.,dataset_autoencoded)
summary(lm.model)
####Model_Building####
#Plotting the Tuning Paramters for each of the above models
model_gbm<-train(train_cset[,1:5],y_train,method='gbm')
model_rf<-train(train_cset[,1:5],y_train,method='rf')
model_nnet<-train(train_cset[,1:5],y_train,method='nnet')
model_glm<-train(train_cset[,1:5],y_train,method='glm')

model_xgb<-train(train_cset[,1:5],y_train,method='xgbTree')
#cubist gives 44% accuracy
#model_6<-train(train_cset[,1:8],y_train,method='mlpKerasDropout')
#Bayesian Neural Network 9% accuracy
model_6_pred = predict(model_xgb,test_cset[,1:5])
postResample(pred= model_6_pred,obs = y_test)
