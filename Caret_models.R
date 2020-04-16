library(caret)
library(xgboost)
#dataset_6 = read.csv("AFLOW_6.csv")
#data = dataset_6
data= Aflow_with_grun
dataset_full = Aflow_with_grun
dataset_full$Bravias_lattice=NULL
#data$X=NULL
cor_matrix_low_k = cor(data)
write.csv(cor_matrix_low_k,"correlations_low_k_reg.csv")
data = dataset_low_kl
dataset_full$AGL_bulk_mod=NULL
dataset_full$AGL_Gruneisen_parameter=NULL
dataset_full$Debye_temp=NULL
dataset_full=as.data.frame(dataset_full)
index_full =createDataPartition(dataset_full$AGL_thermal_conductivity,p=0.80,list = FALSE)
y_train_full = dataset_full$AGL_thermal_conductivity[index_full]
y_test_full =dataset_full$AGL_thermal_conductivity[-index_full]
train_cset_full = dataset_full[index_full,]
test_cset_full = dataset_full[-index_full,1:7]

#write.csv(dataset_low_kl,"dataset_low_kl.csv")
data= data.frame(data)
X_scaled=data[,1:10]
y_scaled =data[,11]
## Scale data
max = apply(X_scaled, 2 , max)
min = apply(X_scaled, 2 , min)
scaled = as.data.frame(scale(X_scaled, center = min, scale = max - min))



#train = scaled[index ]

#trainNN = as.data.frame(cbind(trainNN,y_train))
#testNN = scaled[-index ,]
set.seed(8)
index =createDataPartition(data$AGL_thermal_conductivity,p=0.80,list = FALSE)
y_train = data$AGL_thermal_conductivity[index]
y_test =data$AGL_thermal_conductivity[-index]

train_cset = data[index,]
test_cset = data[-index,1:7]
#test_cset_sim =data[-index,1:11]
#model_predicted_thermal_cond = cubist_pred
#data_sim=cbind(test_cset_sim,model_predicted_thermal_cond)
#write.csv(data_sim,"Dataset_for_simulations.csv")
#Run if data has been scaled...
train_cset = scaled[index,]
test_cset = scaled[-index,]
#caret_reg_train = cbind(train_cset,y_train)
#Feature selection using rfe in caret
control <- rfeControl(functions = rfFuncs,
                      method = "repeatedcv",
                      repeats = 3,
                      verbose = FALSE)
#outcomeName<- 'AGL_Thermal_Conductivity'
#predictors<-names(train_cset)[!names(train_cset) %in% outcomeName]
Thermal_Conductivity_Pred_Profile <- rfe(train_cset[,1:10], y_train,
                         rfeControl = control)

Thermal_Conductivity_Pred_Profile
#The top 5 variables (out of 8):
#energy_atom, AGL_bulk_mod,electronic_energy_band_gap
#, point_group, ratio_oxygen_by_transition_metal_atom
fitControl <- trainControl(method = "repeatedcv",number = 5,
  repeats = 5)
modelLookup(model='rf')
grid_cb <- expand.grid(committees = c(1, 10, 50, 100),
               neighbors = c(0, 1, 5, 9)) 
grid <- expand.grid(n.trees=c(10,20,50,100,500,1000),shrinkage=c(0.01,0.05,0.1,0.5),n.minobsinnode = c(3,5,10),interaction.depth=c(1,5,10))

# training the model
model_cubist<-train(train_cset[,1:10],y_train,method='cubist',trControl=fitControl,tuneGrid = grid_cb)
summary(model_cubist)
print(model_cubist)
plot(model_cubist)
# summarizing the model
print(model_cubist)


####Model_Building####
#Plotting the Tuning Paramters for each of the above models
model_gbm<-train(train_cset[,1:10],y_train,method='gbm')

model_rf1<-train(train_cset[,1:7],y_train,method='rf',trControl=fitControl)

model_rf2<-train(train_cset_full[,1:7],y_train_full,method='rf',trControl=fitControl)

plot(model_rf)
print(model_rf)
model_nnet<-train(train_cset[,1:10],y_train,method='nnet')
model_knn<-train(train_cset[,1:10],y_train,method='knn')
model_lasso <-train(train_cset[,1:10],y_train,method='glmnet')
model_linear <-train(train_cset[,1:10],y_train,method='lm')
summary(model_linear)
#model_blasso <-train(train_cset[,1:10],y_train,method='blassoAveraged')
model_xgb <-train(train_cset[,1:10],y_train,method='xgbTree')
varImp(object = model_xgb)
varImp(model_gprl)
plot(varImp(object=fit.xgbmodel),main="XGBoost - Variable Importance")
#model_elastic <-train(train_cset[,1:9],y_train,method='cubist')
model_gprl<-train(train_cset[,1:10],y_train,method='gaussprPoly')
#cubist gives 44% accuracy
#model_6<-train(train_cset[,1:8],y_train,method='mlpKerasDropout')
#Bayesian Neural Network 9% accuracy
results <- resamples(list(GBM= model_gbm,Random_forests=model_rf,
                          Deep_Neural_Network=model_nnet, 
                          kNN = model_knn,
                          Lasso=model_lasso, 
                          XGBoost=model_xgb,
                          Linear =model_linear,
                          GPR_poly_kernel = model_gprl))
summary(results)
dotplot(results)
#Plotting xgboost
xgb.pl

model_rf1_pred = predict(model_rf1,test_cset)
postResample(pred= model_rf1_pred,obs = y_test)

model_rf2_pred = predict(model_rf2,test_cset_full)
postResample(pred= model_rf2_pred,obs = y_test_full)


#model_glm<-train(train_cset[,1:8],train_cset[,9],method='glm')

#model_glm<-train(train_cset[,1:8],train_cset[,9],method='glm')

#model_glm<-train(train_cset[,1:8],train_cset[,9],method='glm')

#model_glm<-train(train_cset[,1:8],train_cset[,9],method='glm')


plot(model_gbm)
plot(model_rf)
plot(model_nnet)
plot(model_rf)
plot(model_brnn)

##Plotting Variable Importance
varImp(object = model_glm)
varImp(object = model_nnet)
#varImp(object = model_rf)
#varImp(object = model_rf)
plot(varImp(object=model_glm),main="GLM - Variable Importance")

#Computing Model Accuracies
cubist_pred = predict(model_cubist,test_cset[,1:10])
postResample(pred= cubist_pred,obs = y_test)
y_test
cubist_pred
#Computing Model Accuracies
lasso_pred = predict(model_lasso,test_cset[,1:10])
postResample(pred= lasso_pred,obs = y_test)

#Computing Model Accuracies
gpr_pred = predict(model_gprl,test_cset[,1:10])
postResample(pred= gpr_pred,obs = y_test)

#Computing Model Accuracies
lm_pred = predict(model_linear,test_cset[,1:10])
postResample(pred= lm_pred,obs = y_test)

#Computing Model Accuracies
knn_pred = predict(model_knn,test_cset[,1:10])
postResample(pred= knn_pred,obs = y_test)

#Computing Model Accuracies
xgb_pred = predict(model_xgb,test_cset[,1:10])
postResample(pred= xgb_pred,obs = y_test)

#GBM gives 48% accuracy
gbm_pred = predict(model_gbm,test_cset[,1:10])
postResample(pred= gbm_pred,obs = y_test)

#NNet gives  accuracy
nnet_pred = predict(model_nnet,test_cset[,1:10])
postResample(pred= nnet_pred,obs = y_test)

#Random Forests give 55% accuracy
rf_pred = predict(model_rf,test_cset[,1:8])
postResample(pred= rf_pred,obs = y_test)

result_comparison_df = cbind(y_test,rf_pred)
result_comparison_df = as.data.frame(result_comparison_df)
result_comparison_df
result_comparison_df_low_Kl = result_comparison_df[result_comparison_df$y_test<5,]
result_comparison_df_low_Kl
#26 out of 60 observations have low Kl and MAE of 1.5
# 34 out of 60 have Kl>5 and in predicting that we have MAE of 2.7
#nrow(result_comparison_df_low_Kl)
mae_low_Kl = mean(abs(result_comparison_df_low_Kl$y_test-
                        result_comparison_df_low_Kl$rf_pred))
mae_low_Kl
mae = mean(abs(y_test-lm.predictions))
mae

#Bayesian Neural Network 9% accuracy
#random_glm_pred = predict(model_random_glm,test_cset[,1:8])
#postResample(pred= random_glm_pred,obs = test_cset[,9])

#Bayesian Neural Network 9% accuracy
#brnn_pred = predict(model_brnn,test_cset[,1:8])
#postResample(pred= brnn_pred,obs = test_cset[,9])

#Bayesian Neural Network 9% accuracy
#brnn_pred = predict(model_brnn,test_cset[,1:8])
#postResample(pred= brnn_pred,obs = test_cset[,9])

library(caretEnsemble)
attach(caret_reg_train)
attach(train_cset)
control <- trainControl(method="repeatedcv", number=10, repeats=3, savePredictions=TRUE)
algorithmList <- c('cubist','rf','gbm','xgbTree','gaussprPoly','knn','lm') #'glm', 'knn', 'svmRadial','nnet')
set.seed(8)
models <- caretList(AGL_thermal_conductivity~., data=train_cset, trControl=control
                    , methodList=algorithmList)
results <- resamples(models)
summary(results)
dotplot(results)
par(mfrow=c(1,2))
plot_data_test_rf1 = cbind(test_cset,y_test)
plot_data_test_rf2 = cbind(test_cset_full,y_test_full)

require(gridExtra)
#plot1 <- qplot(1)
#plot2 <- qplot(1)
grid.arrange(plot1, plot2, ncol=2)

# Add the regression line
plot1<-ggplot(plot_data_test_rf2, aes(x=model_rf2_pred, y=y_test_full)) + 
  geom_point()+theme(text = element_text(size=15))+
  geom_smooth(method=lm) +xlab("Predicted Thermal Conductivity") +
  ylab("Original Thermal Conductivity") +
  ggtitle("Predictions from Random Forest Model on complete dataset")

plot2 <-ggplot(plot_data_test_rf1, aes(x=model_rf1_pred, y=y_test)) + 
  geom_point()+theme(text = element_text(size=15))+
  geom_smooth(method=lm) +xlab("Predicted Thermal Conductivity") +
  ylab("Original Thermal Conductivity") +
  ggtitle("Predictions from Random Forest Model for Low Thermal Conductivity compounds.")
#par(mfrow=c(1,1))


# correlation between results
modelCor(results)
splom(results)

##
set.seed(7)
stack.glm <- caretStack(models, method="glm")
print(stack.glm)

# stack using random forest
set.seed(7)
stack.glm <- caretStack(models, method="rf")
print(stack.glm)
