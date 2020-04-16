library(caret)
data= Aflow_with_grun
data$Bravias_lattice=NULL
data= data.frame(data)
X_scaled=data[,1:10]
y_scaled =data[,11]


## Scale data
max = apply(X_scaled, 2 , max)
min = apply(X_scaled, 2 , min)
scaled = as.data.frame(scale(X_scaled, center = min, scale = max - min))
set.seed(8)
index =createDataPartition(data$AGL_thermal_conductivity,p=0.80,list = FALSE)
y_train = data$AGL_thermal_conductivity[index]
y_test =data$AGL_thermal_conductivity[-index]

train_cset = data[index,]
#train_cset
test_cset = data[-index,1:10]
#Linear Model on Unscaled Data
colnames(train_cset)
attach(train_cset)
train_set_h2o = cbind(train_cset,y_train)

linear.model1 = lm(train_cset$AGL_thermal_conductivity~.,train_cset)
summary(linear.model1)
lm.predictions = predict(linear.model1,test_cset)
y_test
lm.predictions


train_cset$AGL_bulk_mod =NULL
train_cset$AGL_Gruneisen_parameter=NULL
test_cset$AGL_bulk_mod =NULL
test_cset$AGL_Gruneisen_parameter=NULL
#   RMSE    Rsquared    MAE 
#2.4933404 0.8492615 1.9017892 
model_rf<-train(train_cset[,1:8],y_train,method='rf')
model_blasso <-train(train_cset[,1:10],y_train,method='blassoAveraged')
model_cubist <-train(train_cset[,1:10],y_train,method='cubist')
model_xgb<-train(train_cset[,1:8],y_train,method='xgbTree')
model_svr_poly<-train(train_cset[,1:10],y_train,method='svmPoly')
model_knn<-train(train_cset[,1:10],y_train,method='knn')
model_nnet<-train(train_cset[,1:10],y_train,method='nnet')
model_glm <- train(train_cset[,1:10],y_train,method='glm')
model_rfe <- train(train_cset[,1:10],y_train,method='leapBackward')
#R2 for rf = 83 and mae =2 when Bulk Mod is removed
#R2 for rf is 80 and mae = 1.82 when Bulk mod is removed
# RMSE  Rsquared       MAE (model=Rf When bulk+grun is rem)
#2.3898257 0.8489354 1.7809162 
plot(varImp(model_xgb))
lm.predictions = predict(model_xgb,test_cset)
postResample(lm.predictions,y_test)
result_comparison_df = cbind(y_test,lm.predictions)
result_comparison_df = as.data.frame(result_comparison_df)
result_comparison_df
result_comparison_df_low_Kl = result_comparison_df[result_comparison_df$y_test<5,]
result_comparison_df_low_Kl
#26 out of 60 observations have low Kl and MAE of 1.5
# 34 out of 60 have Kl>5 and in predicting that we have MAE of 2.7
#nrow(result_comparison_df_low_Kl)
mae_low_Kl = mean(abs(result_comparison_df_low_Kl$y_test-
                        result_comparison_df_low_Kl$lm.predictions))
mae_low_Kl
mae = mean(abs(y_test-lm.predictions))
mae
plot(model_rf)
plot(model_xgb)

attach(train_cset)

#Stacking models
library(caretEnsemble)
control <- trainControl(method="repeatedcv", number=10, repeats=3, savePredictions=TRUE)
#algorithmList <- c('lda', 'rf','xgbTree','svmRadial','gaussprPoly','knn') #'glm', 'knn', 'svmRadial','nnet')
algorithmList <- c('nnet','xgbTree','rf','gaussprPoly','enet','lm')
set.seed(8)
models <- caretList(AGL_thermal_conductivity~., data=train_cset, trControl=control
                    , methodList=algorithmList)
results <- resamples(models)
print(models)
summary(models)
summary(results)
dotplot(results)

# correlation between results
modelCor(results)
splom(results)


# stack using random forest
set.seed(8)
stack.glm <- caretStack(models, method="glm")
print(stack.glm)

stack.rf <- caretStack(models, method="rf")
print(stack.rf)
plot(stack.rf)





train_cset = scaled[index,]
test_cset = scaled[-index,]