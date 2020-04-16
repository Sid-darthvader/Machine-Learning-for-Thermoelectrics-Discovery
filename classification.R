library(caret)
dataset_class_old = dataset_class
#cor(dataset_class_old$AGL_bulk_mod,dataset_class_old$energy_atom)
dataset_class$AGL_bulk_mod=NULL
X_scaled=dataset_class[,1:7]
y_scaled =dataset_class[,8]
#cor(X_scaled[,1],y_scaled)
## Scale data
max = apply(X_scaled, 2 , max)
min = apply(X_scaled, 2 , min)
scaled = as.data.frame(scale(X_scaled, center = min, scale = max - min))

index =createDataPartition(dataset_class$Thermal_Conductivity,p=0.80,list = FALSE)
y_train = dataset_class$Thermal_Conductivity[index]
y_test =dataset_class$Thermal_Conductivity[-index]

train_cset = dataset_class[index,]
test_cset = dataset_class[-index,]

#Only run if data is to be scaled
train_cset = scaled[index,]
test_cset = scaled[-index,]
###########################

y_train= as.factor(y_train)
#train_data_caret = cbind(train_cset,y_train)
train_data_caret = train_cset
# Run algorithms using 10-fold cross validation
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

attach(train_data_caret)
# a) linear algorithms
set.seed(8)
fit.lda <- train(Thermal_Conductivity~., data=train_data_caret, method="lda",
                 metric=metric, trControl=control)
print(fit.lda)
#58.5% Accuracy
# b) nonlinear algorithms
# Boosted Logit Model= 72% when niter=31
set.seed(7)
fit.cart <- train(Thermal_Conductivity~., data=train_data_caret, method="LogitBoost",
                  metric=metric, trControl=control)
# kNN = 65.61% Accuracy with k=5
set.seed(7)
fit.knn <- train(Thermal_Conductivity~., data=train_data_caret, method="knn", 
                 metric=metric, trControl=control)
print(fit.knn)
# c) advanced algorithms
# SVM  with RBF kernel= 62.83% Accuracy
# SVM with poly Kernel = 67.2%
set.seed(7)
fit.svm <- train(Thermal_Conductivity~., data=train_data_caret, method="svmRadial", metric=metric, trControl=control)
print(fit.svm)
# Random Forest = 72.76% Accuracy
set.seed(7)
fit.rf <- train(Thermal_Conductivity~., data=train_data_caret, method="rf", metric=metric, trControl=control)
print(fit.rf)

set.seed(7)
fit.nnet <- train(Thermal_Conductivity~., data=train_data_caret, method="nnet", metric=metric, trControl=control)
print(fit.nnet)

set.seed(7)
fit.nb <- train(Thermal_Conductivity~., data=train_data_caret, method="naive_bayes", metric=metric, trControl=control)
print(fit.nb)
#Naive Bayes when use Kernel is TRUE = 0.59
# XG_boost = 73.44% Accuracy,overall accuracy= 72.13% 
set.seed(8)
fit.xgbmodel <- train(Thermal_Conductivity~., data=train_data_caret, method="xgbTree", metric=metric, trControl=control)

print(fit.xgbmodel)
summary(fit.xgbmodel)
plot(fit.xgbmodel)
varImp(object = fit.xgbmodel)
plot(varImp(object=fit.xgbmodel),main="XGBoost - Variable Importance")
 results <- resamples(list(LDA=fit.lda, Logistic_reg=fit.cart, kNN=fit.knn, SVM_rbf_kernel=fit.svm, Random_forest=fit.rf, XGBoost=fit.xgbmodel,Neural_Network =fit.nnet,Naive_bayes = fit.nb))
summary(results)
dotplot(results)
# Random Forest = 72.76% Accuracy
set.seed(7)
#fit.rmodel <- train(y_train~., data=train_data_caret, method="rbf", metric=metric, trControl=control)
#print(fit.rmodel)
#summary(fit.rmodel)
validation = cbind(test_cset,y_test)
# estimate skill of LDA on the validation dataset
#Compute Confusion Matrix
xgb_classifier_pred <- predict(fit.xgbmodel, validation)
xgb_classifier_pred
validation$y_test
y_test_classifier_pred =y_te
predictions <- predict(fit.xgbmodel, validation)
confusionMatrix(predictions, validation$y_test)


library(caretEnsemble)
attach(train_data_caret)

control <- trainControl(method="repeatedcv", number=10, repeats=3, savePredictions=TRUE,classProbs = TRUE)
algorithmList <- c('xgbTree','rf','svmPoly','lda','nnet','knn','naive_bayes') #'glm', 'knn', 'svmRadial','nnet')
set.seed(8)
models <- caretList(Thermal_conductivity~., data=train_cset, trControl=control
                    , methodList=algorithmList)
results <- resamples(models)
summary(results)
dotplot(results)

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