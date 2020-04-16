library(parallel)
library(doMC)
library(caretEnsemble)
#numCores <- detectCores()
#registerDoMC(cores = numCores)


# Example of Stacking algorithms
# create submodels
control <- trainControl(method="repeatedcv", number=10, repeats=3, savePredictions=TRUE, classProbs=TRUE)
algorithmList <- c('lda', 'rf','xgbTree','svmRadial','gaussprPoly','knn') #'glm', 'knn', 'svmRadial','nnet')
algorithmList <- c('nnet','xgbTree','rf','gaussprPoly','enet','lm')
set.seed(7)
models <- caretList(y_train~., data=train_data_caret, trControl=control
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
set.seed(7)
stack.glm <- caretStack(models, method="glm")
print(stack.glm)

stack.rf <- caretStack(models, method="rf")
print(stack.rf)
