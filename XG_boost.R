library(readxl)
library(tidyverse)
library(xgboost)
library(caret)

dataset_xgb = as.matrix(as.data.frame(dataset_6))
View(dataset_xgb)
#train_dataset_xgb = as.matrix(as.data.frame(training_set))
#xgb_temp_train= training_set[,c(-11,-12)]
#xgb_temp_test= test_set[,c(-10,-11)]
X_train = xgb.DMatrix(as.matrix(train_set))
y_train = train_set$AGL_thermal_conductivity
X_test = xgb.DMatrix(as.matrix(test_set))
y_test = y_test


xgb_trcontrol = trainControl(
  method = "cv",
  number = 5,  
  allowParallel = TRUE,
  verboseIter = FALSE,
  returnData = FALSE
)

xgbGrid <- expand.grid(nrounds = c(100,200),  # this is n_estimators in the python code above
                       max_depth = c(10, 15, 20, 25),
                       colsample_bytree = seq(0.5, 0.9, length.out = 5),
                       ## The values below are default values in the sklearn-api. 
                       eta = 0.1,
                       gamma=0,
                       min_child_weight = 1,
                       subsample = 1)
#Training the XGBoost Model
set.seed(26)
xgb_model = train(X_train, y_train,  
                  trControl = xgb_trcontrol,
                  tuneGrid = xgbGrid,
                  method = "xgbTree")
#Viewing the Tuning parameters of the model
xgb_model$bestTune

#Model evaluation
predicted = predict(xgb_model, X_test)
residuals = y_test - predicted
postResample(predicted,y_test)

RMSE = sqrt(mean(residuals^2))
cat('The root mean square error of the test data is ', round(RMSE,3),'\n')

#Calculating R2 on Test set
y_test_mean = mean(y_test)
# Calculate total sum of squares
tss =  sum((y_test - y_test_mean)^2 )
# Calculate residual sum of squares
rss =  sum(residuals^2)
# Calculate R-squared
rsq  =  1 - (rss/tss)
cat('The R-square of the test data is ', round(rsq,3), '\n')
#The R2 turns out to be 0.85 which is lesser than that of GRNNs

#Plotting Results
options(repr.plot.width=8, repr.plot.height=4)
my_data = as.data.frame(cbind(predicted = predicted,
                              observed = y_test))
# Plot predictions vs test data
ggplot(my_data,aes(predicted, observed)) + geom_point(color = "darkred", alpha = 0.5) + 
  geom_smooth(method=lm)+ ggtitle('') + ggtitle("Extreme Gradient Boosting: Prediction vs Test Data") +
  xlab("Predicted Thermal Conductivity ") + ylab("Actual Thermal Conductivity") + 
  theme(plot.title = element_text(,size=16,hjust = 0.5),
        axis.text.y = element_text(size=12), axis.text.x = element_text(size=12,hjust=.5),
        axis.title.x = element_text(size=14), axis.title.y = element_text(size=14))


#Calculating Feature Importance
#xgb.importance(colnames(), model = xgb_model)

# Same thing with co-occurence computation this time
#xgb.importance(colnames(agaricus.train$data), model = bst, 
              # data = agaricus.train$data, label = agaricus.train$label)
