#dataset_2=Label_encoded_data_AFLOW
attach(dataset_2)
train = sample(1:nrow(dataset_2),0.8*nrow(dataset_2))
test = -train
train_set = dataset_2[train,]
test_set = dataset_2[test,]
y_test = test_set$AGL_thermal_conductivity
test_set$AGL_thermal_conductivity = NULL
#attach(dataset_1)
linear.model1 = lm(AGL_thermal_conductivity~.,data = train_set)
summary(linear.model1)
model1.predictions = predict(linear.model1,test_set)
