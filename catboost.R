#install.packages('devtools')
#devtools::install_github('catboost/catboost', subdir = 'catboost/R-package')
library(catboost)

#countries = c('RUS','USA','SUI')
#years = c(1900,1896,1896)
#phone_codes = c(7,1,41)
#domains = c('ru','us','ch')
#dataset_2=dataset_2[,2:10]
#dataset_cb = dataset_2[,2:10]

#label_values_1 = c(4,6,8,12,16,23,24,48)
#label_values_2 = c(12,14,15,33,36,38,45,55,57,58,59,60,62,63,65,84,
#                   88,99,109,119,121,123,127,129,136,137,139,140,
#                   141,148,152,162,164,182,186,191,193,194,198,205,
#                   216,218,221,224,225,227)
#label_values_mat = [label_values_2,label_values_2]
#label_values_mat = data.matrix(cbind(label_values_1,label_values_2))
params <- list(iterations=2000,
               learning_rate=0.01,
               depth=10,
               loss_function='RMSE',
               eval_metric='RMSE',
               random_seed = 55,
               od_type='Iter',
               metric_period = 50,
               od_wait=20,
               use_best_model=TRUE)
train_pool <- catboost.load_pool(data = train_cset[,1:8], label = train_cset[,9])
test_pool <- catboost.load_pool(data = test_cset[,1:8], label = test_cset[,9])
model <- catboost.train(learn_pool = train_pool,params = params)
#predict
y_pred=catboost.predict(model,test_pool)
#pool = catboost.load_pool(dataset)
input_pool =catboost.load_pool(train_cset[,2:8],train_cset[,9])
postResample(y_pred,test_cset[,9])
feature_imp_catboost = catboost.get_feature_importance(model,type = "LossFunctionChange",pool = input_pool)
##
plot(feature_imp_catboost)


#model <- catboost.train(pool, params = fit_params)
object_importance <- catboost.get_object_importance(model,
                                                    input_pool,
                                                    train_pool)
