library(glmnet)
# Loading the data
#attach(dataset_without_sd)
x_vars <- model.matrix(dataset_6$AGL_thermal_conductivity~. , dataset_6)[,-1]
y_var <- dataset_6$AGL_thermal_conductivity
lambda_seq <- 10^seq(2, -2, by = -.1)

# Splitting the data into test and train
set.seed(1)
train = sample(1:nrow(x_vars), nrow(x_vars)*0.8)
test = -train
x_test = (test)
y_test = y_var[test]

cv_output <- cv.glmnet(x_vars[train,], y_var[train], 
                       alpha = 1, lambda = lambda_seq)

# identifying best lamda
best_lam <- cv_output$lambda.min

#Using this value, let us train the lasso model again
# Rebuilding the model with best lamda value identified
lasso_best <- glmnet(x_vars[train,], y_var[train], alpha = 1, lambda = best_lam)
pred <- predict(lasso_best, s = best_lam, newx = x_vars[test,])


final <- cbind(y_var[test], pred)
# Checking the first six obs
head(final)
#Calculating R2
actual <- y_test
preds <- pred
rss <- sum((preds - actual) ^ 2)
tss <- sum((actual - mean(actual)) ^ 2)
rsq <- 1 - rss/tss
rsq


# Important variables
coef(lasso_best)
#Lasso Regression R2= 0.79