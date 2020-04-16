library(h2o)
h2o.init()
attach(train_cset)
y <- "AGL_thermal_conductivity"
x <- setdiff(names(train_cset), y)
x
x=x[1:10]
train = train_cset
train = as.h2o(train_cset)
#train[,11]
# For binary classification, response should be a factor
#train[,y] <- as.factor(train[,y])
#test[,y] <- as.factor(test[,y])

# Run AutoML for 20 base models (limited to 1 hour max runtime by default)
aml <- h2o.automl(y=11,
                  training_frame = train,
                  max_models = 20,
                  seed = 1)

# View the AutoML Leaderboard
lb <- aml@leaderboard
print(lb, n = nrow(lb))  # Print all rows instead of default (6 rows)
