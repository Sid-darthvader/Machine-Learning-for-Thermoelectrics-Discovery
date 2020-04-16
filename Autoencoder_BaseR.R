library(autoencoder)
data=read.csv('Aflow_6.csv', header=TRUE)
summary(data)
data=na.omit(data)
data$Bravias_lattice=NULL
data= data.frame(data)

max = apply(data, 2 , max)
min = apply(data, 2 , min)
scaled = as.data.frame(scale(data, center = min, scale = max - min))

#summary(data)
data1 <-t(scaled)
data1 <-as.matrix ( data1 )

set.seed (7)
n=nrow(data)
train =sample (1:n, 10, FALSE )

fit=autoencode( X.train = data1,#X.train = data1[,train],
                  X.test = NULL,
                  nl = 3,
                  N.hidden = 5,
                  unit.type = "logistic",
                  lambda = 1e-5,
                  beta = 1e-5,
                  rho = 0.07,
                  epsilon =0.1,
                  max.iterations = 100,
                  optim.method = c("BFGS"),
                  rel.tol=0.01,
                  rescale.flag = TRUE,
                  rescaling.offset = 0.001)

#plot(fit)
summary(fit)
fit$mean.error.training.set
features =predict (fit, X.input=data1, hidden.output =TRUE )
features$X.output
autoencoder_reduced_data=features$X.output
pred=predict (fit , X.input = data1 [,train], hidden.output = FALSE )
pred$X.output[,1]
data1[,1]
