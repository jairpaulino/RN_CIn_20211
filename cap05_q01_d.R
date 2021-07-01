# Q1 - C ####
# Limpando ambiente
rm(list = ls()); graphics.off()

# Abrir bibliotecas
library(caTools)
library(h2o)
library(pROC)
library(neuralnet)
library(dummies)
library(plotly)

# Importar funções auxiliares
source("auxiliar.R")

f4 = function(x = 0){
  f = sqrt(2)*sin(x)+sqrt(2)*cos(x)-sqrt(2)*cos(3*x)+sqrt(2)*cos(3*x)
  return(f)
}#f4(0)

# Create two vectors of random data between -5 and 5 
x1 = sort(runif(min = -0, max = 1, n = 1000)) 
#x1 = seq(-10, 10, 0.01)

r = NULL
r_df = data.frame(matrix(ncol = 2, nrow = length(x1)))
colnames(r_df) = c("x1", "r")
c = 1
for (i in 1:length(x1)){
  r[i] = f4(x1[i])
  r_df$x1[c] = x1[i]
  r_df$r[c] = r[i]
  c = c + 1
} #View(r_df)

#plot(r_df$r ~ r_df$x1, type = "l")

plot(r_df$r~r_df$x1, type="l", ylab="f(x)", xlab="x")

# Normalizar dados
dataNorm = as.data.frame(sapply(r_df[,1:2], FUN = normalize_2))
dataNorm$r = r_df$r #plot.ts(dataNorm$r)

# Separar em conjunto de validacao (20%)
set.seed(123)
sample = sample.split(dataNorm[[1]], SplitRatio = 0.8)
dataNormTrainValid = subset(dataNorm, sample == T) #plot.ts(dataNormTrain)
dataNormTest = subset(dataNorm, sample == F) #plot.ts(dataNormTest)
sample_2 = sample.split(dataNormTrainValid[[1]], SplitRatio = 0.8)
dataNormTrain = subset(dataNormTrainValid, sample_2 == T) #plot.ts(dataNormTrain)
dataNormValid = subset(dataNormTrainValid, sample_2 == F) #plot.ts(dataNormTest)

# Criando Grid
hyper_params <- list(
  hidden = list(c(50), c(50, 50), c(100), c(100, 100),
                c(200), c(200, 200), c(300), c(300, 300)),
  activation = list("Tanh", "TanhWithDropout", 
                    "Rectifier", "RectifierWithDropout",
                    "Maxout", "MaxoutWithDropout"),
  rate = c(0.005, 0.001, 0.01))
hyper_params

h2o.init(nthreads = -1)
beginTime = proc.time()
dl_grid = h2o.grid(algorithm = "deeplearning",
                   y = "r",
                   x = names(dataNormTrain)[c(1)], 
                   keep_cross_validation_models = T,  
                   validation_frame = as.h2o(dataNormValid),
                   training_frame = as.h2o(dataNormTrain),
                   nfolds = 10,
                   epochs = 100,
                   hyper_params = hyper_params,
                   #l1 = 1.0E-5, #ver depois
                   #l2 = 0, 
                   seed = 123,
                   reproducible = T,
                   parallelism = 1
)
procTime = proc.time() - beginTime

dl_grid@summary_table

modelMLP_DEEP = h2o.deeplearning(y = "r",
                                 x = names(dataNormTrain)[c(1)], #c(1:7,9)
                                 keep_cross_validation_models = T,  
                                 validation_frame = as.h2o(dataNormValid),
                                 training_frame = as.h2o(dataNormTrain),
                                 activation = 'Tanh', 
                                 nfolds = 5,
                                 hidden = c(30, 30, 30),
                                 epochs = 50,
                                 rate = 0.004,
                                 #l1 = 1.0E-5, #ver depois
                                 #l2 = 0, 
                                 seed = 123,
                                 reproducible = T
                                 #ver depois
                                 #train_samples_per_iteration = -2
                                 #stopping_rounds = 2,
                                 #stopping_tolerance = 0.00001)
)
# sampled training data (from model building)
h2o.performance(modelMLP_DEEP, train = T)
h2o.performance(modelMLP_DEEP, valid = T)
h2o.performance(modelMLP_DEEP, newdata = as.h2o(dataNormTest$x1))

summary(modelMLP_DEEP)
plot(modelMLP_DEEP)

# Predicting the Test set results
forecast_results = as.data.frame(h2o.predict(modelMLP_DEEP, 
                                             newdata = as.h2o(dataNormTest), 
                                             rep = 5, seed = 123))

#mseTrain = getMSE(dataNormTest$r, forecast_results[[1]])
#maeTrain = getMAE(dataNormTest$r, forecast_results[[1]])

plot(dataNormTest$r~forecast_results[[1]], lwd = 1,
     xlab="Aproximação", ylab="Dados")
abline(a=c(0,0), b=c(1,1), col = 2, lty = 2, lwd =2)
plot(dataNormTest$r, type = "l", lwd = 2)#, ylim = c(-0.2, 2))
points(forecast_results[[1]], col = 2)
lines(forecast_results[[1]], col = 2)

# CAP. 05 - Q1d ####

f4 = function(x = 0){
  f = sqrt(2)*sin(x)+sqrt(2)*cos(x)-sqrt(2)*cos(3*x)+sqrt(2)*cos(3*x)
  return(f)
}#f4(0)

# Create two vectors of random data between -5 and 5 
x1 = sort(runif(min = -10, max = 10, n = 1000)) 
#x1 = seq(-10, 10, 0.01)

r_df = data.frame(matrix(ncol = 2, nrow = length(x1)))
colnames(r_df) = c("x1", "r")
c = 1
for (i in 1:length(x1)){
  r[i] = f4(x1[i])
  r_df$x1[c] = x1[i]
  r_df$r[c] = r[i]
  c = c + 1
} #View(r_df)

plot(r_df$r ~ r_df$x1, type = "l")

train = r_df[1:round(length(r_df$x1)*0.9,0),]
test = r_df[(round(length(r_df$x1)*0.9,0)+1):length(r_df$x1),]

set.seed(123)
model = neuralnet(r ~ ., 
                  data = train, 
                  hidden = c(10,10), 
                  act.fct = "logistic",
                  rep = 1)
#plot(model)
plot(train$r, type = "l", lwd = 2)
points(model$net.result[[1]], col = 2)
lines(model$net.result[[1]], col = 2)

mseTrain = getMSE(train$r, model$net.result[[1]])
maeTrain = getMAE(train$r, model$net.result[[1]])

testRes = predict(model, newdata = test[,1:2])
plot(test$r, type = "l", lwd = 2, ylim=c(-2, 5))
points(testRes, col = 2)
lines(testRes, col = 2)
mseTest= getMSE(test$r, testRes)
maeTest = getMAE(test$r, testRes)
