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


# Criar função
f3 = function(x1 = 0, x2 = 0){
  n = sin(sqrt(x1^2+x2^2))
  d = sqrt(x1^2+x2^2)
  return(n/d)
}#f3(0, 1)

# Gerar valores aleatório entre -5 e 5 
x1 = sort(runif(min = -5, max = 5, n = 100)) 
x2 = sort(runif(min = -5, max = 5, n = 100)) 
#x1 = seq(-10, 10, 0.25)
#x1 = seq(-5, 5, 0.25)
#x2 = seq(-5, 5, 0.25)
r = matrix(ncol = length(x1), nrow = length(x2))
r_df = data.frame(matrix(ncol = 3, nrow = length(x1)*length(x2)))
colnames(r_df) = c("x1", "x2", "r")
c = 1
for (i in 1:length(x1)){
  for (j in 1:length(x2)){#i=1; j=1
    r[i,j] = f3(x1[i], x2[j])
    r_df$x1[c] = x1[i]
    r_df$x2[c] = x2[j] 
    r_df$r[c] = r[i,j]
    c = c + 1
  }
} #View(r); View(r_df)

p = plot_ly(z = r, type = "surface"); p

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
  hidden = list(c(5), c(5, 5), c(10), c(10, 10),
                c(20), c(20, 20), c(30), c(30, 30)),
  activation = list("Tanh", "TanhWithDropout", 
                    "Rectifier", "RectifierWithDropout",
                    "Maxout", "MaxoutWithDropout"),
  rate = c(0.005, 0.01))
hyper_params

h2o.init(nthreads = -1)
beginTime = proc.time()
dl_grid = h2o.grid(algorithm = "deeplearning",
                   y = "r",
                   x = names(dataNormTrain)[c(1,2)], 
                   keep_cross_validation_models = T,  
                   validation_frame = as.h2o(dataNormValid),
                   training_frame = as.h2o(dataNormTrain),
                   nfolds = 5,
                   epochs = 50,
                   hyper_params = hyper_params,
                   #l1 = 1.0E-5, #ver depois
                   #l2 = 0, 
                   seed = 123,
                   reproducible = T,
                   parallelism = 0
)
procTime = proc.time() - beginTime

dl_grid@summary_table

modelMLP_DEEP = h2o.deeplearning(y = "r",
                                 x = names(dataNormTrain)[c(1,2)], #c(1:7,9)
                                 keep_cross_validation_models = T,  
                                 validation_frame = as.h2o(dataNormValid),
                                 training_frame = as.h2o(dataNormTrain),
                                 activation = 'Tanh', 
                                 nfolds = 5,
                                 hidden = c(20, 20),
                                 epochs = 50,
                                 rate = 0.005,
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
h2o.performance(modelMLP_DEEP, newdata = as.h2o(dataNormTest))

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

# CAP. 05 - Q1c ####

f3 = function(x1 = 0, x2 = 0){
  n = sin(sqrt(x1^2+x2^2))
  d = sqrt(x1^2+x2^2)
  return(n/d)
}#f3(0, 1)

# Create two vectors of random data between -5 and 5 
x1 = sort(runif(min = -5, max = 5, n = 50)) 
x2 = sort(runif(min = -5, max = 5, n = 50)) 
#x1 = seq(-5, 5, 0.25)
#x2 = seq(-5, 5, 0.25)

r = matrix(ncol = length(x1), nrow = length(x2))
r_df = data.frame(matrix(ncol = 3, nrow = length(x1)*length(x2)))
colnames(r_df) = c("x1", "x2", "r")
c = 1
for (i in 1:length(x1)){
  for (j in 1:length(x2)){#i=1; j=1
    r[i,j] = f3(x1[i], x2[j])
    r_df$x1[c] = x1[i]
    r_df$x2[c] = x2[j] 
    r_df$r[c] = r[i,j]
    c = c + 1
  }
} #View(r); View(r_df)

p = plot_ly(z = r, type = "surface"); p

train = r_df[1:round(length(r_df$x1)*0.7,0),]
test = r_df[(round(length(r_df$x1)*0.7,0)+1):length(r_df$x1),]

set.seed(123)
model = neuralnet(r ~ ., 
                  data = train, 
                  hidden = c(5,5), 
                  act.fct = "logistic",
                  rep = 1)
plot(model)
plot(train$r, type = "l", lwd = 2)
points(model$net.result[[1]], col = 2)
lines(model$net.result[[1]], col = 2)

mseTrain = getMSE(train$r, model$net.result[[1]])
maeTrain = getMAE(train$r, model$net.result[[1]])

testRes = predict(model, newdata = test[,1:2])
plot(test$r, type = "l", lwd = 2, ylim = c(-0.2,1.2))
points(testRes, col = 2)
lines(testRes, col = 2)
mseTest= getMSE(test$r, testRes)
maeTest = getMAE(test$r, testRes)
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
