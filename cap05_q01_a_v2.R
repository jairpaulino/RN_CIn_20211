# Q1 - A ####
# Limpando ambiente
rm(list = ls()); graphics.off()

# Abrir bibliotecas
library(caTools)
library(h2o)
library(pROC)
library(neuralnet)
library(dummies)
library(plotly)
library(keras)
library(tensorflow)

# Importar funções auxiliares
source("auxiliar.R")

# Criar função
f1 = function(x1 = 0, x2 = 0){
  return(max(exp(-1*(x1^2)),
             exp(-2*(x2^2)),
             2*exp(-0.5*(x1^2+x2^2))))
}#f1(0, 0)

# Gerar valores aleatório entre -5 e 5 
#x1 = sort(runif(min = -5, max = 5, n = 100)) 
#x2 = sort(runif(min = -5, max = 5, n = 100)) 
x1 = seq(-5, 5, 0.25)
x2 = seq(-5, 5, 0.25)
r = matrix(ncol = length(x1), nrow = length(x2))
r_df = data.frame(matrix(ncol = 3, nrow = length(x1)*length(x2)))
colnames(r_df) = c("x1", "x2", "r")
c = 1
for (i in 1:length(x1)){
  for (j in 1:length(x2)){#i=1; j=1
    r[i,j] = f1(x1[i], x2[j])
    r_df$x1[c] = x1[i]
    r_df$x2[c] = x2[j] 
    r_df$r[c] = r[i,j]
    c = c + 1
  }
} #View(r); View(r_df)

# Geração do gráfico
axx <- list(
  #nticks = 4,
  range = c(-4,4),
  title = "x1"
)

axy <- list(
  #nticks = 4,
  range = c(-4,4),
  title = "x2"
)

axz <- list(
  title = "f(x1,x2)"
)

p = plot_ly(x = ~r_df$x1, y = ~r_df$x2, z = ~r_df$r, type = "mesh3d")
p <- p %>% layout(scene = list(xaxis=axx,
                               yaxis=axy,
                               zaxis=axz)); p


# Normalizar dados
dataNorm = data.frame(matrix(ncol = 3, nrow = length(r_df$x1)))
colnames(dataNorm) = c("x1", "x2", "r")
dataNorm$x1 = normalize_2(array = r_df$x1, min = min(r_df$x1), max = max(r_df$x1))
dataNorm$x2 = normalize_2(array = r_df$x2, min = min(r_df$x2), max = max(r_df$x2))
dataNorm$r = r_df$r 
#dataNorm = dataNorm %>% as.matrix()

# Separar em conjunto de treinamento, validacao (20%) e teste (20%)
set.seed(123)
sample = sample.split(dataNorm[[1]], SplitRatio = 0.8)
dataNormTrainValid = subset(dataNorm, sample == T) #plot.ts(dataNormTrain)
dataNormTest = subset(dataNorm, sample == F) #plot.ts(dataNormTest)
sample_2 = sample.split(dataNormTrainValid[[1]], SplitRatio = 0.75)
dataNormTrain = subset(dataNormTrainValid, sample_2 == T) #plot.ts(dataNormTrain)
dataNormValid = subset(dataNormTrainValid, sample_2 == F) #plot.ts(dataNormTest)

# Treinar a rede neural MLP
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
                                 #stopping_metric = "MSE"
                                 nfolds = 5,
                                 hidden = c(10, 10),
                                 epochs = 50,
                                 rate = 0.01,
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
plot(dataNormTest$r, type = "l", lwd = 2, ylim = c(-0.2, 2))
points(forecast_results[[1]], col = 2)
lines(forecast_results[[1]], col = 2)

Erro = dataNormTest$r - forecast_results[[1]]
hist(Erro)
lillie.test(Erro)

# Geração do gráfico
axx <- list(
  #nticks = 4,
  range = c(-4,4),
  title = "x1"
)

axy <- list(
  #nticks = 4,
  range = c(-4,4),
  title = "x2"
)

axz <- list(
  title = "f(x1,x2)"
)

x1_denorm = denormalize(dataNormTest$x1, min = min(r_df$x1), max = max(r_df$x1)) 
x2_denorm = denormalize(dataNormTest$x2, min = min(r_df$x1), max = max(r_df$x1)) 

p = plot_ly(x = ~x1_denorm, 
            y = ~x2_denorm, 
            z = ~forecast_results[[1]], 
            type = "mesh3d",
            intensity = c(0, 0.33, 0.66, 1), 
            colors = colorRamp(c("red", "green", "blue"))
            )

p <- p %>% layout(scene = list(xaxis=axx,
                               yaxis=axy,
                               zaxis=axz)); 
p
