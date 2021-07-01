rm(list = ls()); graphics.off()

# Abrir bibliotecas
library(caTools)
library(h2o)
library(keras)
library(tensorflow)
library(pROC)
library(neuralnet)
library(dummies)
library(plotly)

sigmoid = function(x = 0) {
  s = 1 / (1 + exp(-x))
  return(s)
}

#N = sort(rnorm(0, 5, n = 200)); plot(N)
#N = rnorm(0, 5, n = n); plot(N)
n = 1000
x1 = (runif(min = 0, max = 1, n = n)); plot(x1)
x2 = (runif(min = 0, max = 1, n = n)); plot(x2) 

f5 = function(x1 = 0, x2 = 0, N = 0){
  f = sigmoid(x1+2*x2)+0.5*(x1-x2)^2+0.5*rnorm(0, 1, n = n)
  return(f)
}#f5(0,0,0)

f_df = data.frame(matrix(ncol = 3, nrow = length(x1)))
colnames(f_df) = c("x1", "x2", "f")#"N", "f")
f_df$x1 = x1
f_df$x2 = x2
#f_df$N = N
f_df$f = f5(x1, x2)#, N)
head(f_df)

X_trainData = f_df[,-1] %>% as.matrix()
y_trainData = f_df[,1] 

# grid search values
values =      c(0,    1e-1,   1e-2,    1e-3,     1e-4,   1e-5   , 1e-6)
valuesNames = c("0", "0_1", "0_01", "0_001", "0_0001", "0_00001", "0_000001")
all_train = NULL; all_test = NULL 

for (param in 1:length(values)){#param = 1
  model = keras_model_sequential()
  model %>% 
    layer_dense(units = 30, 
                activation = "relu", 
                regularizer_l2(l = values[param]),
                input_shape = 2) %>% 
    layer_dense(units = 30, activation = "sigmoid") %>% 
    #layer_dense(units = 30, activation = "sigmoid") %>% 
    #layer_dense(units = 20, activation = "relu") %>% 
    layer_dense(units = 1, activatio = "sigmoid")
  
  model %>% compile(loss = "mean_squared_error",
                    optimizer  = "adam",
                    metrics = 'mean_absolute_error')
  set.seed(123)
  history = model %>% 
    fit(X_trainData, 
        y_trainData,
        batch_size = 1, 
        verbose = T,
        epochs = 5,
        rate = 0.001,
        validation_split = 0.8,
        #activity_regularizer = regularizer_l2(l = 0)
    ) #plot(history)
# 
#   jpeg(filename = paste("Results/", valuesNames[param], 
#                        ".jpeg", sep = ""),
#       width = 600, height = 400)
#   p = plot(history)
#   dev.off()

  plot(history)
  ggsave(filename = paste("Results/", valuesNames[param], 
                          ".jpeg", sep = ""),)
  
  result_train = model %>% evaluate(X_trainData[1:800,], y_trainData[1:800])
  all_train[param] = result_train[1]
  result_test = model %>% evaluate(X_trainData[801:1000,], y_trainData[801:1000])
  all_test[param] = result_test[1]
}

plot(all_train, type="l", ylim = c(min(min(all_train),min(all_test)), 
                                       max(max(all_train), max(all_test))))
lines(all_test, type="l", col = 2)

forecast_results = as.data.frame(h2o.predict(modelMLP_DEEP,                                          newdata = as.h2o(f_df), 
                                             rep = 5, seed = 123))
plot(f_df$f~forecast_results[[1]], lwd = 1,
     xlab="Aproximação", ylab="Dados")
abline(a=c(0,0), b=c(1,1), col = 2, lty = 2, lwd =2)
plot(f_df$f[1:750], type = "l", lwd = 2)#, ylim = c(-0.2, 2))
points(forecast_results[[1]], col = 2)
lines(forecast_results[[1]], col = 2)

plot(f_df$f[751:1000]~forecast_results[[1]][751:1000], lwd = 1,
     xlab="Aproximação", ylab="Dados")
abline(a=c(0,0), b=c(1,1), col = 2, lty = 2, lwd =2)
plot(f_df$f[751:1000], type = "l", lwd = 2)#, ylim = c(-0.2, 2))
points(forecast_results[[1]], col = 2)
lines(forecast_results[[1]], col = 2)




# h2o.init(nthreads = -1)
# modelMLP_DEEP = h2o.deeplearning(y = "f",
#                                  x = names(f_df)[c(1,2,3)], #c(1:7,9)
#                                  #keep_cross_validation_models = T,  
#                                  training_frame = as.h2o(f_df[1:600,]),
#                                  validation_frame = as.h2o(f_df[601:1000,]),
#                                  regression_stop = -1,
#                                  adaptive_rate = F,
#                                  #activation = 'Tanh', 
#                                  nfolds = 5,
#                                  hidden = c(10, 10, 10),
#                                  epochs = 50,
#                                  rate = 0.001,
#                                  mini_batch_size = 10,
#                                  l2 = 0.5, 
#                                  seed = 123,
#                                  reproducible = T,
#                                  #score_each_iteration = TRUE
#                                  #ver depois
#                                  #train_samples_per_iteration = -2
#                                  #stopping_rounds = 2,
#                                  #stopping_tolerance = 0.00001)
# )
# plot(modelMLP_DEEP)
# modelMLP_DEEP
