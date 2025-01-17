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
library(latex2exp)

sigmoid = function(x = 0) {
  s = 1 / (1 + exp(-x))
  return(s)
}

#N = sort(rnorm(0, 5, n = 200)); plot(N)
#N = rnorm(0, 5, n = n); plot(N)
n = 200
x1 = (runif(min = -1, max = 1, n = n)); plot(x1)
x2 = (runif(min = -1, max = 1, n = n)); plot(x2) 

f5 = function(x1 = 0, x2 = 0){
  f = sigmoid(x1+2*x2)+0.5*(x1-x2)^2+0.5*rnorm(0, 0.05, n = 1)
  return(f)
}#f5(0,0,0)

f_df = data.frame(matrix(ncol = 3, nrow = length(x1)))
colnames(f_df) = c("x1", "x2", "f")#"N", "f")
f_df$x1 = x1
f_df$x2 = x2
#f_df$N = N
f_df$f = f5(x1, x2)
png("Results/x1_x2_y.png", width = 600, height = 400, res = 100)
plot(f_df$f, xlab = "índice", 
     ylab = TeX("y = f(x_1, x_2)+N"),
     col = "#009999", pch = 19)
dev.off()
head(f_df)

X_Data = f_df[,-1] %>% as.matrix()
y_Data = f_df[,1] 

perc = 0.7
x_trainData = X_Data[1:round(perc*length(X_Data[,1]),0),]
x_testData = X_Data[(round(perc*length(X_Data[,1]),0)+1):length(X_Data[,1]),]

y_trainData = y_Data[1:round(perc*length(y_Data),0)]
y_testData = y_Data[(round(perc*length(y_Data),0)+1):length(X_Data[,1])]


# grid search values
values =      c(0, 1e-6, 1e-5, 1e-4, 1e-3, 1e-2, 1e-1)
valuesNames = c("0", "1e-6", "1e-5", "1e-4", "1e-3", "1e-2", "1e-1")
#all_train = rev(all_train)
#all_test = rev(all_test)
#values = rev(values)

all_train = NULL; all_test = NULL 

for (param in 1:length(values)){#param = 1
  model = keras_model_sequential()
  model %>%
    layer_dense(units = 2, 
                activation = "linear", 
                activity_regularizer = regularizer_l2(l = values[param]),
                #regularizer_l2(l = values[param]),
                input_shape = 2) %>% 
    layer_dense(units = 1, activation = "linear")
  
  model %>% compile(loss = "mean_squared_error",
                    optimizer  = "rmsprop",
                    metrics = 'mean_absolute_error')
  set.seed(123)
  history = model %>% 
    fit(x_trainData, 
        y_trainData,
        batch_size = 10, 
        verbose = T,
        epochs = 30, 
        rate = 0.01,
        validation_data = c(list(x_testData), list(y_testData))
        #
    ) #plot(history)

  plot(history)
  ggsave(filename = paste("Results/", valuesNames[param], 
                          ".jpeg", sep = ""))
  
  result_train = model %>% evaluate(x_trainData, y_trainData)
  all_train[param] = result_train[1]
  result_test = model %>% evaluate(x_testData, y_testData)
  all_test[param] = result_test[1]
}

results_df = data.frame("L2" = c(seq(1,7), seq(1,7)),#c(as.numeric(values), as.numeric(values)),
                        "Loss" = c(all_train, all_test),
                        "Phase" = c(rep("Train", 7), rep("Test", 7)))

# resultsValues = c(all_train, all_test)
# resultsLabels = c(rep("Train", 6), rep("Test", 6))
# results_df = data.frame("MSE" = resultsValues, "Phase" = resultsLabels)

#df <- gather(df, event, total, X:Y) #Create long format

plot <- ggplot(results_df, aes(x = L2, y = Loss, fill = Phase))
plot <- plot + geom_bar(stat = "identity", position = 'dodge',  colour="black")
plot <- plot + scale_x_continuous(breaks = 1:7, labels=c(valuesNames))
plot <- plot + coord_cartesian(ylim=c(0.295,0.315))
plot
ggsave(filename = paste("Results/", "l2_params_2",
                        ".jpeg", sep = ""))
write.csv2(results_df, file="Results/param_mse_l2.csv")
