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

f5 = function(x1 = 0, x2 = 0){
  f = sigmoid(x1+2*x2)+0.5*(x1-x2)^2+0.5*rnorm(0, 1, n = 1)
  return(f)
}#f5(0,0,0)

# Training sample
n = 200
x1 = (runif(min = -1, max = 1, n = n)); plot(x1)
x2 = (runif(min = -1, max = 1, n = n)); plot(x2) 
# x1 = seq(-1, 1, 0.01); plot(x1)
# x2 = seq(-1, 1, 0.01); plot(x2) 
train_df = data.frame(matrix(ncol = 3, nrow = length(x1)))
colnames(train_df) = c("x1", "x2", "f")#"N", "f")
train_df$x1 = x1
train_df$x2 = x2
#f_df$N = N
train_df$f = f5(x1, x2)
png("Results/x1_x2_y.png", width = 600, height = 400, res = 100)
plot(train_df$f, xlab = "índice", 
     ylab = TeX("y = f(x_1, x_2)+N"),
     col = "#009999", pch = 19)
dev.off()
head(train_df)

x_train_df = train_df[,-1] %>% as.matrix()
y_train_df = train_df[,1] 

# Test sample
n = 1000
x1 = (runif(min = -1, max = 1, n = n)); plot(x1)
x2 = (runif(min = -1, max = 1, n = n)); plot(x2) 
test_df = data.frame(matrix(ncol = 3, nrow = length(x1)))
colnames(test_df) = c("x1", "x2", "f")#"N", "f")
test_df$x1 = x1
test_df$x2 = x2
#f_df$N = N
test_df$f = f5(x1, x2)
png("Results/x1_x2_y_test.png", width = 600, height = 400, res = 100)
plot(test_df$f, xlab = "índice", 
     ylab = TeX("y = f(x_1, x_2)+N"),
     col = "#009999", pch = 19)
dev.off()
head(test_df, 3)

x_test_df = test_df[,-1] %>% as.matrix()
y_test_df = test_df[,1] 

# grid search values
values =      c(1e-6, 1e-5, 1e-4, 1e-3, 1e-2, 1e-1)
valuesNames = c("1e-6", "1e-5", "1e-4", "1e-3", "1e-2", "1e-1")
#all_train = rev(all_train)
#all_test = rev(all_test)
#values = rev(values)

all_train = NULL; all_test = NULL 

for (param in 1:length(values)){#param = 1
  model = keras_model_sequential()
  model %>%
    layer_dense(units = 5, 
                activation = "linear", 
                activity_regularizer = regularizer_l2(l = values[param]),
                #regularizer_l2(l = values[param]),
                input_shape = 2) %>% 
    layer_dense(units = 1, activation = "linear")
  
  model %>% compile(loss = "mean_squared_error",
                    optimizer  = "sgd",
                    #metrics = 'mean_absolute_error'
                    )
  set.seed(123)
  history = model %>% 
    fit(x_train_df, 
        y_train_df,
        batch_size = 1, 
        verbose = T,
        epochs = 500, 
        rate = 0.01,
        validation_data = c(list(x_test_df), list(y_test_df))
        #
    ) #plot(history)
  
  plot(history)
  ggsave(filename = paste("Results/", valuesNames[param], 
                          ".jpeg", sep = ""))
  
  result_train = model %>% evaluate(x_train_df, y_train_df)
  all_train[param] = result_train[1]
  result_test = model %>% evaluate(x_test_df, y_test_df)
  all_test[param] = result_test[1]
}

results_df = data.frame("Weight_decay" = c(seq(1,length(values)), seq(1,length(values))),#c(as.numeric(values), as.numeric(values)),
                        "Loss" = c(all_train, all_test),
                        "Phase" = c(rep("Train", length(values)), rep("Test", length(values))))

# resultsValues = c(all_train, all_test)
# resultsLabels = c(rep("Train", 6), rep("Test", 6))
# results_df = data.frame("MSE" = resultsValues, "Phase" = resultsLabels)

#df <- gather(df, event, total, X:Y) #Create long format

plot <- ggplot(results_df, aes(x = Weight_decay, y = Loss, fill = Phase))
plot <- plot + geom_bar(stat = "identity", position = 'dodge',  colour="black")
plot <- plot + scale_x_continuous(breaks = 1:length(values), labels=c(valuesNames))
plot <- plot + coord_cartesian(ylim=c(0.2,0.35))
plot
ggsave(filename = paste("Results/", "l2_params_2",
                        ".jpeg", sep = ""))
write.csv2(results_df, file="Results/param_mse_l2.csv")
