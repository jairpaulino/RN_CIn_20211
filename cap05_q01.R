# CAP. 05 - Q1a ####
# Libraries
library(plotly)
library(neuralnet)
source("auxiliar.R")

# Create function
f1 = function(x1 = 0, x2 = 0){
  return(max(exp(-1*(x1^2)),
             exp(-2*(x2^2)),
             2*exp(-0.5*(x1^2+x2^2))))
}#f1(0, 0)

# Create two vectors of random data between -5 and 5 
x1 = sort(runif(min = -5, max = 5, n = 50)) 
x2 = sort(runif(min = -5, max = 5, n = 50)) 
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

# CAP. 05 - Q1b ####

f2 = function(x1 = 0, x2 = 0){
  f = 0.5+0.1*(x1^2)*cos(x2+3)+0.4*x1*x2*exp(1-(x2)^2)
  return(f)
}#f2(0, 0)

# Create two vectors of random data between -5 and 5 
x1 = sort(runif(min = -10, max = 10, n = 50)) 
x2 = sort(runif(min = -10, max = 10, n = 50)) 
#x1 = seq(-10, 10, 0.25)
#x2 = seq(-10, 10, 0.25)

r = matrix(ncol = length(x1), nrow = length(x2))
r_df = data.frame(matrix(ncol = 3, nrow = length(x1)*length(x2)))
colnames(r_df) = c("x1", "x2", "r")
c = 1
for (i in 1:length(x1)){
  for (j in 1:length(x2)){#i=1; j=1
    r[i,j] = f2(x1[i], x2[j])
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
                  hidden = c(5), 
                  act.fct = "logistic",
                  stepmax = 1e+06,
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
