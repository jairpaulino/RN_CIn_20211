library(plotly)
library(neuralnet)

# Create function
f1 = function(x1 = 0, x2 = 0){
  return(max(exp(-1*(x1^2)),
             exp(-2*(x2^2)),
             2*exp(-0.5*(x1^2+x2^2))
        )
  )
}

f1(0, 0)

r = NULL
x1 = seq(-5, 5, 0.25); #names(x1) = x1
x2 = seq(-5, 5, 0.25); #names(x2) = x2
r = matrix(ncol = length(x1), nrow = length(x2))
r_df = data.frame(matrix(ncol=3, nrow=length(x1)*length(x2)))
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

# r_df = data.frame(x1, x2)
# for(i in 1:length(x1)){#i=1
#   r_df$r[i] = f1(x1[i], x2[i])
# }#View(r_df)

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

testRes = predict(model, newdata = test[,1:2])
plot(test$r, type = "l", lwd = 2, ylim = c(-0.2,1.2))
points(testRes, col = 2)
lines(testRes, col = 2)


