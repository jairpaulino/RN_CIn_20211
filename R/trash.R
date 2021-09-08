library(tidyverse) 
library(neuralnet)

print(iris)
iris <- iris %>%  
  mutate(Species = as_factor(Species))

draw_boxplot <- function(){ 
  iris %>%  
    pivot_longer(1:4, names_to = "attributes") %>%  
    ggplot(aes(attributes, value, fill = attributes)) + 
    geom_boxplot() 
}
draw_boxplot()

iris <- iris %>%  
  mutate(across(Sepal.Width, ~squish(.x, quantile(.x, c(0.05, 0.95)))))

iris <- iris %>%  
  mutate(across(1:4, scale))

training_data_rows <- floor(0.70 * nrow(iris))          
set.seed(123) 
training_indices <- sample(c(1:nrow(iris)), training_data_rows)
training_data <- iris[training_indices,] 
test_data <- iris[-training_indices,]

nn=neuralnet(NATACIN  ~ .,  
             data = newdata[3501:4092,], 
             hidden=c(2,2), 
             linear.output = FALSE)
plot(nn)

predict <- function(data){ #data=newdata[3501:4092,]
  prediction <- data.frame(neuralnet::compute(nn, data.frame(data[,-1]))$net.result) 
  labels <- c("SEM ViTIMA", "COM ViTIMA ") 
  prediction_label <- data.frame(max.col(prediction)) %>%  
    mutate(prediction=labels[max.col.prediction.]) %>%  
    select(2) %>%  
    unlist() 
  
  table(data$NATACIN, prediction_label) 
}

predict(training_data)
predict(test_data)
