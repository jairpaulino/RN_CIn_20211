getTfMLP = function(X_trainData, y_trainData){
  #library(keras); library(tensorflow)
  #install_tensorflow(); install_keras()
  #trainSeries = newdata[1:3500,]; allSeries = newdata[1:4092,]
  
  #use_session_with_seed(123)
  
  model = keras_model_sequential()
  model %>% 
    layer_dense(units = 8, activation = "sigmoid", 
                input_shape = 83) %>% 
    layer_dense(units = 20, activation = "relu") %>% 
    layer_dense(units = 20, activation = "sigmoid") %>% 
    layer_dense(units = 20, activation = "relu") %>% 
    layer_dense(units = 3, activatio = "softmax")
  
  model %>% compile(loss = "categorical_crossentropy",
                    optimizer  = "adam",
                    metrics = 'accuracy')
  set.seed(123)
  history = model %>% 
    fit(train, 
        trainLabels,
        batch_size = 10, 
        verbose = T,
        epochs = 50,
        validation_split = 0.25,
    ) #plot(history)
  
  model %>% evaluate(test, testLabels)
  #summary(model)
  
  prob = model %>% 
    predict_proba(as.matrix(test))
  
  pred = model %>% 
    predict_classes(test)
  
  testTarget2 = testTarget
  testTarget2[testTarget2 == "COM ViTIMA"] = 0
  testTarget2[testTarget2 == "SEM ViTIMA"] = 1
  testTarget2[testTarget2 == "ViTIMA FATAL"] = 2
  
  table = table(Predict = pred, Actual = testTarget2)
  s_cor = sum(diag(table))
  s_tot = sum(table)
  acc = s_cor/s_tot; round(acc, 4)
  results = cbind(round(prob,4), pred, testTarget2); View(results)

  # Fitting a basic xgboost model to the training data
  # model <- xgboost(
  #   data = train,
  #   label = trainTarget,
  #   nround = 1,
  #   verbose = FALSE
  # )
  # 
  # explainer <- shapr(train, model)
  # p <- mean(trainTarget)
  
  return(model)
}