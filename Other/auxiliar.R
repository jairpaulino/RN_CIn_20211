getMSE = function(target, forecast){
  se = (target - forecast)^2
  mse = mean(se)
  return(mse)
}

getMAE = function(target, forecast){
  sae = sum(abs(target - forecast))
  mae = mean(sae)
  return(mae)
}

normalize_2 = function(array, min, max){
  #Normalize to [0, 1]
  #array = r_df$x1
  #min = min(array = r_df$x1)
  #max = max(array = r_df$x1)
  range = max - min
  norm1 = (array - min) / range
  
  #Then scale to [x,y]
  #range2 = y - x
  #normalized = (norm1*range2) + x
  return(norm1)
}

denormalize = function(array, min, max){
  #Normalize to [0, 1]
  #array = r_df$x1
  #min = min(array = r_df$x1)
  #max = max(array = r_df$x1)
  range = max - min
  denorm = range*array + min
  return(denorm)
}


