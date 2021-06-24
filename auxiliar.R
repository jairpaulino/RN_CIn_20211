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

normalize_2 = function(array, x = 0.2, y = 0.8){
  #Normalize to [0, 1]
  m = min(array)
  range = max(array) - m
  norm1 = (array - m) / range
  
  #Then scale to [x,y]
  range2 = y - x
  normalized = (norm1*range2) + x
  return(normalized)
}

