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

