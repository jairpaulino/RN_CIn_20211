getAccidentTS = function(data, local = NULL){
  #data = outputData; local = NULL
  
  if(is.null(local)){
    timeSeries = data %>% 
      group_by(data) %>% summarise(QntAcid=sum(count), Qntvit=sum(sum))
  }else{
    acid_local = data %>% 
      filter(bairro == local)
    
    timeSeries = acid_local %>% 
      group_by(data) %>% summarise(QntAcid=sum(count), Qntvit=sum(sum))
  }
  return(timeSeries)
}

#ts = getAccidentTS(outputData, local = "BOA VIAGEM")
#plot.ts(ts$Total); plot.ts(ts$Qntvit)



