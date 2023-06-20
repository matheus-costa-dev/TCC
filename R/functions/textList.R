textList = function(x){
  for(i in 1:length(x)){
    if(i == 1){
      texto = x[i]
    } else if (i == length(x)){
      texto = paste(texto,x[i], sep = " e ")
    } else {
      texto = paste(texto,x[i],sep=", ")
    }
  }
  return(texto)
}
