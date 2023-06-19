toPercent = function(val,multipy=T){
  if(multipy){
    val = round(val * 100,2)
  } else {
    val = round(val,4)
  }
  res = paste0(val,"%")
  return(res)
}
