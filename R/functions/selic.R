require(tidyverse)
require(rbcb)
require(openxlsx)

getSelic = function(start_date="2020-01-01",end_date="2023-01-02"){
  
  
  # os dados obtidos são em taxas selic diárias, sendo necessário a conversão pra taxa anual
  
  selic <- get_series(11,start_date = start_date,end_date = end_date) %>%
    select(1,2) %>%
    filter(date >= start_date & date <= end_date) %>%
    rename(SELIC = `11`) %>%
    mutate(SELIC = SELIC/100,
      SELIC =(1+SELIC)^252 - 1)
  
  return(selic)
}

selicSummary = function(selic,businessDay=F){
  
  if(businessDay){
    nDaysYear = 252
  } else {
    nDaysYear = 360
  }
  
  selic = selic %>% 
    summarise(SELICAnualMedia = mean(SELIC))  %>% 
    mutate(SELICMENSALMedia = (1+SELICAnualMedia)^(1/12) - 1 ,
           SELICDiariaMédia = (1+SELICAnualMedia)^(1/nDaysYear) - 1)
  
  
  return(selic)
}

loadSelicSummary = function(folder="",from="2020-01-01",to=Sys.Date(),businessDay=F){
  
  if(businessDay){
    file = "selicbusinessDay.xlsx" 
  } else {
    file = "selic.xlsx"
  }
  
  fullpath = paste(folder,file,sep="/")
  
  if(file %in% list.files(folder)){
    selic = read.xlsx(fullpath)
    
  } else {
    selic = selicSummary(selic =  getSelic(start_date = from,end_date = to),
                         businessDay=businessDay) 
      
    
    write.xlsx(selic,fullpath)
  }

  return(selic)
}


