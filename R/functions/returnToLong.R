require(tidyverse)

returnToLong = function(df,symbols){
  
  asset_return_long = df %>% 
    as.data.frame(row.names = index(.)) %>% 
    rownames_to_column("data") %>%
    pivot_longer(cols =  all_of(symbols),names_to = "asset",values_to = "returns")  %>%
    arrange(asset)
  
  return(asset_return_long)
  
}