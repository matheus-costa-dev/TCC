require(quantmod)
require(tidyverse)

getPrices = function(folder,symbols,from="2020-01-01",to=Sys.Date()){
  file = paste0("assetReturn",Nsymbols,".rds")
  fullPath = paste(folder,file,sep="/")
  if(file %in% list.files(folder)){
    asset_return = readRDS(fullPath)
  } else {
    prices = getSymbols(symbols,
                        src = "yahoo",
                        from=from,
                        to=to,
                        auto.assign = TRUE) %>%
      map(~Ad(get(.))) %>%
      reduce(merge) %>%
      setNames(symbols)
    
    
    asset_return = Return.calculate(prices,method = "log")  %>% na.omit()
    
    saveRDS(asset_return,fullPath)
  }
  
  return(asset_return)
  
}


getPricesStockMarket = function(folder,symbols,from="2020-01-01",to=Sys.Date()){
  file = paste0("assetReturnStockMarket",Nsymbols,".rds")
  fullPath = paste(folder,file,sep="/")
  if(file %in% list.files(folder)){
    asset_return = readRDS(fullPath)
  } else {
    prices = getSymbols(symbols,
                        src = "yahoo",
                        from=from,
                        to=to,
                        auto.assign = TRUE) %>%
      map(~Ad(get(.))) %>%
      reduce(merge) %>%
      setNames(symbols)
    
    
    asset_return = Return.calculate(prices,method = "log")  %>% na.omit()
    
    saveRDS(asset_return,fullPath)
  }
  
  return(asset_return)
  
}