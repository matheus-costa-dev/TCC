library(tidyverse)
library(quantmod)
library(scales)
library(PerformanceAnalytics)
library(openxlsx)

citeToBibTex= function(packs = c("base","tidyverse","quantmod","scales","PerformanceAnalytics")){
  map(packs, 
      ~ write_lines(toBibtex(citation(.x)),paste0("citacao/",.x,".bib"))
  )
}


getAllPossibilites = function(symbols,by){
  
  Nsymbols = length(symbols)
  
  possibilities = rep(list(seq(0,1,by)),Nsymbols) %>%
    reduce(cbind) %>% 
    as_tibble() %>%
    setNames(symbols)
  
  possibilities = do.call(expand.grid, possibilities)
  
  print("vai iniciar o processo de filtrar soma dos pesos == 1")
  
 possibilities =   tryCatch({
                              possibilities %>% 
                                mutate(total = rowSums(pick(where(is.numeric)))) %>%
                                filter(total == 1) %>%
                                select(-total)
                              },error = function(e){
                                          print('deu erro o metodo padrão, vai tentar de outra forma')
                                
                                          start = 1
                                          end = 10000000
                                          endBy = end
                                          t = nrow(possibilities)/end
                                          i = 1
                                          allPossibilites = data.frame()
                                          
                                          print(paste0(i," - ",start,":",endBy))
                                          
                                          while(i < t ){
                                            
                                                tmp = possibilities %>% 
                                                  slice(start:endBy) %>%
                                                  mutate(total = rowSums(pick(where(is.numeric)))) %>%
                                                  filter(total == 1) %>%
                                                  select(-total)
                                                
                                                allPossibilites = rbind(allPossibilites,tmp)
                                                
                                                i = i+1
                                                start = endBy + 1
                                                endBy = endBy + end
                                                
                                                gc()
                                                print(paste0(i," - ",start,":",endBy))
                                                
                                          }
                                          
                                          print('finalizou')
                                          
                                          return(allPossibilites)
                                          
                                          #possibilities = allPossibilites
                                          #rm(allPossibilites)
                                
                              })
 
 
  
  return(possibilities)
}




getData = function(symbols,
                  from=Sys.Date() - lubridate::years(5) - 5,
                  to=Sys.Date()){
  prices = getSymbols(symbols,
                      src = "yahoo",
                      from=from,
                      to=to,
                      auto.assign = TRUE) %>%
    map(~Ad(get(.))) %>%
    reduce(merge) %>%
    setNames(symbols)
  
  
  return(prices)
}


getReturn = function(prices){
  asset_return = Return.calculate(prices,method = "log")  %>% na.omit()
  return(asset_return)
}

getReturnLong = function(asset_return,symbols){
  asset_return_long = asset_return %>%  as.data.frame(row.names = index(.)) %>% rownames_to_column("data") %>%
    pivot_longer(cols =  all_of(symbols),names_to = "asset",values_to = "returns")  %>%
    arrange(asset)
  return(asset_return_long)
}

getPossibilities = function(symbols,by){
  possibilities = rep(list(seq(0,1,by)),length(symbols)) %>%
    reduce(cbind) %>% as_tibble() %>%
    setNames(symbols)
  
  possibilities = do.call(expand.grid, possibilities[1:length(symbols)])
  rows = which(rowSums(possibilities) == 1)
  possibilities = possibilities[rows,]
  return(possibilities)
}

getPortfolioReturn = function(symbols,asset_return){
  w = 1/length(symbols) %>% rep(length(symbols))
  
  portfolio_return = Return.portfolio(asset_return,weights = w,rebalance_on = "months")
  return(portfolio_return)

}

evaluatePossibilities = function(possibilities,asset_return){
  for(i in 1:nrow(possibilities)) {
    
    
    w = possibilities[i,1:length(symbols)]  %>% as.numeric()
    
    portfolio_return = Return.portfolio(asset_return,weights = w,rebalance_on = "months")
    
    possibilities[i ,"sharpeRatePortfolio"] = SharpeRatio(portfolio_return,rf=rfr,FUN = "StdDev")
    possibilities[i ,"sd"] = sd(portfolio_return$portfolio.returns)
    possibilities[i,"mean"] = mean(portfolio_return$portfolio.returns)
    
    print(paste(i,nrow(possibilities),sep = " - "))
    
  }
  
  return(possibilities)
  
}

getPortfolioReturnOptimized = function(max_sr,asset_return){
  w = max_sr[1:length(symbols)] %>% unlist()
  
  portfolio_return_opmitized = Return.portfolio(asset_return,
                                                weights = w,
                                                rebalance_on = "months")
  
  return(portfolio_return_opmitized)
  
}

routine = function(symbols,
                   from=Sys.Date() - lubridate::years(5),
                   to=Sys.Date(),
                   by=.2) {
  prices = getData(symbols)
  asset_return = getReturn(prices)
  covariance = cov(asset_return)
  correlation = cor(asset_return)
  asset_return_long= getReturnLong(prices,symbols)
  portfolio_return = getPortfolioReturn(symbols,asset_return)
  sd_portfolio = sd(portfolio_return$portfolio.returns)
  mean_portfolio = mean(portfolio_return$portfolio.returns)
  sr_portfolio = SharpeRatio(portfolio_return,rf=rfr,FUN = "StdDev")
  possibilities = getPossibilities(symbols,by)
  possibilities = evaluatePossibilities(possibilities,asset_return)
  min_var <- possibilities[which.min(possibilities$sd),]
  max_sr <- possibilities[which.max(possibilities$sharpeRatePortfolio),]
  max_re <- possibilities[which.max(possibilities$mean),]
  portfolio_return_opmitized = getPortfolioReturnOptimized(max_sr,asset_return)
  sd_portfolio_optimized = sd(portfolio_return_opmitized$portfolio.returns)
  mean_portfolio_optimized = mean(portfolio_return_opmitized$portfolio.returns)
  sr_portfolio_optimized = max_sr$sharpeRatePortfolio
  
  return(list(asset_return=asset_return),)
}
