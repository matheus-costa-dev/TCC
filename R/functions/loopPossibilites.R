require(tidyverse)

loopPossibilites = function(possibilities,asset_return,Nsymbols,rfr){
  
  Npossibilites = nrow(possibilities)
  for(i in 1:Npossibilites) {
    
    
    w = possibilities[i,1:Nsymbols]  %>% as.numeric()
    
    portfolio_return = Return.portfolio(asset_return,weights = w,rebalance_on = "months")
    
    possibilities[i ,"sharpeRatePortfolio"] = SharpeRatio(portfolio_return,Rf=rfr,FUN = "StdDev")
    possibilities[i ,"sd"] = sd(portfolio_return$portfolio.returns)
    possibilities[i,"mean"] = mean(portfolio_return$portfolio.returns)
    
    print(paste(i,Npossibilites,sep = " - "))
    
  }
  
  return(possibilities)
  
}
