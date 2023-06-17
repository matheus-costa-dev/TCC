library(tidyverse)
library(quantmod)
library(scales)
library(PerformanceAnalytics)


## configuração 1 - top 10 maiores capitalização de mercado

## todas as moedas q possuem dados desde 2020-01-01 e estão no top 10 retirada

options(scipen=999)
symbols = c("BTC-USD","ETH-USD","USDC-USD", "USDT-USD", "BNB-USD", "XRP-USD", "ADA-USD","DOGE-USD","SOL-USD","MATIC-USD")
prices = getSymbols(symbols,
                    src = 'yahoo',
                    from="2020-01-01",
                    to="2023-01-01",
                    auto.assign = TRUE) %>%
  map(~Ad(get(.))) %>%
  reduce(merge) %>% 
  setNames(symbols)


asset_return = Return.calculate(prices,method = "log")  %>% na.omit()
asset_return_long = asset_return %>%  as.data.frame(row.names = index(.)) %>% rownames_to_column("data") %>%
  pivot_longer(cols =  all_of(symbols),names_to = "asset",values_to = "returns")  %>%
  arrange(asset)

covariance = cov(asset_return)
correlation = cor(asset_return)

rfr = 0.1375 # taxa selic
by = .2

possibilities = rep(list(seq(0,1,by)),length(symbols)) %>%
  reduce(cbind) %>% as_tibble() %>%
  setNames(symbols)

possibilities = do.call(expand.grid, possibilities[1:length(symbols)])
rows = which(rowSums(possibilities) == 1)
possibilities = possibilities[rows,]

for(i in 1:nrow(possibilities)) {
  
  
  w = possibilities[i,1:length(symbols)]  %>% as.numeric()
  
  portfolio_return = Return.portfolio(asset_return,weights = w,rebalance_on = "months")
  
  possibilities[i ,"sharpeRatePortfolio"] = SharpeRatio(portfolio_return,rf=rfr,FUN = "StdDev")
  possibilities[i ,"sd"] = sd(portfolio_return$portfolio.returns)
  possibilities[i,"mean"] = mean(portfolio_return$portfolio.returns)
  
  print(paste(i,nrow(possibilities),sep = " - "))
  
}

min_var <- possibilities[which.min(possibilities$sd),]
max_sr <- possibilities[which.max(possibilities$sharpeRatePortfolio),]
max_re <- possibilities[which.max(possibilities$mean),]


### tabelas
### top 5  combinações com base no indicio sharpe
possibilities %>% 
  arrange(sharpeRatePortfolio) %>% 
  head() %>% 
  gt() %>% 
  tab_header(title = "top 5 piores combinações com base no indicio sharpe") %>%
  fmt_percent(columns = symbols,decimals = 0, dec_mark = ",", sep_mark = ".") %>%
  fmt_percent(columns = c(sharpeRatePortfolio,sd),decimals = 3, dec_mark = ",", sep_mark = "." ) %>%
  cols_label(sharpeRatePortfolio = "Indice Sharpe", sd= "Risco", mean = "Retorno esperado")


possibilities %>% 
  arrange(desc(sharpeRatePortfolio)) %>% 
  head() %>% 
  gt() %>% 
  tab_header(title = "top 5 melhores combinações com base no indicio sharpe") %>%
  fmt_percent(columns = symbols,decimals = 0, dec_mark = ",", sep_mark = ".") %>%
  fmt_percent(columns = c(sharpeRatePortfolio,sd),decimals = 3, dec_mark = ",", sep_mark = "." ) %>%
  cols_label(sharpeRatePortfolio = "Indice Sharpe", sd= "Risco", mean = "Retorno esperado")


### top 5  combinações com base no risco

possibilities %>% 
  arrange(sd) %>% 
  head() %>% 
  gt() %>% 
  tab_header(title = "top 5 combinações com base no menor risco") %>%
  fmt_percent(columns = symbols,decimals = 0, dec_mark = ",", sep_mark = ".") %>%
  fmt_percent(columns = c(sharpeRatePortfolio,sd),decimals = 3, dec_mark = ",", sep_mark = "." ) %>%
  cols_label(sharpeRatePortfolio = "Indice Sharpe", sd= "Risco", mean = "Retorno esperado")


possibilities %>% 
  arrange(desc(sd)) %>% 
  head() %>% 
  gt() %>% 
  tab_header(title = "top 5 combinações com base no maior risco") %>%
  fmt_percent(columns = symbols,decimals = 0, dec_mark = ",", sep_mark = ".") %>%
  fmt_percent(columns = c(sharpeRatePortfolio,sd),decimals = 3, dec_mark = ",", sep_mark = "." ) %>%
  cols_label(sharpeRatePortfolio = "Indice Sharpe", sd= "Risco", mean = "Retorno esperado")


### graficos 


possibilities %>%
  ggplot(aes(x = sd, y = mean, color = sharpeRatePortfolio)) +
  geom_point() +
  theme_classic() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) +
  labs(x = 'Risco',
       y = 'Retorno esperado',
       title = "Optimização de portfolio & Fronteira eficiente",
       colour = "Indice sharpe") +
  geom_point(aes(x = sd,
                 y = mean), data = min_var, color = 'red') +
  geom_point(aes(x = sd,
                 y = mean), data = max_sr, color = 'red') +
  geom_text(x=min_var$sd, y = min_var$mean + .0002,label="Menor risco", color = "red") +
  geom_text(x=max_sr$sd, y = max_sr$mean + .0002,label="Maior indice sharpe", color = "red", check_overlap = T) +
  theme_bw() +
  theme( plot.title = element_text(hjust = .5))

asset_return_long %>%
  group_by(asset) %>%
  summarise(sd= sd(returns), mean= mean(returns)) %>%
  ggplot(aes(x=sd,y=mean,color=asset)) +
  geom_point(size=3, show.legend = FALSE) + 
  #scale_y_continuous() +
  scale_x_continuous(labels = scales::percent) +
  labs(y="Retorno esperado",
       x="Risco",
       colour="Ativo",
       title = "Risco x Retorno") +
  geom_text(aes(label = asset, y = mean + .0002)) +
  theme_bw() +
  theme(legend.position="none", plot.title = element_text(hjust = .5))

## exporting data

# openxlsx::write.xlsx(possibilities,"outputData/dataPossibilities.xlsx")
# openxlsx::write.xlsx(asset_return_long,"outputData/dataAssetReturn.xlsx")
