library(tidyverse)
library(quantmod)
library(scales)
library(PerformanceAnalytics)

### citation to bibText


map(c("base","stats","tidyverse","quantmod","scales","PerformanceAnalytics"), 

   ~ write_lines(toBibtex(citation(.x)),paste0("citacao/",.x,".bib"))
    )


###



options(scipen=999)
symbols = c("BTC-USD","ETH-USD","BNB-USD", "PAXG-USD")
prices = getSymbols(symbols,
                    src = 'yahoo',
                    from="2018-01-01",
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

w = c(rep(1/length(symbols),length(symbols)))
portfolio_return = Return.portfolio(asset_return,weights = w,rebalance_on = "months")

sd_plot = sd(portfolio_return$portfolio.returns)
mean_plot = mean(portfolio_return$portfolio.returns)
median_plot = median(portfolio_return$portfolio.returns)


skewness(portfolio_return$portfolio.returns)
kurtosis(portfolio_return$portfolio.returns)


rfr = 0.1375 # taxa selic
by = .05

possibilities = rep(list(seq(0,1,by)),length(symbols)) %>% 
  reduce(cbind) %>% as_tibble() %>%
  setNames(symbols)

possibilities = do.call(expand.grid, possibilities[1:length(symbols)])
rows = which(rowSums(possibilities) == 1)
possibilities = possibilities[rows,]


for(i in 1:nrow(possibilities)) {
  
  w = possibilities[i,1:length(symbols)]  %>% as.numeric()
  
  portfolio_return = Return.portfolio(asset_return,weights = w,rebalance_on = "months")
  
  possibilities$sharpeRatePortfolio[i] = SharpeRatio(portfolio_return,rf=rfr,FUN = "StdDev")
  possibilities$sd[i] = sd(portfolio_return$portfolio.returns)
  possibilities$mean[i] = mean(portfolio_return$portfolio.returns)
  
  print(w)
  
}

min_var <- possibilities[which.min(possibilities$sd),]
max_sr <- possibilities[which.max(possibilities$sharpeRatePortfolio),]
max_re <- possibilities[which.max(possibilities$mean),]

asset_return_long %>%
  group_by(asset) %>%
  summarise(sd= sd(returns), mean= mean(returns)) %>%
  add_row(asset="Portfolio",sd=sd_plot,mean=mean_plot)  %>%
  ggplot(aes(x=sd,y=mean,color=asset)) +
  geom_point(size=3) + 
  scale_y_continuous() +
  labs(y="Retorno esperado",x="Risco")


possibilities %>%
  ggplot(aes(x = sd, y = mean, color = sharpeRatePortfolio)) +
  geom_point() +
  theme_classic() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) +
  labs(x = 'Risco',
       y = 'Retorno esperado',
       title = "Optimização de portfolio & Fronteira eficiente") +
  geom_point(aes(x = sd,
                 y = mean), data = min_var, color = 'red') +
  geom_point(aes(x = sd,
                 y = mean), data = max_sr, color = 'red')
