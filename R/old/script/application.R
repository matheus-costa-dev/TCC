library(tidyverse)
library(quantmod)
library(scales)
library(PerformanceAnalytics)

options(scipen=999)
symbols = c("BTC-USD","ETH-USD","ADA-USD")
prices = getSymbols(symbols,
           src = 'yahoo',
           from="2018-01-01",
           to="2023-01-01",
           auto.assign = TRUE) %>%
  map(~Ad(get(.))) %>%
  reduce(merge) %>% 
  setNames(symbols)

prices_monthly = to.monthly(prices,indexAt = "lastof",OHLC=F)
asset_return = Return.calculate(prices_monthly,method = "log")  %>% na.omit()


asset_return_long = asset_return %>% 
  as.data.frame(row.names = index(.))  %>% 
  pivot_longer(cols = c(1:length(symbols)),names_to = "asset",values_to = "returns") %>%
  arrange(asset)

asset_return_long = asset_return %>%  as.data.frame(row.names = index(.)) %>% rownames_to_column("data") %>%
  pivot_longer(cols =  all_of(symbols),names_to = "asset",values_to = "returns")  %>%
  arrange(asset)


### end

w = c(rep(1/length(symbols),length(symbols)))
portfolio_return = Return.portfolio(asset_return,weights = w,rebalance_on = "months")

sd_plot = sd(portfolio_return$portfolio.returns)
mean_plot = mean(portfolio_return$portfolio.returns)
median_plot = median(portfolio_return$portfolio.returns)


skewness(portfolio_return$portfolio.returns)
kurtosis(portfolio_return$portfolio.returns)


by = .05

possibilities = rep(list(seq(0,1,by)),length(symbols)) %>% 
  reduce(cbind) %>% as_tibble() %>%
  setNames(paste0("w",1:length(symbols)))

possibilities = expand_grid(possibilities$w1,possibilities$w2,possibilities$w3) %>%
  setNames(symbols %>% str_remove_all("-USD")) %>%
  mutate(s = BTC + ETH + ADA)  %>%
  filter(s == 1) %>%
  select(-s)



 
# possibilities = data.frame(w1=seq(0,1,by),
#                            w2=seq(0,1,by))
# possibilities = expand_grid(possibilities$w1,possibilities$w2) %>% 
#   setNames(c("BTC","ETH"))  %>% 
#   mutate(s = BTC + ETH) %>%
#   filter(s == 1) %>% 
#   select(BTC,ETH)



rfr = 0.1375 # taxa selic

SharpeRatio(portfolio_return,rf=rfr,FUN = "StdDev")


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


### grafico 

asset_return_long %>%
  ggplot(aes(x=returns,fill=asset)) +
  geom_histogram(alpha=.3,binwidth =.07)


asset_return_long %>%
  group_by(asset) %>%
  summarise(sd= sd(returns)) %>%
  add_row(asset="Portfolio",sd=sd_plot)  %>%
  ggplot(aes(x=asset,y=sd,color=asset)) +
  geom_point() + 
  scale_y_continuous(labels = scales::percent) +
  labs(x="Ativos",y= "Desvio-Padrão (Risco)",title = "Risco por ativo")


possibilities %>%
  ggplot(aes(x=mean,y=sd)) +
  geom_point() +
  labs(x="Retorno esperado",y="Risco")



portfolio_return_plot = portfolio_return  %>%
  as.data.frame() %>%
  rownames_to_column("date") %>%
  rename(returns = "portfolio.returns")%>%
  mutate(date=as.Date(date),
         color = 
           case_when(
             returns < mean_plot - sd_plot ~"red",
             returns > mean_plot + sd_plot ~"green",
             T ~"blue" )
  )

portfolio_return_plot %>%
  ggplot(aes(x=date,y=returns)) + 
  geom_point(color=portfolio_return_plot$color) + 
  scale_x_date(breaks = pretty_breaks(n = 6)) + labs(x="Date",y="Portfolio Returns",title = "Scatterplot of Returns by Date") +
  geom_hline(yintercept = (mean_plot + sd_plot),
             color = "purple",
             linetype = "dotted") +
  geom_hline(yintercept = (mean_plot-sd_plot),
             color = "purple",
             linetype = "dotted")
  # geom_text(aes(x=as.Date("2019-01-01"),y=(mean_plot - sd_plot)),label="1 Desvio padrão abaixo da média") +
  # geom_text(aes(x=as.Date("2019-01-01"),y=(mean_plot + sd_plot)),label="1 Desvio padrão acima da média")



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


###



