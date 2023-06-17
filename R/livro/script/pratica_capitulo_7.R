library(quantmod)
library(tidyverse)
library(PerformanceAnalytics)
library(tidyquant)
library(scales)

options(scipen = 999)

### parametros universais

symbols <- c("SPY","EFA", "IJS", "EEM","AGG")

w <- c(0.25, 
       0.25, 
       0.20, 
       0.20, 
       0.10)

window = 24

rfr = 0.0003

### funcao customizada 

getmode = function(df,col){
  
  return(
    df %>% 
      group_by({{col}}) %>% 
      mutate({{col}}:= round({{col}},3)) %>% 
      summarise(t = n()) %>% 
      slice_max(order_by = t)
  )
}


### dados gerais

prices = map(symbols, ~getSymbols(Symbols = .x,from='2012-12-31',to='2017-12-31',auto.assign = F) %>%
      as.data.frame(row.names = index(.)) %>%
      select(contains('ad'))
) %>%
  reduce(cbind) %>%
  setNames(symbols)

prices_monthly = to.monthly(prices,indexAt = 'lastof',OHLC=F)    

return_long_tidy = prices_monthly %>% 
  rownames_to_column('date') %>%
  pivot_longer(cols = 2:ncol(.),names_to = 'asset',values_to = 'return') %>%
  group_by(asset) %>%
  mutate(return_log = log(return) - log(lag(return)),
         date = as.Date(date)) %>%
  na.omit() %>%
  select(-return) %>%
  arrange(asset)


portfolio_return_tidy = return_long_tidy %>%
  mutate(weight = case_when(
    asset == symbols[1] ~ w[1],
    asset == symbols[2] ~ w[2],
    asset == symbols[3] ~ w[3],
    asset == symbols[4] ~ w[4],
    asset == symbols[5] ~ w[5]
  ),
  return_w  = return_log * weight
  ) %>%
  group_by(date) %>%
  summarise(return = sum(return_w))


portfolio_return_tidy_plot = portfolio_return_tidy %>%
 mutate(cor = case_when(
   return < (mean_portfolio - sd_portfolio) ~'red',
   return > (mean_portfolio + sd_portfolio) ~'green',
   TRUE ~ 'Darkblue'
   )
   )


### parametros portfolio

sd_portfolio = sd(portfolio_return_tidy$return)
mean_portfolio = mean(portfolio_return_tidy$return)
mode_portfolio = getmode(portfolio_return_tidy,return)$return
median_portfolio = median(portfolio_return_tidy$return)
skew_tidy = skewness(portfolio_return_tidy$return)
curtose_tidy = kurtosis(portfolio_return_tidy$return)
sharpe_portfolio_tidy = portfolio_return_tidy %>%
  summarise(sharpe = mean(return - rfr)/sd(return-rfr)) %>% 
  .[[1]]

### rolling

rolling_sd_tidy = portfolio_return_tidy %>%
  tq_mutate(select = return,mutate_fun = rollapply,FUN = sd,width=window,col_rename = 'rolling_sd') %>%
  na.omit() %>%
  select(-return)

rolling_skew_tidy = portfolio_return_tidy %>%
  tq_mutate(select = return,mutate_fun = rollapply,FUN = skewness,width=window,col_rename = 'rolling_skew') %>%
  na.omit() %>%
  select(-return)

rolling_kurt_tidy = portfolio_return_tidy %>%
  tq_mutate(select = return,mutate_fun = rollapply,FUN = kurtosis,width=window,col_rename = 'rolling_kurt') %>%
  na.omit() %>%
  select(-return)

rolling_sharpe_tidy = portfolio_return_tidy %>%
  tq_mutate(select = return,mutate_fun = rollapply,FUN=function(x){mean(x-rfr)/sd(x-rfr)},width=window,col_rename = 'rolling_sharpe') %>%
  na.omit() %>%
  select(-return)

### graficos

return_long_tidy %>%
  group_by(asset) %>%
  summarise(dp = sd(return_log),er = mean(return_log)) %>%
  add_row(asset='Portfolio',dp = sd_portfolio,er=mean_portfolio) %>%
  ggplot(aes(x= dp,y=er,color=asset)) +
  geom_point(size=3) +
  geom_text(aes(x=sd_portfolio,y=mean_portfolio+.0005,label='Portfolio')) +
  labs(x='Risco',y='Retorno Esperado',title='Retorno esperado x Risco')

return_long_tidy %>%
  group_by(asset) %>%
  summarise(dp = sd(return_log),sharpe = mean(return_log-rfr)/sd(return_log - rfr)) %>%
  add_row(asset='Portfolio',dp = sd_portfolio,sharpe=sharpe_portfolio_tidy) %>%
  ggplot(aes(x= dp,y=sharpe,color=asset)) +
  geom_point(size=3) +
  geom_text(aes(x=sd_portfolio,y=sharpe_portfolio_tidy+.02,label='Portfolio')) +
  labs(x='Risco',y='Indice de sharpe',title='Indice de sharpe x Risco')

portfolio_return_tidy_plot %>%
  ggplot(aes(x=date,y=return)) +
  geom_point(color=portfolio_return_tidy_plot$cor) +
  scale_x_date(breaks = pretty_breaks(6)) +
  geom_hline(yintercept = (mean_portfolio- sd_portfolio),color='purple',linetype='dashed')+
  geom_text(aes(x=date[1],y=(mean_portfolio- sd_portfolio)+0.005,label='1 Desvio abaixo'))+
  geom_hline(yintercept = (mean_portfolio+ sd_portfolio),color='purple',linetype='dashed')+
  geom_text(aes(x=date[1],y=(mean_portfolio+ sd_portfolio)+0.005,label='1 Desvio acima'))


portfolio_return_tidy_plot %>%
  ggplot(aes(x=return))+
  geom_histogram(binwidth = 0.005,alpha=.45,fill='Darkblue')


p0 = portfolio_return_tidy_plot %>%
  ggplot(aes(x=return))+
  geom_density() 

p1 = ggplot_build(p0)$data[[1]] %>%
  filter(x < (mean_portfolio - 2*sd_portfolio))

p2 = ggplot_build(p0)$data[[1]] %>%
  filter(x> (mean_portfolio + 2*sd_portfolio))


p0 +
  geom_area(data = p1,aes(x=x,y=y),alpha=.45,fill='pink') +
  geom_area(data = p2,aes(x=x,y=y),alpha=.45,fill='pink')
         
         
rolling_skew_tidy %>%
  ggplot(aes(x=date,y=rolling_skew)) +
  geom_line()

rolling_kurt_tidy %>%
  ggplot(aes(x=date,y=rolling_kurt)) +
  geom_line()

rolling_sharpe_tidy %>%
  ggplot(aes(x=date,y=rolling_sharpe)) +
  geom_line()
