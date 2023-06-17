library(quantmod)
library(tidyverse)
library(PerformanceAnalytics)
library(tidyquant)
library(scales)

options(scipen = 999)

symbols <- c("SPY","EFA", "IJS", "EEM","AGG")

w <- c(0.25, 
       0.25, 
       0.20, 
       0.20, 
       0.10)

window = 24



getmode = function(df,col){
  
  return(
    df %>% 
      group_by({{col}}) %>% 
      mutate({{col}}:= round({{col}},3)) %>% 
      summarise(t = n()) %>% 
      slice_max(order_by = t)
  )
}


prices = map(symbols,function(x){
  print(x)
  getSymbols(Symbols = x,from='2012-12-31',to='2017-12-31',auto.assign = F) %>%
    as.data.frame(row.names = index(.)) %>%
    select(contains('ad'))
}) %>%
  reduce(cbind) %>%
  setNames(symbols)


prices_monthly = to.monthly(prices,indexAt = 'lastof', OHLC=F)

return_long = prices_monthly %>%
  rownames_to_column('date') %>%
  pivot_longer(cols = 2:ncol(.),names_to = 'asset',values_to = 'return') %>%
  group_by(asset) %>%
  mutate(return_log = log(return) - log(lag(return))) %>%
  na.omit() %>%
  select(-return)

portifolio_return = return_long %>%
  mutate(weight = case_when(
    asset == symbols[1] ~ w[1],
    asset == symbols[2] ~ w[2],
    asset == symbols[3] ~ w[3],
    asset == symbols[4] ~ w[4],
    asset == symbols[5] ~ w[5]
  ),
  return_w = return_log * weight,
  date = as.Date(date)) %>%
  group_by(date) %>%
  summarise(return = sum(return_w))
 

mean_portifolio = mean(portifolio_return$return)
sd_portifolio = sd(portifolio_return$return)
median_portifolio = median(portifolio_return$return)
mode_portifolio = getmode(portifolio_return,return)[['return']]

portifolio_return_plot = portifolio_return %>%
  mutate(cor= case_when(
    return < mean_portifolio - sd_portifolio ~ 'red',
    return > mean_portifolio + sd_portifolio ~ 'green',
    T ~ 'Darkblue'
  ))
  

### sharpe ratio

rfr = 0.0003

rfr * 100


xts_sharpe = SharpeRatio(R=portifolio_return %>% column_to_rownames('date') %>% as.xts(),Rf = rfr, FUN = 'StdDev')

byhand_sharpe = portifolio_return %>%
  summarise(sharpe = mean(return - rfr)/sd(return - rfr))


tidy_sharpe = portifolio_return %>%
  tq_performance(Ra = return,performance_fun = SharpeRatio, Rf= rfr, FUN = 'StdDev')


### comparando com um ativo isolado o sharpe_ratio



getSymbols(Symbols = 'SPY',from='2012-12-31',to='2017-12-31',auto.assign = F) %>%
  as.data.frame(row.names = index(.)) %>%
  select(contains('ad')) %>%
  setNames('return') %>%
  to.monthly(indexAt = 'lastof',OHLC=F) %>%
  rownames_to_column('date') %>%
  mutate(return_log = log(return) - log(lag(return))) %>%
  select(-return) %>%
  na.omit() %>%
  summarise(sharpe_ratio = mean(return_log-rfr)/sd(return_log-rfr))



sharp_tidy = portifolio_return %>%
  mutate(sd = sd(return),
         sharpe_ratio = mean(return - rfr)/sd(return - rfr),
         below_sharpe = ifelse(return<rfr,return,NA),
         above_sharpe = ifelse(return>rfr,return,NA)
         )

ggplot(sharp_tidy,aes(x=date)) +
  geom_point(aes(y=below_sharpe),color='red')+
  geom_point(aes(y=above_sharpe),color='green') +
  scale_x_date(breaks = pretty_breaks(6))+
  labs(y='Retorno',x='Data')

sharp_tidy %>%
  ggplot(aes(x=return)) + 
  geom_histogram(binwidth = .01,alpha=.45,fill='cornflowerblue') +
  geom_vline(xintercept = rfr,linetype='dashed',color='green')+
  geom_text(aes(x=rfr,y=10),label='Rate Risk Free',angle=90,vjust=-1)


return_long %>%
  group_by(asset) %>%
  summarise(ratio = mean(return_log - rfr)/sd(return_log-rfr),
            dp = sd(return_log) ) %>%
  add_row(asset = 'Portfolio',ratio = sharp_tidy$sharpe_ratio[1], dp = sd(sharp_tidy$return))  %>%
  ggplot(aes(x=dp,y=ratio,color=asset)) +
  geom_point(size=3) +
  geom_text(aes(x=sd(sharp_tidy$return),y=sharp_tidy$sharpe_ratio[1]+.02,label='Portfolio')) +
  labs(x='Desvio Padrao (Risco)',y='Razão de Sharpe')


### rolling in sharpe ratio


return_long_xts = Return.calculate(prices = prices_monthly,method = 'log') %>% na.omit()
portifolio_return_xts = Return.portfolio(return_long_xts,weights = w,rebalance_on = 'months')

rolling_sharpe_xts = rollapply(portifolio_return_xts,
          width=window,
          FUN = function(df){
            SharpeRatio(df,Rf=rfr,FUN = 'StdDev')
          }) %>%
  na.omit()



rolling_sharpe_tidy =  portifolio_return %>% 
  tq_mutate(select = return,
            mutate_fun = rollapply,
            width = window,
            align = "right",
            FUN = function(df){SharpeRatio(R=df,Rf=rfr,FUN='StdDev')},
            col_rename = "tq_sharpe"
  ) %>% 
  na.omit() %>%
  select(-return)

rolling_sharpe_tidy %>% ggplot(aes(x=date,y=tq_sharpe)) + geom_line()
