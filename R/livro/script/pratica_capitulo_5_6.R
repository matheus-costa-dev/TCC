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
  summarise(return = sum(return_w)) %>%
  mutate(cor= case_when(
    return < mean_portifolio - sd_portifolio ~ 'red',
    return > mean_portifolio + sd_portifolio ~ 'green',
    T ~ 'Darkblue'
  ))

  

sd_portifolio = sd(portifolio_return$return)
mean_portifolio = mean(portifolio_return$return)
median_portifolio = median(portifolio_return$return)
moda_portifolio = getmode(portifolio_return,return)[[1]]




rolling_sd = portifolio_return %>%
  tq_mutate(select = return,mutate_fun = rollapply,FUN = sd,col_rename = 'rolling_sd', width = window) %>%
  select(-return) %>%
  na.omit()


skew_portifolio = portifolio_return %>% summarise(skew = skewness(return))
rolling_skew = portifolio_return %>% 
  tq_mutate(select = return,mutate_fun = rollapply,FUN=skewness,width=window,col_rename = 'rolling_skew') %>%
  select(date,rolling_skew) %>%
  na.omit()

portifolio_return %>% summarise(menor_0 = sum(return < moda_portifolio),
                                maior_0 = sum(return > moda_portifolio))


portifolio_return %>% filter(return < 0) %>% nrow()
portifolio_return %>% filter(return > 0) %>% nrow()

kurtose_portifolio =  portifolio_return %>% summarise(kurt = kurtosis(return))
rolling_kurt = portifolio_return %>% 
  tq_mutate(select = return,mutate_fun = rollapply,FUN=kurtosis,width=window,col_rename = 'rolling_kurt') %>%
  select(date,rolling_kurt) %>%
  na.omit()


### graficos
return_long %>% ggplot(aes(x=return_log)) + geom_histogram(binwidth = 0.005)
portifolio_return %>% ggplot(aes(x=return)) + geom_histogram(binwidth = 0.003,fill='Darkblue',alpha=.2)
p_density = portifolio_return %>% ggplot(aes(x=return)) + geom_density()


p2 = ggplot_build(p_density)$data[[1]] %>% filter(x<mean_portifolio)
p3 = ggplot_build(p_density)$data[[1]] %>% filter(x < median_portifolio)

p1 = p_density +
  geom_area(data =p2,aes(x=x,y=y), fill = 'red',alpha=.2) 

p1 +
  geom_segment(data=p2,aes(x=mean_portifolio,y=0,xend=mean_portifolio,yend=density),color='red',linetype='dashed') +
  geom_segment(data=p3,aes(x=median_portifolio,y=0,xend=median_portifolio,yend=density),color='Darkblue',linetype='dashed') +
  geom_text(aes(x=mean_portifolio,y=5,label='Media'),angle=90,vjust=-1) +
  geom_text(aes(x=median_portifolio,y=5,label='Mediana'),angle=90,vjust=-1)

rolling_skew %>%
  ggplot(aes(x=date,y=rolling_skew)) +
  geom_line() +
  lims(y=c(1,-1))
  

p_neg_kurt = ggplot_build(p_density)$data[[1]] %>% filter(x< mean_portifolio - (2 * sd_portifolio))
p_pos_kurt = ggplot_build(p_density)$data[[1]] %>% filter(x>mean_portifolio + (2 * sd_portifolio))
p_density +
  geom_area(data=p_neg_kurt,aes(x=x,y=y),fill='red',alpha=.2) +
  geom_area(data=p_pos_kurt,aes(x=x,y=y),fill='red',alpha=.2)
  
  
portifolio_return %>% 
  ggplot(aes(x=date,y=return)) + 
  geom_point(size=2,color=portifolio_return$cor) + 
  geom_hline(yintercept = mean_portifolio - sd_portifolio,linetype='dotted',color='purple')+
  geom_hline(yintercept = mean_portifolio + sd_portifolio,linetype='dotted',color='purple')



return_long %>%
  group_by(asset) %>%
  summarise(sd = sd(return_log),mean = mean(return_log)) %>%
  add_row(asset = 'Portifolio',sd=sd_portifolio, mean = mean_portifolio) %>%
  ggplot(aes(x=sd,y=mean,color=asset)) +
  geom_point(size=3) +
  geom_text(aes(x=sd_portifolio,y=mean_portifolio + .0005,label='Portifolio')) + 
  labs(x='Desvio Padrao (Risco)',y = 'Retorno esperado')

rolling_sd %>%
  ggplot(aes(x=date,y=rolling_sd)) +
  geom_line() +
  geom_point()
  




