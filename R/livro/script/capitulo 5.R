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


prices = map(symbols,~getSymbols(Symbols = .x,auto.assign = F,from="2012-12-31", to ="2017-12-31") %>%
               as.data.frame(row.names = index(.))%>%
               select(contains('ad'))) %>%
  reduce(cbind) %>%
  setNames(symbols)


prices_monthly = to.monthly(prices,indexAt = 'lastof',OHLC=F)

return_long = prices_monthly %>%
  rownames_to_column('date') %>%
  pivot_longer(cols = 2:ncol(.),names_to = 'asset',values_to = 'prices') %>%
  group_by(asset) %>%
  mutate(return_log = (log(prices) - log(lag(prices)))) %>%
  select(-prices) %>%
  na.omit()

return_wide = return_long %>% pivot_wider(names_from = 'asset',values_from = 'return_log')

portfolio_return = return_long %>%
  mutate(weights = case_when(
    asset == symbols[1] ~ w[1],
    asset == symbols[2] ~ w[2],
    asset == symbols[3] ~ w[3],
    asset == symbols[4] ~ w[4],
    asset == symbols[5] ~ w[5]
  ),
  return_w = return_log * weights,
  date = as.Date(date)) %>%
  group_by(date) %>%
  summarise(return = sum(return_w))

rolling_portifolio = portfolio_return %>%
  tq_transmute(mutate_fun = rollapply,FUN=sd,width=window) %>%
  na.omit()

rolling_portifolio %>% tail(3)

skew = skewness(portfolio_return$return) %>% round(4)

sd_plot = sd(portfolio_return$return)
mean_plot = mean(portfolio_return$return)



### skewness in xts world
xts_long_return  = Return.calculate(prices_monthly,method = 'log') %>% na.omit()
xts_portifolio_return  =Return.portfolio(xts_long_return, weights = w,rebalance_on = 'months')

skew_xts = skewness(xts_portifolio_return$portfolio.returns)


### skewness in tidyverse

tidy_skew = portfolio_return %>% 
  summarise(skew = skewness(return))

ggplot(portfolio_return,aes(x=return)) + 
  geom_histogram(binwidth = .003, 
                 alpha=.7,
                 fill = "cornflowerblue",
                 color = "cornflowerblue") +
  scale_x_continuous(breaks = pretty_breaks(n=10) ) +
  geom_vline(xintercept = mean_plot,color='purple') +
  geom_vline(xintercept = (mean_plot - sd_plot),color='red') +
  geom_vline(xintercept = (mean_plot + sd_plot),color='green') 

portifolio_density_plot = ggplot(portfolio_return,aes(x=return)) + geom_density(color='cornflowerblue')

shaded_area = ggplot_build(portifolio_density_plot)$data[[1]] %>% filter(x < mean_plot)

portifolio_density_plot_shaded = portifolio_density_plot +
  geom_area(data = shaded_area,aes(x=x,y=y),fill='pink',alpha=.5)

median_plot = median(portfolio_return$return)

median_line_plot = ggplot_build(portifolio_density_plot)$data[[1]] %>% 
  filter(x<median_plot)

portifolio_density_plot_shaded +
geom_segment(data=shaded_area,aes(x=mean_plot,y=0,xend=mean_plot,yend=density),linetype='dotted',color='red')+
  annotate(geom = 'text',x=mean_plot,y=5,label='media',angle=90,alpha=.8,vjust=-1.75) +
  geom_segment(data=median_line_plot,aes(x=median_plot,y=0,xend=median_plot,yend=density),linetype='dotted',color='black')+
  annotate(geom='text',x=median_plot,y=5,angle=90,vjust=1.5,alpha=.8,label='mediana') +
  ggtitle("Density Plot Illustrating Skewness")


return_long %>%
  group_by(asset) %>%
  summarise(skew = skewness(return_log)) %>%
  add_row(asset='Portifolio',skew = tidy_skew$skew)  %>%
  ggplot(aes(x=asset,y=skew,color=asset)) +
  geom_point(size=3)



### rolling skewness xts
rolling_skew_xts =  rollapply(data = xts_portifolio_return,FUN = skewness,width = window) %>% na.omit()

### in tidyverse

rolling_skew_tidy = portfolio_return %>% 
  tq_mutate(select = return,mutate_fun = rollapply,FUN = skewness,width = window,col_rename = 'skew_tidy') %>%
   na.omit() %>% 
  select(date,skew_tidy)

rolling_skew_tidy %>% mutate(xts = coredata(rolling_skew_xts$portfolio.returns))

rolling_skew_tidy %>% 
  ggplot(aes(x=date,y=skew_tidy)) +
  geom_line() + 
  ylim(min = -1,max=1)



### kurtosis

kurt_xts =  kurtosis(xts_portifolio_return$portfolio.returns)
kurt_tidy = portfolio_return %>% summarise(kurt = kurtosis(return))


sd_pos  = mean_plot + (2*sd_plot)
sd_neg  = mean_plot - (2*sd_plot)

sd_pos_shaded = ggplot_build(portifolio_density_plot)$data[[1]] %>% filter(x > sd_pos)
sd_neg_shaded = ggplot_build(portifolio_density_plot)$data[[1]] %>% filter(x < sd_neg)


portifolio_density_plot +
  geom_area(data=sd_pos_shaded, aes(x=x,y=y),fill='red',alpha=.1)+
  geom_area(data=sd_neg_shaded, aes(x=x,y=y),fill='red',alpha=.1) + 
  geom_segment(data = shaded_area,aes(x=mean_plot,y=0,xend=mean_plot,yend=density),color='red',linetype='dotted') +
  annotate(geom = 'text',x=mean_plot,y=5,label='media',angle=90,vjust=-1)+
  geom_segment(data = median_line_plot,aes(x=median_plot,y=0,xend=median_plot,yend=density),color='purple',linetype='dotted')+
  annotate(geom = 'text',x=median_plot,y=5,label='Mediana',vjust=-1,angle=90)+
  scale_x_continuous(breaks = pretty_breaks(10)) 


## comparing kurtosis

return_long %>% group_by(asset) %>%
  summarise(kurt = kurtosis(return_log)) %>%
  add_row(asset='Portifolio',kurt = kurt_tidy$kurt) %>%
  ggplot(aes(x=asset,
             y=kurt,
             color=asset))+
  geom_point(size=3) +
  labs(x='Ativo',y='Kurtose') +
  annotate(geom = 'text',label='Portifolio',x='Portifolio',y=kurt_tidy$kurt +.1,color='red')


### rolling kurtose

rolling_kurt_xts = rollapply(xts_portifolio_return$portfolio.returns,FUN=kurtosis,width=window) %>% na.omit()
rolling_kurt_tidy = portfolio_return %>%
  tq_mutate(select = return,mutate_fun = rollapply,FUN=kurtosis,width=window,col_rename = 'roll_kurt') %>%
  select(date,roll_kurt) %>%
  na.omit()

rolling_kurt_xts %>% tail(3)
rolling_kurt_tidy %>% tail(3)

rolling_kurt_tidy %>%
  ggplot(aes(x=date,y=roll_kurt))+ 
  geom_line() + 
  scale_y_continuous(breaks=pretty_breaks(8))+
  scale_x_date(breaks=pretty_breaks(8)) +
  labs(x="Data",y='Kurtose',title = 'Rolamento kurtose 24 meses') +
  theme(plot.title = element_text(hjust=.5))
