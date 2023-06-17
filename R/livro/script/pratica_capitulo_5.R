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

prices = map(symbols,~getSymbols(Symbols = .x,from="2012-12-31", to='2017-12-31',auto.assign = F) %>% 
      as.data.frame(row.names = index(.))%>% 
      select(contains('ad'))) %>%
  reduce(cbind) %>%
  setNames(symbols)

prices_monthly = to.monthly(prices,indexAt = 'lastof',OHLC=F)

prices_return = prices_monthly %>%
  rownames_to_column('date') %>%
  pivot_longer(cols = 2:ncol(.), names_to = 'asset',values_to = 'return') %>%
  group_by(asset)%>%
  mutate(return_log = log(return) - log(lag(return))) %>%
  na.omit() %>%
  select(-return)


portifolio_return = prices_return %>%
  mutate(weight = case_when(
    asset == symbols[1] ~ w[1],
    asset == symbols[2] ~ w[2],
    asset == symbols[3] ~ w[3],
    asset == symbols[4] ~ w[4],
    asset == symbols[5] ~ w[5]
  ),
  return_w = return_log * weight,
  date=as.Date(date)) %>%
  group_by(date) %>%
  summarise(return = sum(return_w))


sd_plot = sd(portifolio_return$return)
mean_plot = mean(portifolio_return$return)


skew_tidy = skewness(portifolio_return$return)



### visualization

portifolio_plot = portifolio_return %>% 
  mutate(color_p = case_when(
  return < mean_plot - sd_plot ~'red',
  return > mean_plot + sd_plot ~'green',
  T ~ 'DarkBlue'
))
 
portifolio_plot %>%  
  ggplot(aes(x=date,y=return)) +
  geom_point(color=portifolio_plot$color_p) + 
  scale_x_date(breaks = pretty_breaks(6)) +
  geom_hline(yintercept = mean_plot - sd_plot, linetype='dotted',color='purple')+
  geom_hline(yintercept = mean_plot + sd_plot, linetype='dotted',color='purple') +
  labs(x='Data',y='Retorno',title='Serie historica dos retornos')

prices_return %>%
  group_by(asset) %>%
  summarise(expected_r = mean(return_log)) %>%
  add_row(asset='Portifolio',expected_r = mean(portifolio_return$return)) %>%
  ggplot(aes(x=asset,y=expected_r,color=asset)) +
  geom_point()

prices_return %>%
  group_by(asset) %>%
  summarise(expected_r = mean(return_log),sd = sd(return_log)) %>%
  add_row(asset='Portifolio',expected_r = mean_plot, sd = sd_plot) %>%
  ggplot(aes(x=sd,y=expected_r,color=asset)) +
  geom_point(size=3) +
  labs(y="Expectativa de Retorno", x = "Desvio Padrao (Risco)") +
  geom_text(aes(x=sd_plot,y=mean_plot+0.0005,label='Portifolio'))

skew_plot= portifolio_return %>%
  ggplot(aes(x=return)) + 
  #geom_histogram(binwidth = 0.003,fill='Darkblue',alpha = .3) +
  geom_density(color='Darkblue',alpha=.1)

shaded_area = ggplot_build(skew_plot)$data[[1]] %>% filter(x<mean_plot)
median_line = ggplot_build(skew_plot)$data[[1]] %>% filter(x < median(portifolio_return$return))

skew_plot_shaded = skew_plot +
  geom_area(data =  shaded_area,aes(x=x,y=y),fill='red',alpha=.1)

median_plot = median(portifolio_return$return)


skew_plot_shaded +
  geom_segment(data = shaded_area, aes(x=mean_plot,y=0,xend=mean_plot,yend=density)) +
  annotate(geom='text',x=mean_plot,y=5,label='Media',angle=90,vjust=-1)+
  geom_segment(data = median_line,aes(x=median_plot,y=0,xend=median_plot,yend=density))+
  annotate(geom='text',x=median_plot,y=5,label='Mediana',angle=90, vjust=-1)



prices_return %>%
  group_by(asset) %>%
  summarise(skew = skewness(return_log))  %>%
  add_row(asset='Portifolio',skew = skew_tidy) %>%
   ggplot(aes(x=asset,y=skew,color=asset)) +
  geom_point(size=3)
