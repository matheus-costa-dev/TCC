# pacotes ----

library(tidyverse)
library(quantmod)
library(scales)
library(PerformanceAnalytics)
library(openxlsx)
library(corrplot)
library(RColorBrewer)
library(knitr)
source("functions/allFunctions.R")
source("source/variaveis.R")

# alterando variavies ----

rfr = selic$SELICMENSALMedia
filePossibilites = paste0("PossibilitiesMonthly",rfr,".xlsx")
fullPathPossibilites = paste(folder,filePossibilites,sep = "/")
# codigo ----

prices = getSymbols(symbols,
                    src = "yahoo",
                    from=from,
                    to=to,
                    auto.assign = TRUE) %>%
  map(~Ad(get(.))) %>%
  reduce(merge) %>%
  setNames(symbols)

prices_monthly <- to.monthly(prices,
                             indexAt = "lastof",
                             OHLC = FALSE)


asset_return = Return.calculate(prices_monthly,method = "log")  %>% na.omit()

covariance_cryptocurrency = cov(asset_return)
correlation_cryptocurrency = cor(asset_return)

asset_return_long = returnToLong(asset_return,symbols)

summary_asset_long = asset_return_long %>% 
  group_by(asset) %>% 
  summarise(sd=sd(returns),mean=mean(returns))

less_riskier = slice_min(summary_asset_long,sd)
more_proftible = slice_max(summary_asset_long,mean)

## portfolio igual  ----

w = 1/Nsymbols %>% rep(Nsymbols)

portfolio_return = Return.portfolio(asset_return,weights = w,rebalance_on = "months")
sd_portfolio = sd(portfolio_return$portfolio.returns)
mean_portfolio = mean(portfolio_return$portfolio.returns)
sr_portfolio = SharpeRatio(portfolio_return,Rf=rfr,FUN = "StdDev")

## portfolio optimizado ----

possibilities = loadPossibilites(filePossibilites,fullPathPossibilites,symbols,by)

min_var <- slice_min(possibilities,sd)
max_sr <- slice_max(possibilities,sharpeRatePortfolio)
max_re <- slice_max(possibilities,mean)


bestCombination = slice_max(possibilities,sharpeRatePortfolio)
summaryBestCombination = bestCombination[which(bestCombination[1:Nsymbols] > 0)]



w = max_sr[1:Nsymbols] %>% unlist()

portfolio_return_opmitized = Return.portfolio(asset_return,
                                              weights = w,
                                              rebalance_on = "months")

sd_portfolio_optimized = sd(portfolio_return_opmitized$portfolio.returns)
mean_portfolio_optimized = mean(portfolio_return_opmitized$portfolio.returns)
sr_portfolio_optimized = max_sr$sharpeRatePortfolio


# graficos e tabelas ----


## risco x retorno ----

asset_return_long %>%
  group_by(asset) %>%
  summarise(sd= sd(returns), mean= mean(returns)) %>%
  ggplot(aes(x=sd,y=mean,color=asset)) +
  geom_point(size=2, show.legend = FALSE) + 
  # scale_x_continuous(labels = scales::percent) +
  geom_text(aes(label = asset, y = mean + .005),size=2) +
  labs(y="Retorno esperado",
       x="Risco",
       colour="Ativo") +
  theme_bw() +
  theme(legend.position="none")




pRiskReturnComparative_cryptocurrency = data.frame(asset = c("Normal","Otimizado"),
                                                   sd = c(sd_portfolio,sd_portfolio_optimized),
                                                   mean = c(mean_portfolio,mean_portfolio_optimized)) %>%
  ggplot(aes(x=sd,y=mean,color=asset)) +
  geom_point(size=2, show.legend = T) + 
  scale_x_continuous(labels = scales::percent) +
  labs(y="Retorno esperado",
       x="Risco",
       colour="Portfólio") +
  theme_bw() +
  theme(legend.position = "bottom")


  


## optimização de portfolio e fronteira eficiente ----

p = possibilities %>%
  ggplot(aes(x = sd, y = mean, color = sharpeRatePortfolio)) +
  geom_point() +
  geom_point(aes(x = sd,
                 y = mean), data = min_var, color = 'red') +
  geom_point(aes(x = sd,
                 y = mean), data = max_sr, color = 'red') +
  geom_text(x=min_var$sd - .01, y = min_var$mean,label="M.V", color = "red",size=3,check_overlap = T) +
  geom_text(x=max_sr$sd - .01, y = max_sr$mean,label="P.T", color = "red",size=3, check_overlap = T) +
  geom_text(x= 0.01,y=rfr,color="red",label="Rf",size=3, check_overlap = T) +
  geom_point(aes(x = 0,y=rfr),color="red") +
  expand_limits(x = 0, y = 0) +
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = 'Risco',
       y = 'Retorno esperado',
       title = "Optimização de portfolio & Fronteira eficiente",
       colour = "Indice sharpe") +
  theme_bw() +
  theme( plot.title = element_text(hjust = .5))


p + geom_segment(aes(x=0,
                     y=rfr,
                     xend= max_sr$sd,
                     yend=max_sr$mean), color="black")




# p + geom_segment(aes(x=0,
#                      y=rfr,
#                      xend= max_risk$sd,
#                      yend=max_sr$mean + .048), color="black")


## retornos diarios portfolio ----

### estrutura ----

portfolio_plot = portfolio_return %>% 
  as.data.frame(row.names = index(.)) %>%
  rownames_to_column("date") %>%
  rename("returns"=portfolio.returns) %>%
  mutate(date = as.Date(date),
         color = case_when(
           returns < mean_portfolio - sd_portfolio ~ "red",
           returns > mean_portfolio + sd_portfolio ~ "green",
           T ~ "darkblue"
         ),
         color= factor(color, levels = c("green","darkblue","red")))


portfolio_plot_optimized = portfolio_return_opmitized %>% 
  as.data.frame(row.names = index(.)) %>%
  rownames_to_column("date") %>%
  rename("returns"=portfolio.returns) %>%
  mutate(date = as.Date(date),
         color = case_when(
           returns < mean_portfolio_optimized - sd_portfolio_optimized ~ "red",
           returns > mean_portfolio_optimized + sd_portfolio_optimized ~ "green",
           T ~ "darkblue"
         ),
         color= factor(color, levels = c("green","darkblue","red")))



### variavel limitadora do eixo Y ----

yMax = max(c(portfolio_plot$returns, portfolio_plot_optimized$returns)) 
yMin = min(c(portfolio_plot$returns, portfolio_plot_optimized$returns)) 

### graficos ----

portfolio_plot %>%
  ggplot(aes(x=date, y =returns,color=color)) +
  geom_point() +
  scale_color_manual(name="Legenda",
                     labels=c("1 Desvio padrão acima da media",
                              "esta entre um desvio padrão acima e abaixo da média",
                              "1 Desvio padrão abaixo da média"),
                     values = c("darkblue"="darkblue","green"="green","red"="red")) +
  scale_x_date(breaks = pretty_breaks(n = 3)) +
  scale_y_continuous(breaks = pretty_breaks(n = 5),limits = c(yMin,yMax)) +
  geom_hline(yintercept = (mean_portfolio + sd_portfolio),
             color = "black",
             linetype = "dashed") +
  geom_hline(yintercept = (mean_portfolio- sd_portfolio),
             color = "black",
             linetype = "dashed") +
  theme_bw() +
  labs(x="Data",y="Retornos",title = "Gráfico de disperção dos retornos do portfolio normal") +
  theme(legend.text=element_text(size=7),
        legend.position = "bottom",
        legend.spacing.x = unit(0,"mm") ,
        legend.direction = "horizontal",
        legend.title = element_blank())


portfolio_plot_optimized %>%
  ggplot(aes(x=date, y =returns,color=color)) +
  geom_point() +
  scale_color_manual(name="Legenda",
                     labels=c("1 Desvio padrão acima da media",
                              "esta entre um desvio padrão acima e abaixo da média",
                              "1 Desvio padrão abaixo da média"),
                     values = c("darkblue"="darkblue","green"="green","red"="red")) +
  scale_x_date(breaks = pretty_breaks(n = 3)) +
  scale_y_continuous(breaks = pretty_breaks(n = 5),limits = c(yMin,yMax)) +
  geom_hline(yintercept = (mean_portfolio_optimized + sd_portfolio_optimized),
             color = "black",
             linetype = "dashed") +
  geom_hline(yintercept = (mean_portfolio_optimized- sd_portfolio_optimized),
             color = "black",
             linetype = "dashed") +
  theme_bw() +
  labs(x="Data",y="Retornos",title = "Gráfico de disperção dos retornos do portfolio Optimizado") +
  theme(legend.text=element_text(size=7),
        legend.position = "bottom",
        legend.spacing.x = unit(0,"mm") ,
        legend.direction = "horizontal",
        legend.title = element_blank())

###





