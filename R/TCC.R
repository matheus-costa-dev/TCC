library(tidyverse)
library(quantmod)
library(scales)
library(PerformanceAnalytics)
library(openxlsx)
library(corrplot)
library(RColorBrewer)
library(knitr)
source("functions/getAllPossibilites.R")
source("functions/loopPossibilites.R")


# pacotes = c("tidyverse","quantmod","scales","PerformanceAnalytics","openxlsx","corrplot","RColorBrewer","knitr")
# lapply(pacotes, require, character.only = TRUE)

## configuração 1 - top 10 maiores capitalização de mercado

## todas as moedas q possuem dados desde 2020-01-01 e estão no top 10 retirada

## variaveis

#"USDC-USD","USDT-USD"

options(scipen=999)
from = "2020-01-01"
to = "2023-01-01"
rfr = 0.1375 # taxa selic
by = .1
symbols = c("BTC-USD","ETH-USD","BNB-USD","XRP-USD","ADA-USD","DOGE-USD","SOL-USD","MATIC-USD")
Nsymbols = length(symbols)
folder = "outputData"
fileAssetReturn = paste0("assetReturn",Nsymbols,".rds")
fileAssetReturnLong = paste0("AssetReturnLong",Nsymbols,".xlsx")
filePossibilites = paste0("Possibilities",by,"-",rfr,".xlsx")
fullPathPossibilites = paste(folder,filePossibilites,sep = "/")
fullPathassetReturn = paste(folder,fileAssetReturn,sep = "/")
fullPathassetReturnLong = paste(folder,fileAssetReturnLong,sep = "/")

## codigo


if(fileAssetReturn %in% list.files(folder)){
  asset_return = readRDS(fullPathassetReturn)
} else {
  prices = getSymbols(symbols,
                      src = "yahoo",
                      from=from,
                      to=to,
                      auto.assign = TRUE) %>%
    map(~Ad(get(.))) %>%
    reduce(merge) %>%
    setNames(symbols)
  
  
  asset_return = Return.calculate(prices,method = "log")  %>% na.omit()
  
  saveRDS(asset_return,fullPathassetReturn)
}

covariance = cov(asset_return)
correlation = cor(asset_return)



if(fileAssetReturnLong %in% list.files(folder)){
  asset_return_long = read.xlsx(fullPathassetReturnLong)
} else {
  asset_return_long = asset_return %>%  
    as.data.frame(row.names = index(.)) %>% 
    rownames_to_column("data") %>%
    pivot_longer(cols =  all_of(symbols),names_to = "asset",values_to = "returns")  %>%
    arrange(asset)
  
  write.xlsx(asset_return_long,fullPathassetReturnLong)
}

summary_asset_long = asset_return_long %>% 
  group_by(asset) %>% 
  summarise(sd=sd(returns),mean=mean(returns))

less_riskier = slice_min(summary_asset_long,sd)
more_proftible = slice_max(summary_asset_long,mean)


w = 1/Nsymbols %>% rep(Nsymbols)

portfolio_return = Return.portfolio(asset_return,weights = w,rebalance_on = "months")
sd_portfolio = sd(portfolio_return$portfolio.returns)
mean_portfolio = mean(portfolio_return$portfolio.returns)
sr_portfolio = SharpeRatio(portfolio_return,Rf=rfr,FUN = "StdDev")


if(filePossibilites %in% list.files(folder)){
  
  possibilities = read.xlsx(fullPathPossibilites) %>% 
    mutate_all(~as.numeric(.x))
  
} else {
  possibilities = getAllPossibilites(symbols,by)
  possibilities = loopPossibilites(possibilities,asset_return,Nsymbols,rfr)
  
  write.xlsx(possibilities,fullPathPossibilites)
}

min_var <- possibilities[which.min(possibilities$sd),]
max_sr <- possibilities[which.max(possibilities$sharpeRatePortfolio),]
max_re <- possibilities[which.max(possibilities$mean),]


bestCombination = slice_max(possibilities,sharpeRatePortfolio)
summaryBestCombination = bestCombination[which(bestCombination[1:Nsymbols] > 0)]


w = max_sr[1:Nsymbols] %>% unlist()

portfolio_return_opmitized = Return.portfolio(asset_return,
                                              weights = w,
                                              rebalance_on = "months")

sd_portfolio_optimized = sd(portfolio_return_opmitized$portfolio.returns)
mean_portfolio_optimized = mean(portfolio_return_opmitized$portfolio.returns)
sr_portfolio_optimized = max_sr$sharpeRatePortfolio


## graficos e tabelas


asset_return_long %>%
  group_by(asset) %>%
  summarise(sd= sd(returns), mean= mean(returns)) %>%
  add_row(asset="PORTFOLIO", sd = sd_portfolio, mean=mean_portfolio ) %>%
  ggplot(aes(x=sd,y=mean,color=asset)) +
  geom_point(size=2, show.legend = FALSE) + 
  scale_x_continuous(labels = scales::percent) +
  labs(y="Retorno esperado",
       x="Risco",
       colour="Ativo",
       title = "Risco x Retorno") +
  geom_text(aes(label = asset, y = mean + .0002),size=3) +
  theme_bw() +
  theme(legend.position="none", plot.title = element_text(hjust = .5))

correlation2 = correlation %>% as.data.frame() %>% rownames_to_column("ATIVO")

kable(correlation2,
      format = "simple",
      digits = 3,
      format.args = list(scientific=FALSE,big.mark = ".",decimal.mark=","))

corrplot(correlation, 
         type="upper", 
         method = "ellipse",
         col=brewer.pal(n=Nsymbols, name="RdYlBu"),
         tl.col = "black"
)

slice_max(possibilities,sharpeRatePortfolio,n=5) %>% 
  mutate_at(vars(1:Nsymbols),~paste0(.x*100,"%")) %>%
  mutate_at(vars(c("sharpeRatePortfolio","sd")), ~paste0(round(.x * 100,4),"%")) %>%
  rename(`Indice Sharpe` = sharpeRatePortfolio,
         Risco=sd,
         `Retorno esperado`= mean) %>%
  kable(format = "simple") 

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

portfolio_plot = portfolio_return %>% as.data.frame(row.names = index(.)) %>%
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



### test
yMax = max(c(portfolio_plot$returns, portfolio_plot_optimized$returns)) %>% round(1)
yMin = min(c(portfolio_plot$returns, portfolio_plot_optimized$returns)) %>% round(1)

###

portfolio_plot %>%
  ggplot(aes(x=date, y =returns,color=color)) +
  geom_point() +
  scale_color_manual(name="Legenda",
                     labels=c("1 Desvio padrão acima da media",
                              "esta entre um desvio padrão acima e abaixo da média",
                              "1 Desvio padrão abaixo da média"),
                     values = c("darkblue"="darkblue","green"="green","red"="red")) +
  scale_x_date(breaks = pretty_breaks(n = 12)) +
  scale_y_continuous(breaks = pretty_breaks(n = 5),limits = c(yMin,yMax)) +
  geom_hline(yintercept = (mean_portfolio + sd_portfolio),
             color = "purple",
             linetype = "solid") +
  geom_hline(yintercept = (mean_portfolio- sd_portfolio),
             color = "purple",
             linetype = "solid") +
  theme_bw() +
  labs(x="Data",y="Retornos",title = "Gráfico de disperção dos retornos do portfolio normal") +
  theme(legend.text=element_text(size=7),
        legend.position = "bottom",
        legend.spacing.x = unit(0,"mm") ,
        legend.direction = "horizontal")


portfolio_plot_optimized %>%
  ggplot(aes(x=date, y =returns,color=color)) +
  geom_point() +
  scale_color_manual(name="Legenda",
                     labels=c("1 Desvio padrão acima da media",
                              "esta entre um desvio padrão acima e abaixo da média",
                              "1 Desvio padrão abaixo da média"),
                     values = c("darkblue"="darkblue","green"="green","red"="red")) +
  scale_x_date(breaks = pretty_breaks(n = 12)) +
  scale_y_continuous(breaks = pretty_breaks(n = 5),limits = c(yMin,yMax)) +
  geom_hline(yintercept = (mean_portfolio_optimized + sd_portfolio_optimized),
             color = "purple",
             linetype = "solid") +
  geom_hline(yintercept = (mean_portfolio_optimized- sd_portfolio_optimized),
             color = "purple",
             linetype = "solid") +
  theme_bw() +
  labs(x="Data",y="Retornos",title = "Gráfico de disperção dos retornos do portfolio Optimizado") +
  theme(legend.text=element_text(size=7),
        legend.position = "bottom",
        legend.spacing.x = unit(0,"mm") ,
        legend.direction = "horizontal")
