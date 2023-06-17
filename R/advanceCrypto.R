library(tidyverse)
library(quantmod)
library(scales)
library(PerformanceAnalytics)
library(openxlsx)

## configuração 1 - top 10 maiores capitalização de mercado

## todas as moedas q possuem dados desde 2020-01-01 e estão no top 10 retirada

options(scipen=999)
from = "2020-01-01"
to = "2023-01-01"

symbols = c("BTC-USD","ETH-USD","USDC-USD","USDT-USD","BNB-USD","XRP-USD","ADA-USD","DOGE-USD","SOL-USD","MATIC-USD")

if("assetReturn.rds" %in% list.files("outputData")){
  asset_return = readRDS("outputData/assetReturn.rds")
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
  
  saveRDS(asset_return,"outputData/assetReturn.rds")
}


covariance = cov(asset_return)
correlation = cor(asset_return)

rfr = 0.1375 # taxa selic
by = .2


if("AssetReturnLong.xlsx" %in% list.files("outputData")){
  asset_return_long = read.xlsx("outputData/assetReturnLong.xlsx")
} else {
  asset_return_long = asset_return %>%  as.data.frame(row.names = index(.)) %>% rownames_to_column("data") %>%
    pivot_longer(cols =  all_of(symbols),names_to = "asset",values_to = "returns")  %>%
    arrange(asset)
  
  write.xlsx(asset_return_long,"outputData/AssetReturnLong.xlsx")
}

w = 1/length(symbols) %>% rep(length(symbols))

portfolio_return = Return.portfolio(asset_return,weights = w,rebalance_on = "months")
sd_portfolio = sd(portfolio_return$portfolio.returns)
mean_portfolio = mean(portfolio_return$portfolio.returns)
sr_portfolio = SharpeRatio(portfolio_return,Rf=rfr,FUN = "StdDev")

## grafico risco x retorno

asset_return_long %>%
  group_by(asset) %>%
  summarise(sd= sd(returns), mean= mean(returns)) %>%
  add_row(asset="PORTFOLIO", sd = sd_portfolio, mean=mean_portfolio ) %>%
  ggplot(aes(x=sd,y=mean,color=asset)) +
  geom_point(size=3, show.legend = FALSE) + 
  scale_x_continuous(labels = scales::percent) +
  labs(y="Retorno esperado",
       x="Risco",
       colour="Ativo",
       title = "Risco x Retorno") +
  geom_text(aes(label = asset, y = mean + .0002)) +
  theme_bw() +
  theme(legend.position="none", plot.title = element_text(hjust = .5))


## grafico retorno do portfolio

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

  
portfolio_plot %>%
  ggplot(aes(x=date, y =returns,color=color)) +
  geom_point() +
  scale_color_manual(name="Legenda",
                     labels=c("1 Desvio padrão acima da media",
                              "esta entre um desvio padrão acima e abaixo da média",
                              "1 Desvio padrão abaixo da média"),
                     values = c("darkblue"="darkblue","green"="green","red"="red")) +
  scale_x_date(breaks = pretty_breaks(n = 12)) +
  geom_hline(yintercept = (mean_portfolio + sd_portfolio),
             color = "purple",
             linetype = "solid") +
  geom_hline(yintercept = (mean_portfolio-sd_portfolio),
             color = "purple",
             linetype = "solid") +
  theme_bw() +
  labs(x="Data",y="Retornos",title = "Gráfico de disperção dos retornos do portfolio") +
  theme(legend.text=element_text(size=7),
        legend.position = "bottom",
        legend.spacing.x = unit(0,"mm") ,
        legend.direction = "horizontal")

### possibilidades

if("Possibilities.xlsx" %in% list.files("outputData")){
  
  possibilities = read.xlsx("outputData/Possibilities.xlsx") %>% mutate_all(~as.numeric(.x))
  
} else {
  possibilities = rep(list(seq(0,1,by)),length(symbols)) %>%
    reduce(cbind) %>% as_tibble() %>%
    setNames(symbols)
  
  possibilities = do.call(expand.grid, possibilities[1:length(symbols)])
  rows = which(rowSums(possibilities) == 1)
  possibilities = possibilities[rows,]
  
  for(i in 1:nrow(possibilities)) {
    
    
    w = possibilities[i,1:length(symbols)]  %>% as.numeric()
    
    portfolio_return = Return.portfolio(asset_return,weights = w,rebalance_on = "months")
    
    possibilities[i ,"sharpeRatePortfolio"] = SharpeRatio(portfolio_return,Rf=rfr,FUN = "StdDev")
    possibilities[i ,"sd"] = sd(portfolio_return$portfolio.returns)
    possibilities[i,"mean"] = mean(portfolio_return$portfolio.returns)
    
    print(paste(i,nrow(possibilities),sep = " - "))
    
  }
  
  
  write.xlsx(asset_return_long,"outputData/AssetReturnLong.xlsx")
}

min_var <- possibilities[which.min(possibilities$sd),]
max_sr <- possibilities[which.max(possibilities$sharpeRatePortfolio),]
max_re <- possibilities[which.max(possibilities$mean),]

w = max_sr[1:length(symbols)] %>% unlist()

portfolio_return_opmitized = Return.portfolio(asset_return,
                                              weights = w,
                                              rebalance_on = "months")

sd_portfolio_optimized = sd(portfolio_return_opmitized$portfolio.returns)
mean_portfolio_optimized = mean(portfolio_return_opmitized$portfolio.returns)
sr_portfolio_optimized = max_sr$sharpeRatePortfolio

### grafico risco x retorno optimizado

asset_return_long %>%
  group_by(asset) %>%
  summarise(sd= sd(returns), mean= mean(returns)) %>%
  add_row(asset="PORTFOLIO", sd = sd_portfolio_optimized, mean=mean_portfolio_optimized ) %>%
  ggplot(aes(x=sd,y=mean,color=asset)) +
  geom_point(size=3, show.legend = FALSE) + 
  scale_x_continuous(labels = scales::percent) +
  labs(y="Retorno esperado",
       x="Risco",
       colour="Ativo",
       title = "Risco x Retorno") +
  geom_text(aes(label = asset, y = mean + .0002)) +
  theme_bw() +
  theme(legend.position="none", plot.title = element_text(hjust = .5))

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

portfolio_plot %>%
  ggplot(aes(x=date, y =returns,color=color)) +
  geom_point() +
  scale_color_manual(name="Legenda",
                     labels=c("1 Desvio padrão acima da media",
                              "esta entre um desvio padrão acima e abaixo da média",
                              "1 Desvio padrão abaixo da média"),
                     values = c("darkblue"="darkblue","green"="green","red"="red")) +
  scale_x_date(breaks = pretty_breaks(n = 12)) +
  geom_hline(yintercept = (mean_portfolio_optimized + sd_portfolio_optimized),
             color = "purple",
             linetype = "solid") +
  geom_hline(yintercept = (mean_portfolio_optimized- sd_portfolio_optimized),
             color = "purple",
             linetype = "solid") +
  theme_bw() +
  labs(x="Data",y="Retornos",title = "Gráfico de disperção dos retornos do portfolio") +
  theme(legend.text=element_text(size=7),
        legend.position = "bottom",
        legend.spacing.x = unit(0,"mm") ,
        legend.direction = "horizontal")

### retorno portfolio optimizado

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


portfolio_plot_optimized %>%
  ggplot(aes(x=date, y =returns,color=color)) +
  geom_point() +
  scale_color_manual(name="Legenda",
                     labels=c("1 Desvio padrão acima da media",
                              "esta entre um desvio padrão acima e abaixo da média",
                              "1 Desvio padrão abaixo da média"),
                     values = c("darkblue"="darkblue","green"="green","red"="red")) +
  scale_x_date(breaks = pretty_breaks(n = 12)) +
  geom_hline(yintercept = (mean_portfolio_optimized + sd_portfolio_optimized),
             color = "purple",
             linetype = "solid") +
  geom_hline(yintercept = (mean_portfolio_optimized- sd_portfolio_optimized),
             color = "purple",
             linetype = "solid") +
  theme_bw() +
  labs(x="Data",y="Retornos",title = "Gráfico de disperção dos retornos do portfolio") +
  theme(legend.text=element_text(size=7),
        legend.position = "bottom",
        legend.spacing.x = unit(0,"mm") ,
        legend.direction = "horizontal")


### verificando

comparacao = tibble(categoria = c("negativos","positivos","um desvio padrão acima da média","um desvio padrão abaixo da média",
                     "esta entre um desvio padrão acima e abaixo da média"))



comparacao = data.frame(matrix(ncol = 9,nrow=1)) %>%
  setNames(c("portfolio","negativos","positivos","um desvio padrão acima da média",
             "esta entre um desvio padrão acima e abaixo da média","um desvio padrão abaixo da média",
             "risco","retorno esperado","indice sharpe"
             ))



portfolio_normal_summary = portfolio_plot  %>% summarise(negativos = sum(returns < 0),
                              positivos = sum(returns >= 0),
                              `um desvio padrão acima da média` = sum(color == "green"),
                              `esta entre um desvio padrão acima e abaixo da média` =  sum(color == "darkblue"),
                              `um desvio padrão abaixo da média`= sum(color == "red"))

portfolio_optimized_summary = portfolio_plot_optimized  %>% summarise(negativos = sum(returns < 0),
                                                            positivos = sum(returns >= 0),
                                                            `um desvio padrão acima da média` = sum(color == "green"),
                                                            `esta entre um desvio padrão acima e abaixo da média` =  sum(color == "darkblue"),
                                                            `um desvio padrão abaixo da média`= sum(color == "red"))


resultado_comparativo = comparacao %>% add_row(
  portfolio = "normal",
  negativos = portfolio_normal_summary$negativos,
  positivos = portfolio_normal_summary$positivos,
  `um desvio padrão acima da média` = portfolio_normal_summary$`um desvio padrão acima da média`,
  `esta entre um desvio padrão acima e abaixo da média` = portfolio_normal_summary$`esta entre um desvio padrão acima e abaixo da média`,
  `um desvio padrão abaixo da média` = portfolio_normal_summary$`um desvio padrão abaixo da média`,
  risco = sd_portfolio,
  `retorno esperado` = mean_portfolio,
  `indice sharpe` = sr_portfolio
) %>%
  add_row(
    portfolio = "optimized",
    negativos = portfolio_optimized_summary$negativos,
    positivos = portfolio_optimized_summary$positivos,
    `um desvio padrão acima da média` = portfolio_optimized_summary$`um desvio padrão acima da média`,
    `esta entre um desvio padrão acima e abaixo da média` = portfolio_optimized_summary$`esta entre um desvio padrão acima e abaixo da média`,
    `um desvio padrão abaixo da média` = portfolio_optimized_summary$`um desvio padrão abaixo da média`,
    risco = sd_portfolio_optimized,
    `retorno esperado` = mean_portfolio_optimized,
    `indice sharpe` = sr_portfolio_optimized
  ) %>%
  na.omit() %>%
  pivot_longer(cols = -portfolio, names_to = "categorias",values_to = "valores")

resultado_comparativo %>%
  ggplot(aes(x=categorias,y=valores,fill=portfolio)) +
  geom_bar(stat="identity",position = "dodge",width = .1) +
  facet_wrap(~categorias,scales = "free")

### test

window <- 24

rolling_sd_portfolio = rollapply(portfolio_return,
            FUN = sd,
            width = window) %>%
  na.omit() %>% 
  setNames("rolling_sd")

rolling_sd_portfolio %>% 
  as.data.frame(row.names = index(.)) %>%
  rownames_to_column("date") %>% 
  mutate(date=as.Date(date)) %>%
  ggplot(aes(x=date,y=rolling_sd)) +
  geom_line()



rolling_mean_portfolio = rollapply(portfolio_return,
                                 FUN = mean,
                                 width = window) %>%
  na.omit() %>% 
  setNames("rolling_mean")

x = cbind(rolling_sd_portfolio,rolling_mean_portfolio) %>% 
  as.data.frame(row.names = index(.)) %>%
  rownames_to_column("date") %>% 
  mutate(rolling_sr = (rolling_mean - rfr)/rolling_sd) %>%
  summarise(mean_sr = mean(rolling_sr))


## optimizado

window <- 30

rolling_sd_portfolio_optimized = rollapply(portfolio_return_opmitized,
                                 FUN = sd,
                                 width = window) %>%
  na.omit() %>% 
  setNames("rolling_sd")

rolling_sd_portfolio_optimized %>% 
  as.data.frame(row.names = index(.)) %>%
  rownames_to_column("date") %>% 
  mutate(date=as.Date(date)) %>%
  ggplot(aes(x=date,y=rolling_sd)) +
  geom_line()



rolling_mean_portfolio_optimized  = rollapply(portfolio_return_opmitized ,
                                   FUN = mean,
                                   width = window) %>%
  na.omit() %>% 
  setNames("rolling_mean")

x = cbind(rolling_sd_portfolio_optimized,rolling_mean_portfolio_optimized) %>% 
  as.data.frame(row.names = index(.)) %>%
  rownames_to_column("date") %>% 
  mutate(rolling_sr = (rolling_mean - rfr)/rolling_sd) %>%
  summarise(mean_sr = mean(rolling_sr))


### sharpe na mao

portfolio_return %>% 
  as.data.frame(row.names = index(.)) %>%
  rownames_to_column("date") %>%
  rename(returns = portfolio.returns) %>%
  summarise(sr =(mean(returns) - rfr)/  sd(returns) )

###

library(corrplot)
library(RColorBrewer)
M <-cor(mtcars)
corrplot(correlation, 
         type="upper", 
         method = "ellipse",
         col=brewer.pal(n=length(symbols), name="RdYlBu"),
         tl.col = "black",
         )
library(gtsummary)

kable(correlation,"simple",
      digits = 3,
      format.args = list(scientific=FALSE,big.mark = ".",decimal.mark=","))


kable(possibilities,"simple",
      digits = 3,
      format.args = list(scientific=FALSE,big.mark = ".",decimal.mark=","))


test = possibilities
test[1:length(symbols)] = paste(possibilities[1:length(symbols)]*100,"%")
kable(possibilities,digits = c(rep(0,length(symbols)),rep(3,2),5))

possibilities %>% 
  arrange(desc(sharpeRatePortfolio)) %>% 
  head() %>% 
  gt() %>% 
  fmt_percent(columns = symbols,decimals = 0, dec_mark = ",", sep_mark = ".") %>%
  fmt_percent(columns = c(sharpeRatePortfolio,sd),decimals = 3, dec_mark = ",", sep_mark = "." ) %>%
  cols_label(sharpeRatePortfolio = "Indice Sharpe", sd= "Risco", mean = "Retorno esperado")


kable(test2,"simple" ,escape = T)
