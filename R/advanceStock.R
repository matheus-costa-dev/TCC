library(tidyverse)
library(quantmod)
library(scales)
library(PerformanceAnalytics)
library(openxlsx)
source("functions/possibilites.R")

options(scipen=999)
from = "2020-01-01"
to = "2023-01-01"

### verifica se a mesma situação ocorre na mercado capital noraml
symbols = c("AAPL","MSFT","2222.SR","GOOG","AMZN","NVDA","TSLA","BRK-B")
Nsymbols = length(symbols)


prices = getSymbols(symbols,
                    src = "yahoo",
                    from=from,
                    to=to,
                    auto.assign = TRUE) %>%
  map(~Ad(get(.))) %>%
  reduce(merge) %>%
  setNames(symbols)

asset_return = Return.calculate(prices,method = "log")  %>% na.omit()

write_rds(asset_return,"outputData/assetReturnStockMarket.rds")

asset_return_long = asset_return %>%  as.data.frame(row.names = index(.)) %>% rownames_to_column("data") %>%
  pivot_longer(cols =  all_of(symbols),names_to = "asset",values_to = "returns")  %>%
  arrange(asset)

write.xlsx(asset_return_long,"outputData/assetReturnLongStockMarket.xlsx")



covariance = cov(asset_return)
correlation = cor(asset_return)

rfr = 0.1375 # taxa selic
by = .1
w = 1/length(symbols) %>% rep(length(symbols))


possibilities = read.xlsx("outputData/Possibilities0.xlsx") %>% 
  mutate_all(~as.numeric(.x)) %>%
  select(1:Nsymbols) %>%
  setNames(symbols)

possibilities = loopPossibilites(possibilities,asset_return,Nsymbols,rfr)

write.xlsx(possibilities,"outputData/PossibilitiesStockMarket0.00531.xlsx")

min_var <- slice_min(possibilities,sd)
max_sr <- slice_max(possibilities,sharpeRatePortfolio)
max_re <- slice_max(possibilities,sharpeRatePortfolio)


bestCombination = slice_max(possibilities,sharpeRatePortfolio)
summaryBestCombination = bestCombination[which(bestCombination[1:Nsymbols] > 0)]



w = max_sr[1:Nsymbols] %>% unlist()

portfolio_return_opmitized = Return.portfolio(asset_return,
                                              weights = w,
                                              rebalance_on = "months")

sd_portfolio_optimized = sd(portfolio_return_opmitized$portfolio.returns)
mean_portfolio_optimized = mean(portfolio_return_opmitized$portfolio.returns)
sr_portfolio_optimized = max_sr$sharpeRatePortfolio


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


### grafico risco x retorno optimizado

asset_return_long %>%
  group_by(asset) %>%
  summarise(sd= sd(returns), mean= mean(returns)) %>%
  add_row(asset="PORTFOLIO OPTIMIZADO", sd = sd_portfolio_optimized, mean=mean_portfolio_optimized ) %>%
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
  labs(x="Data",y="Retornos",title = "Gráfico de disperção dos retornos do portfolio optimizado") +
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

