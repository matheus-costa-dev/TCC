library(quantmod)
library(tidyverse)
library(PerformanceAnalytics)
library(lubridate)
library(tidyquant)
library(timetk)
library(tibbletime)
library(highcharter)

options(scipen = 999)

symbols <- c("SPY","EFA", "IJS", "EEM","AGG")


prices <- getSymbols(Symbols = symbols,
                     src = "yahoo",
                     from= "2012-12-31",
                     to = "2017-12-31",
                     auto.assign = T,
                     warnings = F) %>%
  map(~Ad(get(.))) %>%
  reduce(merge) %>%
  setNames(symbols)



prices_to_monthly = to.monthly(prices,indexAt = "lastof",OHLC=F)

asset_return_xts <- Return.calculate(prices_to_monthly,method = "log") %>%
  na.omit()


### building a portifolio

w <- c(0.25,0.25,0.20,0.20,0.1)

tibble(symbols,w)

portfolio_returns_byhand <-
  (w[1] * asset_return_xts[,1]) +
  (w[2] * asset_return_xts[,2]) +
  (w[3] * asset_return_xts[,3]) +
  (w[4] * asset_return_xts[,4]) +
  (w[5] * asset_return_xts[,5])

names(portfolio_returns_byhand) <- "returns"


### xts portifolio

portfolio_returns_xts <- Return.portfolio(asset_return_xts,
                 weights = w ,
                 rebalance_on = "months") %>%
  setNames("Returns")



### dplyr portifolio

portfolio_returns_dplyr_byhand <- asset_return_dplyr_long %>%
  group_by(asset) %>%
  mutate(weights = case_when(
    asset == symbols[1] ~ w[1],
    asset == symbols[2] ~ w[2],
    asset == symbols[3] ~ w[3],
    asset == symbols[4] ~ w[4],
    asset == symbols[5] ~ w[5]
    ),
    weighted_return = return * weights) %>%
  group_by(data) %>%
  summarise(Returns = sum(weighted_return)) %>%
  na.omit()

### tidyquant portifolio

portfolio_returns_tq_rebalanced_monthly <- asset_return_long %>% 
  mutate(data = as.POSIXct(data)) %>%
  tq_portfolio(assets_col = asset,
               returns_col = Returns,
               weights = w,
               col_rename = "returns",
               rebalance_on = "months")



highchart(type = "stock") %>%
  hc_add_series(portfolio_returns_xts$Returns,
                name = "Rebalanced monthly",
                color = "cornflowerblue") %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_navigator(enabled =  F) %>%
  hc_scrollbar(enabled = F) %>%
  hc_legend(enabled = T) %>% 
  hc_exporting(enabled = T)



hc_portfolio <-
  hist(portfolio_returns_xts$Returns,
       breaks = 50,
       plot = FALSE)
hchart(hc_portfolio,
       color = "cornflowerblue",
       name = "Portfolio") %>%
  hc_title(text = "Portfolio Returns Distribution") %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_exporting(enabled = TRUE)


