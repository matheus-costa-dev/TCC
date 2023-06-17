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



### 1 - metodo quantomood + perfomanceanalytics

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

asset_return_xts <- Return.calculate(prices_to_monthly,method = "log")

### 2 - metodo tidyverse


prices<- getSymbols(Symbols = symbols,
                    src = "yahoo",
                    from= "2012-12-31",
                    to = "2017-12-31",
                    auto.assign = T,
                    warnings = F) %>%
  map(~get(.x) %>% 
        as.data.frame(row.names = index(.)) %>% 
        select(contains("Ad"))) %>% 
  reduce(cbind) %>%
  setNames(symbols)

asset_return_dplyr_byhand <- to.monthly(prices ,indexAt = "lastof", OHLC = F) %>%
  rownames_to_column("data") %>% 
  pivot_longer(cols =2:ncol(.),names_to = "asset",values_to = "prices") %>%
  arrange(asset,data) %>%
  group_by(asset) %>%
  mutate(return = (log(prices) - log(lag(prices)))) %>%
  select(-prices) %>%
  pivot_wider(names_from = asset,values_from = return) %>%
  slice(-1) %>%
  relocate(data,symbols)


### 3 - metodo tidyquant

prices <- getSymbols(Symbols = symbols,
                     src="yahoo",
                     from = "2012-12-31",
                     to = "2017-12-31",
                     auto.assign = T,
                     warnings = F) %>% 
  map(~get(.x) %>%
        as.data.frame(row.names = index(.)) %>%
        select(contains("Ad"))) %>%
  reduce(cbind) %>%
  setNames(symbols)


asset_return_tidyquant <- prices %>% 
  tk_tbl(preserve_index = T,rename_index = "data") %>%
  pivot_longer(cols = 2:ncol(.),names_to = "asset",values_to = "prices") %>%
  arrange(asset,data) %>%
  group_by(asset) %>%
  tq_transmute(mutate_fun = periodReturn,
               period = "monthly",
               type="log",
               col_rename = "return") %>%
  pivot_wider(names_from = asset,values_from = return) %>%
  slice(-1)


### 4 - metodo tibbletime


prices <- getSymbols(Symbols = symbols,
                     src="yahoo",
                     from = "2012-12-31",
                     to = "2017-12-31",
                     auto.assign = T,
                     warnings = F) %>% 
  map(~get(.x) %>%
        as.data.frame(row.names = index(.)) %>%
        select(contains("Ad"))) %>%
  reduce(cbind) %>%
  setNames(symbols)

asset_return_tibbletime <- prices %>% 
  tk_tbl(rename_index = "data") %>%
  as_tbl_time(index = data) %>%
  as_period(period = "month",side = "end") %>%
  pivot_longer(cols = 2:ncol(.),names_to = "asset",values_to = "prices") %>%
  arrange(asset,data) %>%
  group_by(asset) %>%
  tq_transmute(mutate_fun = periodReturn,
               type = "log",
               col_rename = "return") %>%
  pivot_wider(names_from = asset,values_from = return) %>%
  relocate(data,symbols) %>%
  slice(-1)


### visualizaçao de dados


highchart(type = "stock") %>%
  hc_title(text = "Monthly Log Return") %>%
  hc_add_series(asset_return_xts[,symbols[1]],name = symbols[1]) %>%
  hc_add_series(asset_return_xts[,symbols[2]],name = symbols[2]) %>%
  hc_add_series(asset_return_xts[,symbols[3]],name = symbols[3]) %>%
  hc_add_series(asset_return_xts[,symbols[4]],name = symbols[4]) %>%
  hc_add_series(asset_return_xts[,symbols[5]],name = symbols[5]) %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_navigator(enabled = F) %>%
hc_scrollbar(enabled = FALSE) %>%
  hc_exporting(enabled = TRUE) %>%
  hc_legend(enabled = TRUE)




asset_return_long <- asset_return_dplyr_byhand %>%
pivot_longer(cols = 2:6,names_to = "asset",values_to = "Returns") %>%
  group_by(asset)


