library(tidyverse)
library(quantmod)
library(scales)


options(scipen = 999)
symbols <- c("SPY","EFA", "IJS", "EEM","AGG")


prices <- map(symbols,~getSymbols(Symbols = .x,from="2012-12-31", to ="2017-12-31" ,warnings = F,auto.assign = F) %>%
                as.data.frame(row.names = index(.)) %>%
                select(contains("Ad")) %>%
                setNames(.x)) %>% reduce(cbind)


prices_long <- to.monthly(prices,indexAt = "lastof", OHLC = F) %>%
  rownames_to_column("data") %>%
  pivot_longer(cols = 2:ncol(.), names_to = "asset",values_to = "returns") %>%
  arrange(asset,data) %>%
  group_by(asset) %>%
  mutate(log_return = (log(returns) - log(lag(returns)))) %>%
  na.omit() %>%
  select(-returns)


prices_long %>% 
  ggplot(aes(x = log_return)) +
  geom_histogram(alpha = .45, binwidth = .015, aes(fill=asset)) +
  geom_density(aes(color = asset)) +
  facet_wrap(~asset) +
  theme_bw()


w <- c(0.25,0.25,0.20,0.20,0.1)


portifolio_return <- prices_long %>% 
  mutate(weight = case_when(
    asset == symbols[1] ~w[1],
    asset == symbols[2] ~w[2],
    asset == symbols[3] ~w[3],
    asset == symbols[4] ~w[4],
    asset == symbols[5] ~w[5]),
    return_weighted = log_return * weight,
    data = as.Date(data)) %>%
  group_by(data) %>%
  summarise(returns = sum(return_weighted))


### visualização via GGPLOT

portifolio_return %>%
  ggplot(aes(x=data,y=returns)) + 
  geom_line(color = "Darkblue") + 
  scale_x_date(breaks = "6 month", labels=date_format("%b / %Y")) +
  theme_bw()



### visualização via highchart

portifolio_return_xts <- portifolio_return %>% 
  column_to_rownames(var = "data") %>%
  as.xts() 


highchart(type = "stock") %>%
  hc_add_series(portifolio_return_xts$returns,name = "return", type = "line") %>%
  hc_xAxis(type = "datetime", labels = list(format = '{value:%Y %b}')) %>%
  hc_navigator(enabled = F) %>%
  hc_scrollbar(enabled = F) %>%
  hc_exporting(enabled = T) %>%
  hc_add_theme(hc_theme_flat())
