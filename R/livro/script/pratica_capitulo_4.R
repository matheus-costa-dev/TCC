library(tidyverse)
library(quantmod)
library(scales)

options(scipen = 999)
symbols <- c("SPY","EFA", "IJS", "EEM","AGG")
w <- c(0.25,0.25,0.20,0.20,0.1)

prices <- map(symbols,~getSymbols(Symbols = .x , from="2012-12-31",to="2017-12-31",auto.assign = F) %>%
      as.data.frame(row.names = index(.)) %>%
      select(contains("Ad"))) %>%
  reduce(cbind) %>%
  setNames(symbols)

prices_monthly <- to.monthly(prices,indexAt = "lastof",OHLC=F)

returns_long <- prices_monthly %>% 
  rownames_to_column("date") %>%
  pivot_longer(cols=2:ncol(.),names_to = "asset",values_to = "returns") %>%
  group_by(asset) %>%
  mutate(return_log = log(returns) - log(lag(returns)),
         date = as.Date(date)) %>%
  na.omit() %>%
  select(-returns)

returns_long %>% ggplot(aes(x=date,y=return_log,color=asset)) + 
  geom_point() + 
  scale_x_date(breaks= pretty_breaks(6)) + 
  facet_wrap(~asset)

portifolio_return <-  returns_long %>%
  mutate(pesos = case_when(
    asset == symbols[1] ~ w[1],
    asset == symbols[2] ~ w[2],
    asset == symbols[3] ~ w[3],
    asset == symbols[4] ~ w[4],
    asset == symbols[5] ~ w[5],
    ),
    return_w = pesos * return_log) %>%
  group_by(date) %>%
  summarise(return = sum(return_w))


ggplot(portifolio_return,aes(x=date,y=return)) + geom_point() + theme_bw()


sd_plot <- sd(portifolio_return$return)
mean_plot <- mean(portifolio_return$return)

returns_long %>%
  group_by(asset) %>%
  summarise(sd = sd(return_log)) %>%
  add_row(asset = "portifolio",sd =sd_plot) %>%
  ggplot(aes(x=asset,y=sd,color=asset)) + 
  geom_point(size=3) +
  geom_text(aes(x="portifolio",y=(sd_plot+.002),label="portifolio")) +
  labs(x="Ativos",y="Desvio padrão") + 
  theme_bw()

returns_long %>%
  group_by(asset) %>%
  summarise(sd = sd(return_log),
            expected_return = mean(return_log)) %>%
  add_row(asset = "portifolio",
          sd =sd_plot,
          expected_return=mean_plot) %>%
  ggplot(aes(x=sd,y=expected_return,color=asset)) + 
  geom_point(size=3) +
  geom_text(aes(x=(sd_plot*1.05),y=(mean_plot),label="Portifolio")) +
  labs(y="Retorno Esperado",
       x="Desvio Padrão",
       title = "Expectativa de retorno mensal x Risco") + 
  scale_y_continuous(labels =  ~paste0(.,"%")) +
  theme_bw()

returns_long %>%
  group_by(asset) %>%
  summarise(var(return_log))

             