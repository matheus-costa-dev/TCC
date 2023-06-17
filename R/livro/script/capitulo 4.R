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

prices <- map(symbols,
              ~getSymbols(Symbols = .x,from="2012-12-31", to ="2017-12-31" ,warnings = F,auto.assign = F) %>%
                as.data.frame(row.names = index(.)) %>%
                select(contains("Ad")) %>%
                setNames(.x)) %>% 
  reduce(cbind)


prices_monthly <- to.monthly(prices,indexAt = "lastof", OHLC = F)

asset_returns_xts <- Return.calculate(prices_monthly,method = "log") %>% na.omit()

w <- c(0.25, 
       0.25, 
       0.20, 
       0.20, 
       0.10)

portfolio_returns_xts_rebalanced_monthly <- Return.portfolio(asset_returns_xts,weights = w,rebalance_on = "months") %>%
  setNames("Return")

### desvio padrao na mao

covariance_matrix <- cov(asset_returns_xts)
round(covariance_matrix, 5)


sd_matrix_algebra <- sqrt(t(w) %*% covariance_matrix %*% w)
sd_matrix_algebra_percent <-
  round(sd_matrix_algebra * 100, 2) %>%
  `colnames<-`("standard deviation")
sd_matrix_algebra_percent[1,]

### desvio padrao in xts world

portfolio_sd_xts_builtin <- StdDev(asset_returns_xts,weights = w)
portfolio_sd_xts_builtin_percent <-
  round(portfolio_sd_xts_builtin * 100, 2)

### desvio padrao no dpylr

asset_returns_long <- to.monthly(prices ,indexAt = "lastof", OHLC = F) %>%
  rownames_to_column("data") %>% 
  pivot_longer(cols =2:ncol(.),names_to = "asset",values_to = "prices") %>%
  arrange(asset,data) %>%
  group_by(asset) %>%
  mutate(returns = (log(prices) - log(lag(prices)))) %>%
  select(-prices) %>%
  na.omit()

portfolio_returns_dplyr_byhand <- 
  asset_returns_long %>%
  group_by(asset) %>% 
  mutate(weights = case_when(asset == symbols[1] ~ w[1],
                             asset == symbols[2] ~ w[2],
                             asset == symbols[3] ~ w[3],
                             asset == symbols[4] ~ w[4],
                             asset == symbols[5] ~ w[5]),
         weighted_returns = returns * weights,
         data= as.Date(data)) %>% 
  group_by(data) %>% 
  summarise(returns = sum(weighted_returns))


portfolio_sd_tidy_builtin_percent <- 
  portfolio_returns_dplyr_byhand %>%
  summarise(sd = sd(returns)) %>%
  mutate(sd = round(sd,4)*100)


portfolio_returns_dplyr_byhand %>%
  ggplot(aes(x = data, y = returns)) +
  geom_point(color = "cornflowerblue") +
  scale_x_date(breaks = pretty_breaks(n = 6)) +
  ggtitle("Scatterplot of Returns by Date") +
  theme(plot.title = element_text(hjust = 0.5))


mean_plot <- mean(portfolio_returns_dplyr_byhand$returns)
sd_plot <- sd(portfolio_returns_dplyr_byhand$returns)

portfolio_returns_dplyr_byhand %>%
  mutate(
    hist_col_red = ifelse(returns < (mean_plot - sd_plot),returns,as.numeric(NA) ),
    hist_col_green = ifelse(returns > (mean_plot + sd_plot),returns,as.numeric(NA) ),
    hist_col_blue = ifelse(returns > (mean_plot - sd_plot)&
                           returns < (mean_plot + sd_plot) ,returns,as.numeric(NA) )
    ) %>%
  ggplot(aes(x = data)) +
  geom_point(aes(y = hist_col_red),
             color = "red") +
  geom_point(aes(y = hist_col_green),
             color = "green") +
  geom_point(aes(y = hist_col_blue),
             color = "blue") +
  geom_hline(yintercept = (mean_plot + sd_plot),
             color = "purple",
             linetype = "dotted") +
  geom_hline(yintercept = (mean_plot-sd_plot),
             color = "purple",
             linetype = "dotted") +
  labs(title = "Colored Scatter with Line", y = "monthly returns") +
  scale_x_date(breaks = pretty_breaks(n = 8)) +
  theme(plot.title = element_text(hjust = 0.5))

sd_plot
mean_plot


asset_returns_long %>%
  group_by(asset) %>%
  summarise(sd=sd(returns)) %>%
  add_row(asset="Portifolio",sd=sd_plot) %>%
  ggplot(aes(x=asset,y=sd,color=asset)) + 
  geom_point(size=3) +
  geom_text(aes(x="Portifolio",y=(sd_plot+.002),label="portifolio")) +
  labs(x="Ativos",y="Desvio padrão") + 
  theme_bw()


asset_returns_long %>%
  group_by(asset) %>%
  summarise(sd=sd(returns),
            expected_return = mean(returns)) %>%
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


### calculating volatility
window <- 24

## in xts world
port_rolling_sd_xts <- rollapply(portfolio_returns_xts_rebalanced_monthly,FUN=sd,width=window) %>% 
  na.omit() %>% 
  setNames("rolling_sd")

## in tidyverse not work well

portfolio_returns_dplyr_byhand %>%
  mutate(sd_rolling = rollapply(returns,width=window,FUN=sd,fill=NA)) %>% na.omit() %>%tail() ##missing 2017 and 2012

## in tibble

sd_roll_24 <- rollify(sd,window = window)

port_rolling_sd_tidy_tibbletime <- 
  portfolio_returns_dplyr_byhand %>%
  as_tbl_time(index = data) %>%
  mutate(sd = sd_roll_24(returns)) %>%
  select(-returns) %>%
  na.omit() 

tail(port_rolling_sd_tidy_tibbletime)

### visualization rolling sd

port_rolling_sd_tidy_tibbletime %>% 
  ggplot(aes(x=data,y=sd)) +
  geom_line(color="cornflowerblue") +
  theme_bw() +
  scale_y_continuous(labels = percent) +
  scale_x_date(breaks = pretty_breaks(8))

### highchart

port_rolling_sd_xts_hc <-
  round(port_rolling_sd_xts, 4) * 100

highchart(type = "stock") %>%
  hc_title(text = "24-Month Rolling Volatility") %>%
  hc_add_series(port_rolling_sd_xts_hc,
                color = "cornflowerblue") %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_yAxis(
    labels = list(format = "{value}%"),
    opposite = FALSE) %>%
  hc_navigator(enabled = FALSE) %>%
  hc_scrollbar(enabled = FALSE) %>%
  hc_exporting(enabled= TRUE) %>%
  hc_legend(enabled = TRUE)

### realizando na mão rolling deviation

resultado_funcao <- rollapply(portfolio_returns_xts_rebalanced_monthly,FUN = sd,width= window,fill = NA)
resultado_byhand <- data.frame()

for(i in 1:nrow(portfolio_returns_xts_rebalanced_monthly)){
  janela = i-window+1
  tryCatch({
    if(janela>0){
      resultado_byhand = rbind(resultado_byhand,
                               sd(portfolio_returns_xts_rebalanced_monthly[i:janela]))
    } else{
      stop()
    }
    
    },error = function(e){
      resultado_byhand <<- rbind(resultado_byhand,
                              NA)
    })  
}

resultado_byhand <- cbind(resultado_byhand,
                          portfolio_returns_dplyr_byhand$data) %>%
  setNames(c("Return","date")) %>%
  relocate(date) %>%
  column_to_rownames("date") %>% as.xts()

resultado_byhand == resultado_funcao

