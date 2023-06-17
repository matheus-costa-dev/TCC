library(PerformanceAnalytics)
library(quantmod)
library(scales)
library(tidyverse)
library(tidyquant)


symbols <- c("SPY","EFA", "IJS", "EEM","AGG")

### tidyverse

prices = map(symbols, 
    ~getSymbols(Symbols = .x,auto.assign = F,src = "yahoo",from="2012-12-31",to="2017-12-31") %>%
      as.data.frame(row.names = index(.)) %>%
      select(contains("Adjusted"))
      ) %>%
  reduce(cbind) %>%
  setNames(symbols)


prices_monthly = to.monthly(prices, indexAt = "lastof",OHLC=F)

asset_returns_dplyr = prices_monthly %>%
  rownames_to_column("date") %>%
  pivot_longer(cols = -date,names_to = "asset",values_to = "prices") %>%
  group_by(asset) %>%
  mutate(return = log(prices) - lag(log(prices)),
         date=as.Date(date)) %>%
  select(-prices) %>%
  na.omit()

asset_returns_dplyr %>% pivot_wider(names_from = "asset",values_from = "return")

asset_returns_dplyr %>% ggplot(aes(x=return, fill=asset)) + geom_histogram(alpha=0.3)
asset_returns_dplyr %>% ggplot(aes(x=return, fill=asset)) + geom_histogram(alpha=0.3) + facet_wrap(~asset)

asset_returns_dplyr %>%
  ggplot(aes(x = return)) +
  geom_density(aes(color = asset), alpha = 1) +
  geom_histogram(aes(fill = asset), alpha = 0.45, binwidth = .01) +
  guides(fill = FALSE) +
  facet_wrap(~asset) +
  ggtitle("Monthly Returns Since 2013") +
  xlab("monthly returns") +
  ylab("distribution") +
  theme_update(plot.title = element_text(hjust = 0.5))

w <- c(0.25,
       0.25,
       0.20,
       0.20,
       0.10)

portfolio_returns_dplyr = asset_returns_dplyr %>%
  group_by(asset) %>%
mutate(weights = case_when(
           asset == symbols[1] ~ w[1],
           asset == symbols[2] ~ w[2],
           asset == symbols[3] ~ w[3],
           asset == symbols[4] ~ w[4],
           asset == symbols[5] ~ w[5],
                   ),
       weighted_return = return * weights) %>%
  group_by(date) %>%
  summarise(returns = sum(weighted_return))


portfolio_returns_dplyr %>%
  ggplot(aes(x=date,y = returns)) + 
  geom_point(colour="cornflowerblue") + 
  labs(x="date",y="monthly return",title =  "Portfolio Returns Scatter") +
  scale_x_date(breaks = pretty_breaks(n=6))

portfolio_returns_dplyr %>% 
  ggplot(aes(x=returns)) + 
  geom_histogram(fill="cornflowerblue",binwidth = .01) +
  geom_density(alpha = 1,color="red") + 
  labs(x="Monthly returns",y = "Distribution", title = "Portfolio Histogram and Density")

#standard deviation measures the
#extent to which a portfolio’s returns are dispersed around their mean. If returns
#are more dispersed, the portfolio has a higher standard deviation and is seen
#as riskier or more volatile


portfolio_returns_dplyr %>%
  summarise(sd = round(sd(returns) * 100,2),
            sd_byHand = round( sqrt(sum( (returns - mean(returns))^2 / (nrow(.) -1) )) * 100,2))


portfolio_returns_dplyr  %>%
  ggplot(aes(x=date,y=returns)) + 
  geom_point(color="cornflowerblue") + 
  scale_x_date(breaks = pretty_breaks(n = 6)) + labs(x="Date",y="Returns",title = "Scatterplot of Returns by Date") 

sd_plot = sd(portfolio_returns_dplyr$returns)
mean_plot = mean(portfolio_returns_dplyr$returns)
median_plot = median(portfolio_returns_dplyr$returns)

portfolio_plot = portfolio_returns_dplyr %>%
  mutate(color = 
           case_when( 
             returns < mean_plot - sd_plot ~"red",
             returns > mean_plot + sd_plot ~"green",
             T ~"Darkblue"        )
         )

portfolio_plot  %>%
  ggplot(aes(x=date,y=returns)) + 
  geom_point(color=portfolio_plot$color, size=2) + 
  scale_x_date(breaks = pretty_breaks(n = 6)) + labs(x="Date",y="Returns",title = "Scatterplot of Returns by Date") +
  geom_hline(yintercept = (mean_plot + sd_plot),
             color = "purple",
             linetype = "dotted") +
  geom_hline(yintercept = (mean_plot-sd_plot),
             color = "purple",
             linetype = "dotted")

asset_returns_dplyr %>%
  group_by(asset) %>%
  summarise(sd = round(sd(return) * 100,2)) %>%
  add_row(asset="Portfolio",sd=round(sd_plot * 100,2)) %>%
  ggplot(aes(x=asset,y=sd,color=asset)) +
  geom_point(size=2) +
  labs(x="Assets",y="Standard Deviation (Risk)") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  geom_text(
    aes(x = "Portfolio",
        y =
          sd_plot * 100 + .2),
    label = "Portfolio",
    color = "cornflowerblue") 

asset_returns_dplyr %>%
  group_by(asset) %>%
  summarise(sd = round(sd(return) * 100,2),
            mean = mean(return)) %>%
  add_row(asset="Portfolio",sd=round(sd_plot * 100,2),mean = mean_plot) %>%
  ggplot(aes(y=mean,x=sd,color=asset)) +
  geom_point(size=2) +
  labs(x="Standard Deviation",y="Expected Return") +
  geom_text(
    aes(y = mean_plot + .0005,
        x = sd_plot * 100),
    label = "Portfolio",
    color = "cornflowerblue") 

window <- 24

port_rolling_sd_xts = portfolio_returns_dplyr %>% 
  column_to_rownames("date") %>% 
  as.xts() %>%
  rollapply(FUN = sd,
            width = window) %>%
  na.omit() %>%
  setNames("rolling_sd")

port_rolling_sd_tq <-
  portfolio_returns_dplyr %>%
  tq_mutate(mutate_fun = rollapply,
            width = window,
            FUN = sd,
            col_rename = "rolling_sd") %>%
  select(date, rolling_sd)  %>%
  na.omit()

port_rolling_sd_tq %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = rolling_sd), color = "cornflowerblue") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_date(breaks = pretty_breaks(n = 8)) +
  labs(title = "Rolling Standard Deviation", y = "") +
  theme(plot.title = element_text(hjust = 0.5))

skew_xts = skewness(portfolio_returns_dplyr$returns)
skew_tidy = portfolio_returns_dplyr %>% summarise(skewness = skewness(returns))


portfolio_returns_dplyr %>% 
  ggplot(aes(x=returns)) + 
  geom_histogram(fill="cornflowerblue",alpha=0.5,binwidth = 0.003) +
  scale_x_continuous(breaks = pretty_breaks(10))
  
portfolio_returns_dplyr %>% 
  ggplot(aes(x=returns)) + 
  geom_density()

portfolio_density_plot <-
  portfolio_returns_dplyr %>%
  ggplot(aes(x = returns)) +
  stat_density(geom = "line",
               alpha = 1,
               colour = "cornflowerblue")
shaded_area_data <-
  ggplot_build(portfolio_density_plot)$data[[1]] %>%
  filter(x <
           mean(portfolio_returns_dplyr$returns))

portfolio_density_plot_shaded <-
  portfolio_density_plot +
  geom_area(data = shaded_area_data,
            aes(x = x, y = y),
            fill="pink",
            alpha = 0.5)

 
rolling_skew_xts = rollapply(portfolio_returns_dplyr %>% column_to_rownames('date') %>% as.xts(),FUN=skewness,width=window) %>% na.omit()



rolling_skew_tq <-
  portfolio_returns_dplyr %>%
  tq_mutate(select = returns,
            mutate_fun = rollapply,
            width = window,
            FUN = skewness,
            col_rename = "tq") %>%
  na.omit()

# Kurtosis is a measure of the degree to which portfolio returns appear in the
# tails of their distribution. A normal distribution has a kurtosis of 3, which
# follows from the fact that a normal distribution does have some of its mass in
# its tails. A distribution with a kurtosis greater than 3 has more returns in its
# tails than the normal, and one with kurtosis less than 3 has fewer returns in its
# tails than the normal. That matters to investors because more bad returns in
# the tails means that our portfolio might be at risk of a rare but huge downside
# event. The terminology is a bit confusing because negative kurtosis actually is
# less risky because it has fewer returns in the tails.

kurt_xts <-
  kurtosis(portfolio_returns_dplyr$returns)

kurt_tidy = portfolio_returns_dplyr %>% summarise(kurt = kurtosis(returns))

sd_pos <-
  mean_plot + (2* sd(portfolio_returns_dplyr$returns))
sd_neg <-
  mean_plot - (2* sd(portfolio_returns_dplyr$returns))

sd_pos_shaded_area <-
  ggplot_build(portfolio_density_plot)$data[[1]] %>%
  filter(x > sd_pos )

sd_neg_shaded_area <-
  ggplot_build(portfolio_density_plot)$data[[1]] %>%
  filter(x < sd_neg)

portfolio_density_plot +
  geom_area(data = sd_pos_shaded_area,
            aes(x = x, y = y),
            fill="pink",
            alpha = 0.5) +
  geom_area(data = sd_neg_shaded_area,
            aes(x = x, y = y),
            fill="pink",
            alpha = 0.5) +
  scale_x_continuous(breaks = pretty_breaks(n = 10))

rfr = 0.003

portfolio_returns_dplyr %>%
  summarise(sharpe_dplyr = mean(returns - rfr)/
              sd(returns - rfr)
  )
