library(tidyquant) 
library(plotly) 
library(timetk) 


### https://www.codingfinance.com/post/2018-05-31-portfolio-opt-in-r/

symbols = c("BTC-USD","ETH-USD","ADA-USD")

price_data  = tq_get(symbols, from="2018-01-01",
       to="2023-01-01")

log_ret_tidy <- price_data %>%
  group_by(symbol) %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = 'daily',
               col_rename = 'ret',
               type = 'log')

log_ret_tidy %>% pivot_wider(names_from = "symbol",values_from = "ret") %>% head()


log_ret_xts <- log_ret_tidy %>%
  pivot_wider(names_from =  symbol, values_from  = ret) %>%
  tk_xts

head(log_ret_xts)

mean_ret  = colMeans(log_ret_xts)
print(round(mean_ret, 5))


cov_mat <- cov(log_ret_xts) * 252

print(round(cov_mat,4))

wts <- runif(n = length(symbols))
print(sum(wts))
wts <- wts/sum(wts)
print(wts)
sum(wts)

port_returns <- (sum(wts * mean_ret) + 1)^252 - 1

port_risk <- sqrt(t(wts) %*% (cov_mat %*% wts))
print(port_risk)

# Since Risk free rate is 0% 

sharpe_ratio <- port_returns/port_risk
print(sharpe_ratio)


num_port <- 5000

# Creating a matrix to store the weights

all_wts <- matrix(nrow = num_port,
                  ncol = length(symbols))

# Creating an empty vector to store
# Portfolio returns

port_returns <- vector('numeric', length = num_port)

# Creating an empty vector to store
# Portfolio Standard deviation

port_risk <- vector('numeric', length = num_port)

# Creating an empty vector to store
# Portfolio Sharpe Ratio

sharpe_ratio <- vector('numeric', length = num_port)


for (i in seq_along(port_returns)) {
  
  wts <- runif(length(symbols))
  wts <- wts/sum(wts)
  
  # Storing weight in the matrix
  all_wts[i,] <- wts
  
  # Portfolio returns
  
  port_ret <- sum(wts * mean_ret)
  port_ret <- ((port_ret + 1)^252) - 1
  
  # Storing Portfolio Returns values
  port_returns[i] <- port_ret
  
  
  # Creating and storing portfolio risk
  port_sd <- sqrt(t(wts) %*% (cov_mat  %*% wts))
  port_risk[i] <- port_sd
  
  # Creating and storing Portfolio Sharpe Ratios
  # Assuming 0% Risk free rate
  
  sr <- port_ret/port_sd
  sharpe_ratio[i] <- sr
  
}

portfolio_values <- tibble(Return = port_returns,
                           Risk = port_risk,
                           SharpeRatio = sharpe_ratio)

all_wts <- tk_tbl(all_wts)
colnames(all_wts) <- colnames(log_ret_xts)
portfolio_values <- tk_tbl(cbind(all_wts, portfolio_values))
head(portfolio_values)


min_var <- portfolio_values[which.min(portfolio_values$Risk),]
max_sr <- portfolio_values[which.max(portfolio_values$SharpeRatio),]

p <- min_var %>%
  pivot_longer(cols= symbols[1]:symbols[length(symbols)], names_to = "Asset",
         values_to = "Weights") %>%
  mutate(Asset = as.factor(Asset)) %>%
  ggplot(aes(x = fct_reorder(Asset,Weights), y = Weights, fill = Asset)) +
  geom_bar(stat = 'identity') +
  theme_minimal() +
  labs(x = 'Assets', y = 'Weights', title = "Minimum Variance Portfolio Weights") +
  scale_y_continuous(labels = scales::percent) 

ggplotly(p)


p <- max_sr %>%
  pivot_longer(cols= symbols[1]:symbols[length(symbols)], names_to = "Asset",
               values_to = "Weights") %>%
  mutate(Asset = as.factor(Asset)) %>%
  ggplot(aes(x = fct_reorder(Asset,Weights), y = Weights, fill = Asset)) +
  geom_bar(stat = 'identity') +
  theme_minimal() +
  labs(x = 'Assets', y = 'Weights', title = "Minimum Variance Portfolio Weights") +
  scale_y_continuous(labels = scales::percent) 

ggplotly(p)


p <- portfolio_values %>%
  ggplot(aes(x = Risk, y = Return, color = SharpeRatio)) +
  geom_point() +
  theme_classic() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) +
  labs(x = 'Annualized Risk',
       y = 'Annualized Returns',
       title = "Portfolio Optimization & Efficient Frontier") +
  geom_point(aes(x = Risk,
                 y = Return), data = min_var, color = 'red') +
  geom_point(aes(x = Risk,
                 y = Return), data = max_sr, color = 'red')


ggplotly(p)
