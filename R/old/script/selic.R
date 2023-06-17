library(tidyverse)
library(scales)
options(scipen = 999)

df = read_csv2("taxa_selic_apurada.csv") %>%
  mutate(Data=as.Date(Data,format="%d/%m/%Y")) %>%
  mutate_at(vars(2:ncol(.)), ~str_remove_all(.x,"\\.")) %>%
  mutate_at(vars(2:ncol(.)), ~str_replace_all(.x,"\\,","\\.") %>% as.numeric)
  
xts_selic = xts(x = df$`Financeiro (R$)`,order.by =  df$Data)
xts_selic_return = Return.calculate(xts_selic,method = "log") %>% 
  na.omit()

xts_selic_return %>% 
  as.data.frame(row.names = index(.)) %>%
  rownames_to_column("date") %>%
  mutate(date=as.Date(date)) %>%
  rename(returns = V1) %>%
  ggplot(aes(x=date,y=returns)) +
  geom_line()

# dfCompleto = df %>%
#   select(Data,`Financeiro (R$)`) %>%
#   rename(valor=`Financeiro (R$)`) %>%
#   arrange(Data) %>%
#   mutate(return = log(valor) - log(lag(valor))) %>%
#   na.omit()


dfCompleto %>% ggplot(aes(x=Data,y=return)) +
  geom_line() +
  scale_x_continuous(breaks = pretty_breaks(6))

dfCompleto$Data
dfCompleto %>% 
  filter(Data < "2021-01-01") %>%
  ggplot(aes(x=Data,y=return)) +
  geom_point()



