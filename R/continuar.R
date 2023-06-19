tx = 0.01
(((1+ tx)^360) - 1) * 100



selic <- rbcb::get_series(11,start_date = "2020-01-01",end_date = "2023-01-01")

x = selic  %>% 
  filter(date > "2020-01-01") %>% 
  rename(`selic(a.d)` = `11`)

x %>%
  mutate(`selic(a.a)` = (((1+`selic(a.d)`)^360 )- 1) * 100)


tx=selic %>% filter(date == "2023-01-02") %>% .$`11`


tx * 100

# ao ano ao dia
(1+tx)^(1/360)-1	 

# ao dia ao ano
((1+tx)^360)-1	

# ano ao mes
(1+TAXA)^(1/12)-1	

### selic

selicCsv = read_csv2("taxa_selic_apurada.csv") %>%
  select(Data,`Taxa (% a.a.)`) %>%
  mutate(Data = as.Date(Data,"%d/%m/%Y")) %>%
  arrange(Data)


x = selicCsv %>% 
  summarise(`txMean(a.a)` = mean(`Taxa (% a.a.)`)/100) %>%
  mutate(`txMean(a.m)` = ((1+`txMean(a.a)`)^(1/12)) - 1)


tx = 7.47/100 
(1+tx)^(1/12)-1	




val = 0.050788
valFinal = 13.75
t = function(TAXA){
  TAXA=TAXA/100
  # ano ao dia
  x1=(1+TAXA)^(1/360)-1	 
  # ano ao mes
  x2=(1+TAXA)^(1/12)-1	
  
  
  #mes ao dia
  x3=((1+TAXA)^(1/30))-1	
  # mes ao ano
  x4=((1+TAXA)^12)-1	
  
  
  #
  # dia ao ano
  x5=((1+TAXA)^360)-1	
  
  # dia ao mes
  x6=((1+TAXA)^30)-1
  
  # dia ao ano, dias uteis apenas
  x7 = (1+TAXA)^252 - 1
  
  
  print(data.frame(x1,x2,x3,x4,x5,x6,x7))
  
}
t(val)



selic <- rbcb::get_series(11,start_date = "2020-01-01",end_date = "2023-01-02") %>%
  rename(txDiaria = `11` ) %>%
  mutate(txDiaria = txDiaria/100) %>%
  mutate(
         txMensal = ((1+txDiaria)^21)-1,
         txAnual = (1+txDiaria)^252 - 1)

tail(selic,1)

summarise(selic,txDiariaMean = mean(txDiaria),
          txMensalMean = mean(txMensal),
          txAnualMean = mean(txAnual)
          )

val = selic %>% mutate(anual =(1+txDiaria/100)^252 - 1 )
tail(val)
(1+ (val/100))^252 - 1 


library(timetk)
library(xts)

x = selic %>%
  rename(SELIC = `11`) %>%
  mutate(SELIC = SELIC/100) %>%
  mutate(SELIC = (1+SELIC)^252 - 1) %>%
  tk_xts() %>%
  #to.yearly(OHLC = F) %>%
  tk_tbl()

x %>% 
  summarise(mean = mean(SELIC))  %>% 
  mutate(SELICMENSAL = (1+mean)^(1/12) - 1 )




selicCsv2 = selicCsv %>% filter(Data >= "2020-01-01" & Data <= "2023-01-01") %>%
  rename(SELIC=`Taxa (% a.a.)`) %>%
  mutate(SELIC = SELIC/100)

selicCsv2 %>% 
  summarise(`txMean(a.a)` = mean(SELIC)) %>%
  mutate(`txMean(a.m)` = ((1+`txMean(a.a)`)^(1/12)) - 1)

selicApi = selic %>% 
  rename(Data=date) %>%
  filter(Data >= "2020-01-01" & Data <= "2023-01-01") %>%
  select(Data,txAnual) %>%
  mutate(txAnual = round(txAnual,4))

selicApi %>% summarise(mean = mean(txAnual))  %>% 
  mutate(SELICMENSAL = (1+mean)^(1/12) - 1 )

selicApi %>% rename(SELIC=txAnual) == selicCsv2



getSelic = function(start_date="2020-01-01",end_date="2023-01-02"){
  
  # os dados obtidos são em taxas selic diárias, sendo necessário a conversão pra taxa anual
  
  selic <- rbcb::get_series(11,start_date,end_date) %>%
    filter(date >= start_date & date <= end_date) %>%
    rename(SELIC = `11`) %>%
    mutate(SELIC = SELIC/100) %>%
    mutate(
      SELICAnual = (1+SELIC)^252 - 1)
  
  return(selic)
}

selicSummary = function(selic){
  
  selicApi = selic %>% 
    select(date,SELICAnual) %>%
    mutate(SELICAnual = round(SELICAnual,4)) %>%
    summarise(SELICAnualMedia = mean(SELICAnual))  %>% 
    mutate(SELICMENSALMedia = (1+SELICAnualMedia)^(1/12) - 1 )
  
  
  return(selicApi)
}


df = getSelic() %>% selicSummary()


df %>%
  group_by(date) %>%
  summarise(last(date))

library(timetk)
library(xts)

df %>% 
  tk_xts() %>%
  to.yearly(OHLC=F) %>%
  tk_tbl() %>%
  mutate(SELICAnual = round(SELICAnual,4)) %>%
  summarise(SELICAnualMedia = mean(SELICAnual))  %>% 
  mutate(SELICMENSALMedia = (1+SELICAnualMedia)^(1/12) - 1 )


toBibtex(citation("rbcb"))