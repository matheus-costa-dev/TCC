source("functions/selic.R")

options(scipen=999)

# geral 
from = "2020-01-01"
to = "2023-01-01"
symbols = c("BTC-USD","ETH-USD","BNB-USD","XRP-USD","ADA-USD","DOGE-USD","SOL-USD","MATIC-USD")
Fsymbols = c("Bitcoin","Ethereum","Binance","XRP","Cardano","Dogecoin","Solana","Polygon")
Nsymbols = length(symbols)
folder = "outputData"
by = .1

## selic

selic = loadSelicSummary(folder,from,to,businessDay=T)
rfr = selic$SELICDiariaMédia
selicAnualMedia = selic$SELICAnualMedia

# todas possibilidades de construção de portfolio

filePossibilites = paste0("Possibilities",rfr,".xlsx")
fullPathPossibilites = paste(folder,filePossibilites,sep = "/")