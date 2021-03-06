install.packages("ggplot2")
install.packages("plyr")
install.packages("dplyr")
install.packages("BatchGetSymbols")
install.packages("quantmod")
install.packages("GetDFPData")
install.packages("ggthemes")

library(BatchGetSymbols)
library(quantmod)
library(GetDFPData)
library(ggplot2)
library(ggthemes)
library(reshape2)
library(plyr)

# iniciando a filtragem dos dados

acao = 'WEGE3.SA'
di = '2016-01-01'
df = Sys.Date()
benchmark = '^BVSP'


dadosDf = BatchGetSymbols(
  tickers = acao,
  first.date = di,
  last.date = df,
  bench.ticker = benchmark,
)
dadosDf = dadosDf$df.tickers


p = ggplot(dadosDf, aes(ref.date, price.adjusted) ) + geom_line(color = 'blue')
p + labs(x = "Data", y = "Pre�o Ajustado", title = "Varia��o do Pre�o", subtitle = "De 01/01/2016 a 03/06/2021" )

ibov = GetIbovStocks()

ibov_tickers= paste(ibov$tickers, ".SA", sep ='')

dadosDf_ibov = BatchGetSymbols(
  tickers = ibov_tickers,
  first.date = di,
  last.date = df,
  bench.ticker = benchmark,
)
dadosDf_ibov = dadosDf_ibov$df.tickers

dadosDf_ibov2 = dlply(dadosDf_ibov, .(ticker), function(x) {rownames(x) = x$row; x$row = NULL; x})

papel = dadosDf_ibov2[[1]][,c(7,6)]
colnames(papel) = c("Data", paste("Pre�os",dadosDf_ibov2[[1]][1,8]))

for (i in 2:75){
  
  novo_papel = dadosDf_ibov2[[i]][,c(7,6)]
  colnames(novo_papel) = c("Data", paste("Pre�os",dadosDf_ibov2[[i]][1,8]))
  papel = merge(papel, novo_papel, by = "Data")
  
}

#gerando gr�fico com v�rias a��es

f = ggplot()+
  geom_line(data = papel, aes(x = Data, y = papel$`Pre�os BBAS3.SA`, color = "Banco do Brasil"))+
  geom_line(data = papel, aes(x = Data, y = papel$`Pre�os BBDC4.SA`, color = "Bradesco"))+
  geom_line(data = papel, aes(x = Data, y = papel$`Pre�os ITUB4.SA`, color = "Itau"))+
  geom_line(data = papel, aes(x = Data, y = papel$`Pre�os SANB11.SA`, color = "Santander"))+
  
  xlab("Data")+
  ylab("Pre�o")

f$labels$colour = "Bancos"

print(f)

#Normaliza��o dos pre�os das a��es

IBOV = BatchGetSymbols(
  tickers = '^BVSP',
  first.date = di,
  last.date = df,
  bench.ticker = benchmark,
)
IBOV = IBOV$df.tickers

colnames(IBOV)[6] = "IBOV"
colnames(IBOV)[7] = "Data"

IBOV = IBOV[,c(7,6)]
SP500 = SP500[,c(7,6)]

SP500 = BatchGetSymbols(
  tickers = '^GSPC',
  first.date = di,
  last.date = df,
  bench.ticker = '^GSPC',
)

colnames(SP500)[6] = "SP500"
colnames(SP500)[7] = "Data"

SP500 = SP500$df.tickers

ibov_sp500 = merge(IBOV, SP500, by = "Data")

total = merge(ibov_sp500, papel, by = "Data")

normalizado = total[, -c(1)]

novo_total = data.frame(lapply(normalizado, function(x) x/x[1]))

novo_total$Data = total$Data

g = ggplot()+
  geom_line(data = novo_total, aes(x = Data, y = novo_total$Pre�os.BBAS3.SA, color = "Banco do Brasil"))+
  geom_line(data = novo_total, aes(x = Data, y = novo_total$Pre�os.BBDC4.SA, color = "Bradesco"))+
  geom_line(data = novo_total, aes(x = Data, y = novo_total$Pre�os.ITUB4.SA, color = "Itau"))+
  geom_line(data = novo_total, aes(x = Data, y = novo_total$Pre�os.SANB11.SA, color = "Santander"))+
  geom_line(data = novo_total, aes(x = Data, y = novo_total$IBOV, color = "IBOV"))+
  geom_line(data = novo_total, aes(x = Data, y = novo_total$SP500, color = "S&P500"))+
  
  xlab("Data")+
  ylab("Pre�o")

g$labels$colour = "Bancos"

print(g)

# utilizando a biblioteca quantmode

library(quantmod)
library(ggplot2)
library(BatchGetSymbols)

dados_BB = getSymbols(Symbols = "BBAS3.SA", from = '2020-01-01', src = 'yahoo', auto.assign = FALSE)

chart_Series(dados_BB)

# EXPLICAR O SIGNIFICADO ...

ggplot(dados_BB, aes(index(dados_BB), dados_BB[,6])) + geom_line(color = 'darkblue') 

dados_BB_filtrado = subset(dados_BB, index(dados_BB)>= '2020-02-20')

MM_BB_10 = rollmean(dados_BB_filtrado[,6], 10, fill = list(NA, NULL, NA), align = 'right')
MM_BB_30 = rollmean(dados_BB_filtrado[,6], 30, fill = list(NA, NULL, NA), align = 'right')  

dados_BB_filtrado$MM_BB_10 = MM_BB_10
dados_BB_filtrado$MM_BB_30 = MM_BB_30

m =ggplot(dados_BB_filtrado, aes(index(dados_BB_filtrado)))+
  geom_line(aes(y=dados_BB_filtrado[,6], color = "Pre�o de BBAS3"))+
  geom_line(aes(y=dados_BB_filtrado$MM_BB_10, color = "M�dia m�vel de 10 per�odos"))+
  geom_line(aes(y=dados_BB_filtrado$MM_BB_30, color = "M�dia m�vel de 30 per�odos"))+
  xlab("Data") + ylab("Pre�o")
  
print(m)

dailyReturn(dados_BB_filtrado)
weeklyReturn(dados_BB_filtrado)
monthlyReturn(dados_BB_filtrado)
yearlyReturn(dados_BB_filtrado)
sd(na.omit(dados_BB_filtrado$BBAS3.SA.Adjusted))

# trabalhando com dados fundamentalistas
library(GetDFPData)

empresas = gdfpd.get.info.companies()
company = "RUMO MALHA PAULISTA S.A."
di = '2019-01-01'
df = '2020-01-01'
type.export = "xlsx"

dados_fund_rail3 = gdfpd.GetDFPData(name.companies = company,
                 first.date = di,
                 last.date = df,
                 )

fluxodecaixa = dados_fund_rail3[[17]]
fluxodecaixa = data.frame(fluxodecaixa[[1]])

# calculo de correla��o em a��es
install.packages("corrplot")

library(corrplot)

# normalizado = total[, -c(1)]

papelSemData = papel[, -c(1)]

tabela = papelSemData[,colnames(papelSemData) %in% c("Pre�os WEGE3.SA", "Pre�os ITUB4.SA", "Pre�os MGLU3.SA", "Pre�os ABEV3.SA", "Pre�os B3SA3.SA")]
correlacoes = cor(tabela, use = "complete.obs", method = 'spearman')

k= corrplot(correlacoes, number.cex = 1, number.font = 1, method = "number", type = "lower")
print(k)


