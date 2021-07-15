library(quantmod)

tickers <- c("GGBR4.SA", "MRFG3.SA", "TUPY3.SA", "BTOW3.SA", "VALE3.SA")
start.date <- as.Date("2018-01-01")
end.date <- as.Date("2021-06-11")

getSymbols(tickers, src = "yahoo", from = start.date, to = end.date)

GERDAU <- data.frame(GGBR4.SA)
MARFRIG <- data.frame(MRFG3.SA)
TUPY <- data.frame(TUPY3.SA)
B2W <- data.frame(BTOW3.SA)
VALE <- data.frame(VALE3.SA)

GERDAU <- na.omit(GERDAU)
MARFRIG <- na.omit(MARFRIG)
TUPY <- na.omit(TUPY)
B2W <- na.omit(B2W)
VALE <- na.omit(VALE)

?manipulate

#chartSeries(GGBR4.SA)
chartSeries(to.monthly(GGBR4.SA), up.col = "green", dn.col = "white")

#install.packages("TTR")

library(TTR)

#chartSeries(MRFG3.SA, TA = NULL)
#addMACD()
#addBBands()
#addCCI()

macd <- MACD(GGBR4.SA$GGBR4.SA.Adjusted, nFast = 12, nSlow = 26, nSig = 9, maType = SMA, percent = T)
trade.rule <- lag(ifelse(macd$macd < macd$signal, -1, 1))

retornos <- ROC(GGBR4.SA$GGBR4.SA.Adjusted)*trade.rule
retornos <- retornos["2019-01-01/2021-06-10"]

carteira <- exp(cumsum(retornos$GGBR4.SA.Adjusted))-1
plot(carteira)

# AVALIAÇÃO DA CARTEIRA

library(PerformanceAnalytics)
table.Drawdowns(retornos, top = 10)
table.DownsideRisk(retornos)
charts.PerformanceSummary(retornos)