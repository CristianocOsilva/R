#install.packages("quadprog")
#install.packages("PerformanceAnalytics")
#install.packages("IntroCompFinR", repos="http://R-Forge.R-project.org")

library(IntroCompFinR)
library(quantmod) 
library(moments)  
library(tidyverse)
library(tidyquant)

data.inicial = as.Date("2021-04-01") 
data.final = as.Date("2021-06-12")


tickers = c("^BVSP","GGBR4.SA","USIM5.SA","VALE3.SA","PETR4.SA","BBDC4.SA","ITUB4.SA")
tickers_setor = c('Mercado','Siderugira','Siderurgia','Mineracao','Petroleo','Banco','Banco')

getSymbols(tickers, src = "yahoo", from = data.inicial, to = data.final)

BVSP_RET <- dailyReturn(BVSP)
GGBR4_RET <- dailyReturn(GGBR4.SA)
USIM5_RET <- dailyReturn(USIM5.SA)
VALE3_RET <- dailyReturn(VALE3.SA)
PETR4_RET <- dailyReturn(PETR4.SA)
BBDC4_RET <- dailyReturn(BBDC4.SA)
ITUB4_RET <- dailyReturn(ITUB4.SA)
plot(BVSP_RET)

retornos <- cbind(BVSP_RET, GGBR4_RET, USIM5_RET, VALE3_RET, PETR4_RET, BBDC4_RET, ITUB4_RET)

retornos <- na.omit(retornos)
colSums(is.na(retornos))

names(retornos) <- c("ibov", "gerdau", "usiminas", "vale", "petrobras", "bradesco", "itau")
retornos$ibov <- retornos$ibov

retorno_medio <- rbind(mean(retornos$ibov), mean(retornos$gerdau), mean(retornos$usiminas), mean(retornos$vale), mean(retornos$petrobras), mean(retornos$bradesco), mean(retornos$itau))

ativos <- c("BVSP", "GGBR4", "USIM5", "VALE3", "PETR4", "BBDC4", "ITUB4")
rownames(retorno_medio) <- ativos

retorno_medio

retornos

matriz_cov <- cov(retornos)
matriz_cov

rownames(matriz_cov) <- ativos
colnames(matriz_cov) <- ativos

tx_livre_risco <- 0.00

short_selling <- FALSE


library(IntroCompFinR)

#?tangency.portfolio

#carteira_eficiente <- tangency.portfolio(retorno_medio, matriz_cov, tx_livre_risco, shorts = short_selling)

carteira_eficiente <- tangency.portfolio(er = retorno_medio , 
                   cov.mat = matriz_cov , 
                   risk.free = 0.00 , 
                   shorts = FALSE)

carteira_eficiente$weights

ativos

# Calculo da carteira com a menor risco possível
carteira_min_risco <- globalMin.portfolio(retorno_medio, matriz_cov, shorts = short_selling)

carteira_min_risco
ativos
# compute portfolio frontier
fronteira_eficiente <- efficient.frontier(retorno_medio, matriz_cov, nport = 40, shorts = short_selling)

fronteira_eficiente

attributes(fronteira_eficiente)

# Visualizaçao da saida

plot(fronteira_eficiente, plot.assets=TRUE, col="blue", pch=16)

points(carteira_min_risco$sd, carteira_min_risco$er, col="green", pch=10, cex=2)
points(carteira_eficiente$sd, carteira_eficiente$er, col="red", pch=10, cex=2)

text(carteira_min_risco$sd, carteira_min_risco$er, labels="Risco Minimo", pos=2)
text(carteira_eficiente$sd, carteira_eficiente$er, labels="Carteira Eficiente", pos=2)

tangente <- (carteira_eficiente$er - tx_livre_risco)/carteira_eficiente$sd
abline(a = tx_livre_risco, b=tangente, col="green", lwd=2)
