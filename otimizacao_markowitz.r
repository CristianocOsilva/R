install.packages("quadprog")
install.packages("PerformanceAnalytics")
install.packages("IntroCompFinR", repos="http://R-Forge.R-project.org")

library(IntroCompFinR)
library(quantmod) 
library(moments)  
library(tidyverse)
library(tidyquant)

itau_ret <- c("ITUB4.SA") %>%
  tq_get(get = "stock.prices",
         from = "2016-01-01",
         to = "2021-06-04",) %>%
  group_by(symbol) %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = "annually",
               col_rename = "Ra")

b2w_ret <- c("BTOW3.SA") %>%
  tq_get(get = "stock.prices",
         from = "2016-01-01",
         to = "2021-06-04",) %>%
  group_by(symbol) %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = "annually",
               col_rename = "Ra")

marfrig_ret <- c("MRFG3.SA") %>%
  tq_get(get = "stock.prices",
         from = "2016-01-01",
         to = "2021-06-04",) %>%
  group_by(symbol) %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = "annually",
               col_rename = "Ra")

gerdau_ret <- c("GGBR4.SA") %>%
  tq_get(get = "stock.prices",
         from = "2016-01-01",
         to = "2021-06-04",) %>%
  group_by(symbol) %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = "annually",
               col_rename = "Ra")

localiza_ret <- c("RENT3.SA") %>%
  tq_get(get = "stock.prices",
         from = "2016-01-01",
         to = "2021-06-04",) %>%
  group_by(symbol) %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = "annually",
               col_rename = "Ra")


retornos <- cbind(itau_ret, b2w_ret, marfrig_ret, gerdau_ret, localiza_ret)
retornos[,3]
mean(retornos[[3]])

retorno_medio <- rbind(mean(retornos[[3]]), mean(retornos[[6]]), mean(retornos[[9]]), mean(retornos[[12]]), mean(retornos[[15]]))

ativos <- c("ITUB4", "BTOW3", "MRFG3", "GGBR4", "RENT3")
rownames(retorno_medio) <- ativos

retorno_medio

retornos

retornos <- retornos[c(1:6), c(3,6,9,12,15)]
retornos


matriz_cov <- cov(retornos)
matriz_cov

rownames(matriz_cov) <- ativos
colnames(matriz_cov) <- ativos

matriz_cov

tx_livre_risco <- 0.035

short_selling <- FALSE

# Vamos calcular a nossa carteira mais eficiente - chamado de Tangency Portfolio

# Carteira Eficience
library(IntroCompFinR)
?tangency.portfolio
carteira_eficiente <- tangency.portfolio(er = retorno_medio,
                                         cov.mat = matriz_cov,
                                         risk.free = tx_livre_risco,
                                         shorts = short_selling)

carteira_eficiente

ativos

# Calculo da carteira com a menor risco possível
carteira_min_risco <- globalMin.portfolio(retorno_medio, matriz_cov, shorts = short_selling)

carteira_min_risco

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
