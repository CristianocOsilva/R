library("quantmod") 
install.packages("moments")
library(moments)  # to get skew & kurtosis

#Seleção do período de análise
startDate = as.Date("2018-01-01") 
endDate = as.Date("2021-06-06")

#Seleção das ações

tickers <- c("^BVSP","ALUP11.SA", "BBDC4.SA", "ITUB4.SA", "MGLU3.SA", "NTCO3.SA", "PETR4.SA", "RDOR3.SA","SIMH3.SA","TUPY3.SA", "VALE3.SA")
#tickers = c('^BVSP','GGBR4.SA','USIM5.SA','VALE5.SA','PETR4.SA','BBDC4.SA','ITUB4.SA')
#tickers_setor = c('Mercado','Siderugira','Siderurgia','Mineracao','Petroleo','Banco','Banco')

getSymbols(tickers, src = "yahoo", from = startDate, to = endDate)

BVSP_RET <- dailyReturn(BVSP)
ALUP11_RET <- dailyReturn(ALUP11.SA)
BBDC4_RET <- dailyReturn(BBDC4.SA)
ITUB4_RET <- dailyReturn(ITUB4.SA)
MGLU3_RET <- dailyReturn(MGLU3.SA)
NTCO3_RET <- dailyReturn(NTCO3.SA)
PETR_RET <- dailyReturn(PETR4.SA)
RDOR3_RET <- dailyReturn(RDOR3.SA)
SIMH3_RET <- dailyReturn(SIMH3.SA)
TUPY3_RET <- dailyReturn(TUPY3.SA)
VALE3_RET <- dailyReturn(VALE3.SA)
plot(BVSP_RET)

#ALUP11_RET,BBDC4_RET,ITUB4_RET,MGLU3_RET,NTCO3_RET,PETR_RET,RDOR3_RET,SIMH3_RET,TUPY3_RET,VALE3_RET

cor(ALUP11_RET)
hist(ALUP11_RET)

#install.packages("nortest")
#library(nortest)

shapiro.test(as.numeric(ALUP11_RET))
#ad.test(GGBR4_RET)
qqnorm(ALUP11_RET)
qqline(ALUP11_RET, col = 2)

statNames <- c("Media", "Desvio", "Inclinacao", "Curtose")
ALUP11_STATS <- c(mean(ALUP11_RET), sd(ALUP11_RET), skewness(ALUP11_RET), kurtosis(ALUP11_RET))
names(ALUP11_RET) <- statNames
ALUP11_STATS

grupo_ret <- cbind(BVSP_RET,ALUP11_RET,BBDC4_RET,ITUB4_RET,MGLU3_RET,NTCO3_RET,PETR_RET,RDOR3_RET,SIMH3_RET,TUPY3_RET,VALE3_RET)
PCA <- prcomp(grupo_ret,center = TRUE, scale. = TRUE)
plot(PCA)
summary(PCA)
PCA_VAR_EXPLAINED = PCA$sdev^2 / sum(PCA$sdev^2)
barplot(100*PCA_VAR_EXPLAINED, las=2, xlab='', ylab='% Variância Explicada', main = "Variância por PC")

p = princomp(na.omit(grupo_ret))
loadings = p$loadings[]
PC1 = loadings[,1]
PC2 = loadings[,2]

# PC1 em função de PC2 para entender a relação dos retornos diários
plot(PC1, PC2, type='p', pch=20, 
     xlab='Componente 1', 
     ylab='Componente 2', 
     ylim = c(-0.75,0.45), 
     xlim = c(-0.7,-0.1),
     main = 'PCA para o IBovespa')
text(PC1, PC2, tickers, cex=.7, pos=4)