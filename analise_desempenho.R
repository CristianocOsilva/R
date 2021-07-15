install.packages("tidyverse")
install.packages("tidyquant")

library(tidyverse)
library(tidyquant)

?tidyquant
?tq_transmute
?tq_transmute_fun_options()
tq_get_options()

amar3_ret <- c("AMAR3.SA") %>%
  tq_get(get = "stock.prices",
         from = "2016-01-01",
         to = "2021-06-04",) %>%
  group_by(symbol) %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = "annually",
               col_rename = "Ra")
Ra

Rb = "XLK" %>%
  tq_get(get = "stock.prices",
         from = "2018-01-01",
         to = "2021-06-04",) %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = "monthly",
               col_rename = "Rb") 
Rb

RaRb <- left_join(Ra, Rb, by = c("date" = "date"))
RaRb

RaRb %>%
  tq_performance( Ra = Ra,
                  Rb = NULL,
                  performance_fun = SharpeRatio,
                  p = 0.95)
#RaRb %>%
#  tq_performance( Ra = Ra,
#                  Rb = Rb,
#                  performance_fun = table.CAPM)

RaRb %>%
  tq_performance( Ra = Ra,
                  Rb = NULL,
                  performance_fun = SharpeRatio,
                  Rf = 0,03 / 12,
                  p = 0.99)

?tq_performance_fun_options

# multiplos portfolios

stock_returns_monthly <- c("PETR4.SA", "MGLU3.SA", "ITUB4.SA") %>%
  tq_get(get = "stock.prices", 
         from = "2016-01-01",
         to = "2021-06-04",) %>%
  group_by(symbol) %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = "monthly",
               col_rename = "Ra")

baseline_returns_monthly = "XLK" %>%
  tq_get(get = "stock.prices",
         from = "2016-01-01",
         to = "2021-06-04",) %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = "monthly",
               col_rename = "Rb") 

wts <- c(0.5, 0.0, 0.5)
portfolio_returns_mounthly <- stock_returns_monthly %>%
  tq_portfolio(assets_col = symbol,
               returns_col = Ra,
               weights = wts,
               col_rename = "Ra")
portfolio_returns_mounthly

RaRb_single_portfolio <- left_join(portfolio_returns_mounthly,
                                   baseline_returns_monthly,
                                   by = "date")
RaRb_single_portfolio

RaRb_single_portfolio %>%
  tq_performance(Ra = Ra, Rb = NULL, performance_fun = table.AnnualizedReturns)

library(ggplot2)

portfolio_returns_mounthly %>%
  ggplot(aes(x = date, y = Ra)) + 
  geom_bar(stat = "identity", fill = palette_light()[[1]]) +
  labs(title = "Retorno de Portfólio",
       subtitle = "50% PETR4, 0% MGLU3, e 50% ITUB4",
       caption = "Shows an above-zero trend meaning positive returns",
       x = "", y = "Retornos Mensais") +
  geom_smooth(method = "lm") +
  theme_tq() +
  scale_color_tq() +
  scale_y_continuous(labels = scales::percent)

