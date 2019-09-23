
getSymbols("AMZN")
AMZN_log_returns<-AMZN%>%Ad()%>%dailyReturn(type='log')

AMZN%>%Ad()%>%chartSeries()
AMZN%>%chartSeries(TA='addBBands();addVo();addMACD()',subset='2018')


library(tidyquant)
library(Rblpapi)
blpConnect()


library(PerformanceAnalytics)
library(timetk)
amzn <- tq_get("AMZN")
amzn.xts <- tk_xts(amzn, close, date)
amzn_returns <- Return.calculate(amzn.xts)
SharpeRatio(R = amzn_returns, Rf = .02, FUN = "VaR")

# tidy way, I suppose
amzn %>%
  tq_transmute(mutate_fun = periodReturn,
               period = "daily") %>%
  tq_performance(Ra = daily.returns,
                 performance_fun = SharpeRatio,
                 Rf = .02,
                 FUN = "VaR")

# average annual return (?)
amzn %>%
  tq_transmute(mutate_fun = periodReturn,
               period = "yearly") %>%
  pull(yearly.returns) %>%
  mean()

wdc cgate atvi


library(tidyquant)
fred <- tq_get("MSFT", get = "economic.data")
