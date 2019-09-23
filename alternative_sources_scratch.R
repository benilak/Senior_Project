
# TIDYQUANT
library(tidyquant)
SPY <- tq_get("SPY", from = "2006-01-01")
SPY %>%
  ggplot() +
  geom_line(aes(x = date, y = adjusted))
SP500 <- tq_index("SP500")
# last trade price - DISCONTINUED
tq_get("AAPL", get = "key.stats")



# ALPHA VANTAGER w/ TIDYQUANT
library(alphavantager)
# works with tidyquant
av_api_key(Sys.getenv("AV_API_KEY"))
my_intraday_data <- c("FB", "MSFT") %>%
  tq_get(get = "alphavantager", av_fun = "TIME_SERIES_INTRADAY", interval = "5min")
# get quote from AV via tidyquant
quote <- tq_get("AAPL", get = "alphavantager", av_fun = "GLOBAL_QUOTE")
av_get(symbol = "AAPL", av_fun = "GLOBAL_QUOTE") # same as above
# last price
quote$price

# get quote directly from ALPHA VANTAGER API:
# "Thank you for using Alpha Vantage! Our standard API call frequency is 5 calls per minute and 500 calls per day. Please visit https://www.alphavantage.co/premium/ if you would like to target a higher API call frequency."
# retreive as json
library(jsonlite)
quote.json <- fromJSON("https://www.alphavantage.co/query?function=GLOBAL_QUOTE&symbol=AAPL&apikey=")
# last price
quote.json[[1]][[5]] %>% as.numeric()
# raw data
quote.raw <- httr::GET("https://www.alphavantage.co/query?function=GLOBAL_QUOTE&symbol=AAPL&apikey=")

# get crypto quote: throwing errors for some reason
tq_get("BTC", get = "alphavantager", av_fun = "CURRENCY_EXCHANGE_RATE", 
       from_currency = "BTC", to_currency = "USD")
av_get(symbol = NULL, av_fun = "CURRENCY_EXCHANGE_RATE", 
       from_currency = "BTC", to_currency = "USD")

# get crypto quote directly from AV API
crypto.json <- fromJSON("https://www.alphavantage.co/query?function=CURRENCY_EXCHANGE_RATE&from_currency=BTC&to_currency=USD&apikey=")
crypto.dat <- crypto.json[[1]] %>% as.tibble()
# get crypto price
fromJSON(paste("https://www.alphavantage.co/query?function=CURRENCY_EXCHANGE_RATE&from_currency=BTC&to_currency=USD&apikey=", Sys.getenv("AV_API_KEY"), sep="")) %>% 
  .[[1]] %>% 
  .[[5]] %>% 
  as.numeric()



# IEX (iexcloudR)
library(iexcloudR)
res <- iexRaw("/stock/AAPL/quote")
historyFor("AAPL")
topsLast("AAPL")
priceOf("AAPL") # same as topsLast("AAPL")$price
# crypto currently unavailable w/ iexcloud
priceOf("BTC")




# R wrapper for Coinbase (GDAX) API
library(rgdax)

# get quote
public_daystats("BTC-USD")
# get historical data
btc.gdax <- public_candles("BTC-USD", start = "2019-05-15", granularity = 60)

# quandl historical data - only daily available
quandl_api_key(Sys.getenv("QUANDL_API_KEY"))
tq_get(x = 'BITSTAMP/USD', get = "quandl")
