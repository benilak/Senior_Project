# devtools::install_github("krose/robinhoodr")
library(tidyquant)
library(robinhoodr)


tickers <- c("MSFT", "FB")

hd <- rh_historicals(symbols = 'AAPL', 
                     interval = "5minute", 
                     span = "day", 
                     bounds = "regular", 
                     keep_meta = FALSE, 
                     to_xts = TRUE)

tst <- rh_quote(symbols = "AAPL")


# robinhoodr is not maintained and is useless ^^
# ----------------------------------------------


# Robinhood API using Python:

library(reticulate) 
# seems like you must load, unload, then re-load reticulate before you can activate the python interpreter from the virtual environment
use_virtualenv("~/../PycharmProjects/M499/venv", required = TRUE)
py_config()
Robinhood <- import_from_path(module = "Robinhood", path = "~/../PycharmProjects/M499/venv/Lib/site-packages/")
py_module_available("Robinhood") # check to see if Robinhood is loaded

# Robinhood python as R code
trader <- Robinhood$Robinhood()
logged_in <- trader$login(username = "bregan1992@gmail.com", 
                          password = "", qr_code = ROBINHOOD_QR) # in .Renviron
# get quote
trader$quote_data("AAPL")
# wrapper for `quote_data`
trader$get_quote("AAPL")
# get last price
trader$last_trade_price("AAPL")
trader$get_quote("AAPL")$last_trade_price
trader$quote_data("AAPL")$last_trade_price

# currently no access to crypto data

# get historical data
apple_json <- trader$get_historical_quotes(stock = "AAPL", interval = "5minute", span = "day")
apple_dat <- apple_json$results[[1]]$historicals %>% bind_rows()
library(lubridate)
apple_clean <- apple_dat %>% mutate_at(2:6, as.numeric) %>%
  mutate_at(1, as_datetime)
apple_clean %>%
  ggplot() +
  geom_line(aes(x = begins_at, y = close_price))

# change bounds to extended (time spans 8:00 to 12:20)
apple_json2 <- trader$get_historical_quotes(stock = "AAPL", interval = "5minute", 
                                           span = "day", bounds = "extended")
apple_dat2 <- apple_json2$results[[1]]$historicals %>% bind_rows()
apple_clean2 <- apple_dat2 %>% mutate_at(2:6, as.numeric) %>%
  mutate_at(1, as_datetime)
apple_clean2 %>%
  ggplot() +
  geom_line(aes(x = begins_at, y = close_price))

# change span (goes back one week)
apple_json3 <- trader$get_historical_quotes(stock = "AAPL", interval = "5minute", span = "week")
apple_dat3 <- apple_json3$results[[1]]$historicals %>% bind_rows()
apple_clean3 <- apple_dat3 %>% mutate_at(2:6, as.numeric) %>%
  mutate_at(1, as_datetime)
apple_clean3 %>%
  ggplot() +
  geom_line(aes(x = begins_at, y = close_price))

# time spans from 13:30 to 19:55  (9:30AM to 4PM Eastern)

# groupon stock
groupon_json <- trader$get_historical_quotes(stock = "GRPN", interval = "5minute", 
                                             span = "day")
groupon_dat <- groupon_json$results[[1]]$historicals %>% bind_rows()
groupon_clean <- groupon_dat %>% mutate_at(2:6, as.numeric) %>%
  mutate_at(1, as_datetime)
groupon_clean %>%
  ggplot() +
  geom_line(aes(x = begins_at, y = close_price))










