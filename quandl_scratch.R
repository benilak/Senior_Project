library(Quandl)
library(tidyquant)

# Quandl
Quandl.api_key("")
metaData("AAPL")
Quandl.search("AAPL")

AAPL <- Quandl("XNAS/AAPL") # goes back to 2006-12-28

ggplot(AAPL) +
  geom_line(aes(x = Date, y = Close))


# tidyquant
AAPL.tq <- tq_get("AAPL") # defaults to 10 years

AAPL.tq %>%
  ggplot() +
  geom_line(aes(x = date, y = close))

# get Quandl data using tidyquant
quandl_api_key("")
NASDAQ100_set_val <- tq_get(x = "NASDAQOMX/XQC", get = "quandl")




