
port <- c("SBUX", "S", "CPRX", "LADR", "MAIN", "KEY", "NOBL", "MSFT")

exchanges <- rbind(AMEX, NYSE, NASDAQ)

df <- tibble(symbol = port, 
             sector = exchanges$sector[match(port, exchanges$symbol)],
             industry = exchanges$industry[match(port, exchanges$symbol)])

MAIN_ind <- get_industry("MAIN", get = "names")
df[5, 2:3] <- MAIN_ind[1, 2:3]


# rank symbols by 5Y Ann. Yield
HUN <- tq_get("HUN")

HUN %>%
  filter(between(date, today()-years(10), today())) %>%
  tq_transmute(select = adjusted, # gets monthly returns
               mutate_fun = periodReturn, 
               period     = "monthly") %>%
  # annualizes
  tq_performance(Ra = monthly.returns, performance_fun = table.AnnualizedReturns)


tmp <- c('WFC',
'BLK',
'BBT',
'CCL',
'PII',
'WSM',
'HUN',
'LAZ',
'BOH',
'CHFC',
'CZFS',
'LARK')

tibble(symbol = tmp, 
       sector = exchanges$sector[match(tmp, exchanges$symbol)],
       industry = exchanges$industry[match(tmp, exchanges$symbol)])

tmp <- "HRB"

