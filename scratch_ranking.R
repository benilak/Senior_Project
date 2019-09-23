
source("get_industry.R", encoding = "utf-8") # to read trademark symbol (R) in webpage
source("get_stockrow.R")
source("get_keystats.R")
library(ggrepel)


ind_compare <- get_industry(c("WDC", "MSFT", "AAPL"), get = "compare")
ind_names <- get_industry(c("WDC", "MSFT", "AAPL"), get = "names")

### scratch junk, keep for reference (used to update get_industry.R) ###
# compare table in tidy format
WDC_compare %>%
  rename(Percentile = `Percentilein Industry`) %>%
  gather(`Industry Average`, `Percentile`, `Value`, key = Key2, value = Value) %>%
  spread(Key, value = Value) %>% 
  rename(Key = Key2) %>% 
  slice(3,1,2) %>%
  View()
# compare table in tidy for multiple tickers (ignores row order, no slicing)
ind_compare %>% View()
ind_compare %>% 
  gather(key = "Symbol", value = "Value", symbol) %>%
  select(Symbol, everything()) %>%
  filter(!is.na(Value)) %>%
  rename(Percentile = `Percentilein Industry`) %>%
  gather(`Industry Average`, `Percentile`, `Value`, key = Key2, value = Value) %>% 
  spread(Key, value = Value) %>%
  rename(Key = Key2) %>% 
  View()
###




# rank companies based on PE ratio ~relative to industry~
ind_compare %>% 
  filter(Key == "Percentile") %>%
  mutate(PE = parse_number(`P/E (TTM)`),
         rank = rank(PE)) %>%
  select(Symbol, PE, rank)



# get annualized return (5 years, monthly)
AAPL <- tq_get("AAPL")
AAPL %>%
  filter(between(date, today()-years(5), today())) %>%
  tq_transmute(select = adjusted, # gets monthly returns
               mutate_fun = periodReturn, 
               period     = "monthly") %>% 
  # annualizes
  tq_performance(Ra = monthly.returns, Rb = NULL, performance_fun = table.AnnualizedReturns)

# get ann. returns for mulitple stocks (maybe inaccurate?)
mult_stocks <- tq_get(c("TJX", "AAPL", "NOBL", "TSLA", "SPY"))
mult_stocks %>%
  filter(between(date, today()-years(5), today())) %>%
  group_by(symbol) %>%
  tq_transmute(select = adjusted, # gets monthly returns
               mutate_fun = periodReturn, 
               period     = "monthly") %>% 
  # annualizes
  tq_performance(Ra = monthly.returns, performance_fun = table.AnnualizedReturns)

# all stocks in each exchange, includes sector + industry
# (industry title does not match get_industry exactly)
NYSE <- tq_exchange("NYSE")
NYSE %>%
  filter(symbol == "TJX") %>%
  .$industry
NASDAQ <- tq_exchange("NASDAQ")
NYSE %>%
  filter(symbol == "SPY")
AMEX <- tq_exchange("AMEX")

# get dividends for a stock
tq_get("AAPL", get = "dividends")

# get random ticker symbols
SP1000 <- tq_index("SP1000")
symbols <- SP1000 %>%
  pull(symbol) %>%
  sample(10)


# rank symbols by 5Y Ann. Yield
stock_dat <- tq_get(symbols)

stock_annualized <- stock_dat %>%
  filter(between(date, today()-years(5), today())) %>%
  group_by(symbol) %>%
  tq_transmute(select = adjusted, # gets monthly returns
               mutate_fun = periodReturn, 
               period     = "monthly") %>%
  # annualizes
  tq_performance(Ra = monthly.returns, performance_fun = table.AnnualizedReturns) %>%
  ungroup() %>%
  mutate(rankReturn = rank(-AnnualizedReturn),
         rankSD = rank(AnnualizedStdDev))

# rank PE and dividend yield
industry_dat <- get_industry(symbols, get = "compare")
stock_stats <- industry_dat %>% 
  filter(Key == "Percentile") %>%
  mutate(percentileDividend = parse_number(`Dividend Yield (Annualized)`),
         percentilePE = parse_number(`P/E (TTM)`),
         percentileEPSgrowth = parse_number(`EPS Growth (TTM vs Prior TTM)`),
         rankPE = rank(percentilePE),
         rankDividend = rank(-percentileDividend, na.last = "keep"),
         rankEPSgrowth = rank(-percentileEPSgrowth)) %>%
  rename(symbol = Symbol) %>%
  select(symbol, percentilePE, percentileDividend, percentileEPSgrowth, 
         rankPE, rankDividend, rankEPSgrowth)

# final ranks
wghts <- c(rankDividend = 2.5, rankEPSgrowth = 2, rankPE = 10, rankReturn = 4.5, rankSD = 2)
ranks <- full_join(stock_annualized, stock_stats, by = "symbol") %>%
  gather(names(wghts), key = "metric", value = "rank") %>%
  mutate(metric = factor(metric, levels = names(wghts))) %>%
  select(-`AnnualizedSharpe(Rf=0%)`)
ranks_final <- ranks %>%
  group_by(symbol) %>%
  arrange(metric) %>% # must arrange by metric, which must be a factor ordered by weight names
  summarise(final = weighted.mean(rank, wghts, na.rm = TRUE)) %>%
  mutate(final = rank(final))

ranks_final2 <- ranks %>%
  group_by(symbol) %>%
  arrange(metric) %>% # must arrange by metric, which must be a factor ordered by weight names
  summarise(final = rank*wghts) %>%
  mutate(final = rank(final))

# parallel plots (move NA populated ranks to edge of plot)
library(directlabels)
ranks %>%
  ggplot(aes(x = metric, y = rank, color = symbol, group = symbol)) +
  geom_point(size = 2) +
  geom_path(size = 1) +
  scale_x_discrete(expand = c(0,1)) +
  scale_y_reverse(breaks = 1:10) +
  labs(title = "Parallel Plot of metrics", subtitle = "ranks as 1st, 2nd, etc...",
       y = "Rank", x = "") +
  theme_tq() +
  theme(legend.position = "none",
        panel.grid.major.y = element_line(linetype = "dashed")) +
  geom_dl(aes(label = symbol), method = list(dl.trans(x=x+0.5), "last.points"))

# risk vs reward
stock_annualized %>%
  ggplot(aes(x = AnnualizedStdDev, y = AnnualizedReturn)) +
  geom_point() +
  geom_label_repel(aes(label = symbol, color = symbol)) +
  theme_tq() +
  theme(legend.position = "none",
        panel.grid.minor = element_line(linetype = "dashed")) +
  labs(title = "Risk vs. Reward", x = "Standard Deviation of Returns", y = "Annualized Return")

# ranks final plot
ranks_final %>%
  mutate(score = 1/sqrt(final)) %>%
  ggplot(aes(x = reorder(symbol, final), y = score)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent) + 
  labs(x = "Stock", title = "Recommendation Score", y = "")

ranks_final %>%
  mutate(Score = 1/sqrt(final)) %>%
  ggplot() +
  geom_bar(aes(x = reorder(symbol, final), y = Score),
           stat = "identity", alpha = 0.4, width = .9) +
  geom_segment(aes(x = symbol, 
                   xend = symbol, 
                   y = Score,
                   yend = Score), 
               color = "purple") +
  geom_bar(data = mutate(ranks, score = 1/sqrt(rank)*.5),
           aes(x = symbol, y = score, fill = metric, group = metric), 
           stat = "identity", position = "dodge", alpha = 0.7) +
  scale_y_continuous(labels = scales::percent) + 
  labs(x = "Stock", title = "Recommendation Score", y = "", fill = "") +
  scale_fill_manual(values = 2:5, labels = c("PE", "Div Yield", "5Y Return", "Std Dev")) +
  theme_tq()

ranks_final %>%
  mutate(Score = 1/sqrt(final)) %>%
  ggplot() +
  geom_bar(aes(x = reorder(symbol, final), y = Score),
           stat = "identity", alpha = 0.4, width = .7) +
  geom_hline(aes(x = symbol, yintercept = Score), color = "purple") +
  geom_bar(data = mutate(ranks, score = 1/sqrt(rank)),
           aes(x = symbol, y = score, fill = metric, group = metric), 
           stat = "identity", position = "stack") +
  scale_y_continuous(labels = scales::percent) + 
  labs(x = "Stock", title = "Recommendation Score", y = "", fill = "") +
  scale_fill_manual(values = 2:6, labels = c("PE", "Div Yield", "5Y Return", "Std Dev")) +
  theme_tq()


# decide what to do with NA metrics
# companies that don't pay dividends may not necessarily deserve dead last in the rankings
# you can rank with na.last=NA to remove NA, na.last="keep" to rank as NA






