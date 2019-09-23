
SP500 <- tq_index("SP500")
tickers <- sample_n(SP500, 20)
symbols <- tickers$symbol


### ROE compare
source("get_industry.R")
source("get_stockrow.R")
source("get_keystats.R")

keystats <- get_keystats(symbols)
industry <- get_industry(symbols)
# industry data is not ordered properly
industry$industry <- left_join(tibble(Symbol = symbols), industry$industry, by = "Symbol")

# get ROE for tickers
ROE <- tibble(Symbol = symbols,
              Company_ROE = keystats$`Return on Equity (ttm)` %>% parse_number(),
              Industry_ROE = industry$industry$`Return on Equity (TTM)` %>% parse_number())
ROE # as a percent

# get P/E for tickers
PE <- tibble(Symbol = symbols,
              Company_PE = keystats$`Trailing P/E ` %>% parse_number(),
              Industry_ROE = industry$industry$`P/E (Last Year GAAP Actual)` %>% parse_number())
PE

industry$industry %>%
  mutate_all(parse_number,
             )




