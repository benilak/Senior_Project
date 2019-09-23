
## change Phantom.js scrape file
symbol <- "TJX"
url <- paste0("https://stockrow.com/", symbol)
lines <- read_lines("scrape_final.js")
lines[1] <- paste0("var url ='", url ,"';")
write_lines(lines, "scrape_final.js")

## Download website
system("phantomjs scrape_final.js")

## use Rvest to scrape the downloaded website.
webpage <- read_html("1.html")
html_nodes(webpage, ".name") %>%
  html_text()




PE/Growth=PEG=2.2
21.68/g = 2.2


# get Beta, PE, EPS, others from Yahoo
library(tidyquant)
library(rvest)
symbol <- "AAPL"
url <- paste0("https://finance.yahoo.com/quote/", symbol)
txt <- read_html(url) %>%
  html_nodes("tr td") %>%
  html_text()
list1 <- txt[seq(2, length(txt), 2)] %>% as.list()
names(list1) <- txt[seq(1, length(txt), 2)]
list1
list1$`PE Ratio (TTM)`
# even more metrics
symbol <- "AAPL"
url <- paste0("https://finance.yahoo.com/quote/", symbol, "/key-statistics?p=", symbol)
txt <- read_html(url) %>%
  html_nodes("tr td") %>%
  html_text()
list1 <- txt[seq(2, length(txt), 2)] %>% as.list()
names(list1) <- txt[seq(1, length(txt), 2)]
list1





