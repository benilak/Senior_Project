
get_stockrow <- function(symbol, section, dimension = "MRY", tidy = TRUE, desc = TRUE) {
  
  require(tidyverse)
  
  dimlist = list(MRQ = c("MRQ", "quarterly", "Quarterly", "quarter", "Quarter", "Q", "q"), 
                 MRY = c("MRY", "Yearly", "yearly", "Year", "year", "Annual", "annual", "Annually", "annually", "A", "a", "Y", "y"),
                 MRT = c("MRT", "Trailing", "trailing", "Trail", "trail", "T", "t"))
  dim_id = map(dimlist, function(x){dimension %in% x}) %>% map(which)
  dimensions = vector()
  for (i in seq_along(dim_id)) {
    dimensions[dim_id[[i]]] = names(dim_id[i])
  }
  
  seclist = list(`Income%20Statement` = c("Income", "income"),
                 `Balance%20Sheet` = c("Balance", "balance", "Bal", "bal", "Balance Sheet", "Balance sheet", "balance sheet"),
                 `Cash%20Flow` = c("Cash", "cash", "Flow", "flow", "Cash flow", "cash flow", "Cash Flow"),
                 Metrics = c("Metrics", "metrics", "Metric", "metric"),
                 Growth = c("Growth", "growth"))
  sec_id = map(seclist, function(x){section %in% x}) %>% map(which)
  sections = vector()
  for (i in seq_along(sec_id)) {
    sections[sec_id[[i]]] = names(sec_id[i])
  }
  
  sortbool = c(desc = desc, asc = !desc)
  sort = names(which(sortbool))
  
  if (sections %in% c("Balance%20Sheet", "Growth") & dimensions == "MRT") {
    stop("Balance Sheet and Growth sections do not contain trailing-12-months data")
  }
  
  urls = paste("https://stockrow.com/api/companies/", symbol, "/financials.xlsx?dimension=", dimensions, "&section=", sections, "&sort=", sort, sep = "")
  tmps = tempfile(symbol, fileext = ".xlsx")
  download.file(urls, tmps, method = "libcurl", mode = "wb") # kinda slow?
  
  # browser()
  
  stockrow = map(tmps, function(x) {
    # browser()
    dat = readxl::read_xlsx(x) %>%
      select(-num_range(prefix = "...", range = 2:100)) 
    nams = names(dat)[2:length(dat)] %>%
      as.numeric() %>%
      as.Date(origin = "1899-12-30") %>%
      as.character()
    names(dat) = c("Key", nams)
    dat$Key = make.unique(dat$Key)
    return(dat)
  })
  
  if (length(stockrow) == 1) {
    stockrow = stockrow[[1]]
    if (tidy == TRUE) {
      stockrow = stockrow %>%
        gather(key = "Date", value = "value", -"Key") %>%
        spread(key = Key, value = value) %>%
        select(Date, stockrow$Key)
    }
  } else {
    names(stockrow) = symbol
    if (tidy == TRUE) {
      stockrow = map(stockrow, function(x){
        x = x %>%
          gather(key = "Date", value = "value", -"Key") %>%
          spread(key = Key, value = value) %>%
          select(Date, x$Key)
      })
    }
  }
  
  # file.remove(tmps)
  return(stockrow)
  
}
