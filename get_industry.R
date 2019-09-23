
get_industry <- function(symbol, get=c("names", "compare", "industry")) {
  
  require(tidyverse)
  require(rvest)
  
  urls = paste("https://eresearch.fidelity.com/eresearch/evaluate/snapshot.jhtml?symbols=", 
               symbol, sep = "")
  tmps = tempfile(symbol)
  # 'libcurl' method allows simultaneous downloads
  download.file(urls, tmps, method = "libcurl")
  
  # names
  # browser()
  if ("names" %in% get) {
    ind_names = map_df(tmps, function(x) {
      webtext = read_html(x) %>%
        html_nodes("#companyProfile :nth-child(1)") %>%
        html_text()
      nam = tibble("Sector" = webtext[(match("Sector (GICS®)", webtext) + 1)],
                   "Industry" = webtext[(match("Industry (GICS®)", webtext) + 1)])
      return(nam)
    })
    ind_names = mutate(ind_names, Symbol = symbol) %>%
      select(Symbol, everything())
  } else {ind_names = NULL}
  
  # compare
  # browser()
  if ("compare" %in% get) {
    ind_compare = map_df(tmps, function(x) {
      dat = read_html(x) %>%
        html_nodes("tr:nth-child(1) .bottom-border") %>%
        html_text() %>%
        .[-(1:2)] %>%
        matrix(ncol = 4, byrow = TRUE) %>%
        as_tibble()
      colnames(dat) = dat[1,]
      colnames(dat)[1] = "Key"
      dat = dat[-1,]
      dat$Key = c("Market Capitalization", "Total Return (1 Year Annualized)", "Beta", "EPS (TTM)", "EPS Estimate", "EPS Growth (TTM vs Prior TTM)", "P/E (TTM)", "Dividend Yield (Annualized)", "Total Revenue (TTM)", "Revenue Growth (TTM vs Prior TTM)", "Shares Outstanding", "Institutional Ownership")
      return(dat)
    })
    ind_compare = ind_compare %>% 
      gather(key = "Symbol", value = "Value", symbol) %>%
      select(Symbol, everything()) %>%
      filter(!is.na(Value)) %>%
      rename(Percentile = `Percentilein Industry`) %>%
      gather(`Industry Average`, `Percentile`, `Value`, key = Key2, value = Value) %>% 
      spread(Key, value = Value) %>%
      rename(Key = Key2)
  } else {ind_compare = NULL}
  
  # industry
  if ("industry" %in% get) {
    ind_urls = map(tmps, function(x) {
      ind_url = read_html(x) %>%
        html_nodes("#compare :nth-child(1)") %>% 
        .[5] %>%
        as.character() %>%
        str_extract('(?<=").*?(?=")') %>%
        str_replace('&amp;', '&')
      return(ind_url)
    }) %>% unlist() %>% c()
    ind_tmps = tempfile(paste0("ind_", symbol))
    download.file(ind_urls, ind_tmps, method = "libcurl")
    ind_data = map_df(ind_tmps, function(x) {
      ind_html = read_html(x)
      ind_keys = html_nodes(ind_html, ".align-left") %>%
        html_text()
      ind_values = html_nodes(ind_html, ".align-left+ td") %>%
        html_text() %>% 
        str_replace_all('\t|\r\n|\\s', '')
      dat = tibble(Key = ind_keys, Value = ind_values)
      return(dat)
    }, .id = "Symbol")
    ind_data = mutate(ind_data, Symbol = rep(symbol, each = 10)) %>%
      spread(key = Key, value = Value)
    file.remove(ind_tmps)
  } else {ind_data = NULL}
  
  file.remove(tmps)
  
  # return data
  # browser()
  dat = list(names = ind_names, compare = ind_compare, industry = ind_data)
  dat = dat[c("names", "compare", "industry") %in% get]
  if (length(dat) == 1) {
    dat = dat[[1]]
    return(dat)
  } else {
    return(dat)
  }
}
