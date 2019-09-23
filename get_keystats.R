
get_keystats <- function(symbol, path=NULL) {
  
  require(tidyquant)
  require(rvest)
  
  urls = paste0("https://finance.yahoo.com/quote/", symbol, "/key-statistics?p=", symbol)
  
  if (!is.null(path)) {
    tmps = path
  } else {
    tmps = tempfile(symbol)
    download.file(urls, tmps, method = "libcurl")
  }
  
  dat = map_df(tmps, function(x){
    txt = read_html(x) %>% 
      html_nodes("tr td") %>%
      html_text()
    lst = txt[seq(2, length(txt), 2)] %>% as.list()
    names(lst) = txt[seq(1, length(txt), 2)]
    return(lst)
  })
  dat = mutate(dat, Symbols = symbol) %>%
    select(Symbols, everything())
  
  file.remove(tmps)
  return(dat)
}
