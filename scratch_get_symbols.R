library(tidyquant)
SP500 <- tq_index("SP500")
set.seed(7)
SP <- sample(seq(to=nrow(SP500)), 10) %>%
  SP500[.,]
SP

url = "https://eresearch.fidelity.com/eresearch/evaluate/snapshot.jhtml?symbols=PNW"
tmp = tempfile("PNW")
download.file(url, tmp)

symbols = c("AAPL", "TSLA", "MSFT")
urls = paste("https://eresearch.fidelity.com/eresearch/evaluate/snapshot.jhtml?symbols=", 
             symbols, sep = "")
tmps = tempfile(symbols)
# libcurl method downloads files simultaneously
download.file(urls, tmps, method = "libcurl")




# compare speeds
library(microbenchmark)
microbenchmark({
  ind_names3 = map(tmps, function(x) {
    webtext = read_html(x) %>%
      html_nodes("#companyProfile :nth-child(1)") %>%
      html_text()
    ind_names = list("Sector" = webtext[(match("Sector (GICS®)", webtext) + 1)],
                     "Industry" = webtext[(match("Industry (GICS®)", webtext) + 1)])
    return(ind_names)
  })
})

microbenchmark({
  webtext = map(tmps, function(x) {
    txt = read_html(x) %>%
      html_nodes("#companyProfile :nth-child(1)") %>%
      html_text()
    return(txt)
  }) 
  ind_names = list("Sector" = webtext[(match("Sector (GICS®)", webtext) + 1)],
                   "Industry" = webtext[(match("Industry (GICS®)", webtext) + 1)])
})


