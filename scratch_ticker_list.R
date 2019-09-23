# scratch: adding to and removing from list of checkboxes in dashboard

# update checkbox list of tickers
tickers <- eventReactive(input$searchButton, {
  tickers.list <<- str_split(input$tickers.text, ",") %>%
    unlist() %>%
    str_replace_all("\\s", "") %>%
    toupper() %>%
    append(tickers.list) %>%
    unique()
  return(tickers.list)
})
output$tickers <- renderUI({
  fluidRow(column(10, checkboxGroupInput("tickers.checked", "Select tickers",
                                         choices = tickers(), selected = tickers())),
           column(2, actionButton("clear", "Clear Unchecked")))
})
tickers.checked <<- reactive(input$tickers.checked)

