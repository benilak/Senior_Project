library(shiny)
library(jsonlite)

ui <- fluidPage(
  
  h2(textOutput("lastPrice"))
  
)

server <- function(input, output, session) {
  
  output$lastPrice <- renderText({
    invalidateLater(5000, session)
    price <- fromJSON("https://www.alphavantage.co/query?function=CURRENCY_EXCHANGE_RATE&from_currency=BTC&to_currency=USD&apikey=")
    paste("Bitcoin: $", price[[1]]$`5. Exchange Rate`, sep = "")
  })
  
}

shinyApp(ui, server)