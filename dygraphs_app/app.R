library(tidyquant)
library(timetk)
library(dygraphs)

ui <- fluidPage(
  
  titlePanel("more stocks and stupendous plots"),
  
  sidebarLayout(
    sidebarPanel(
      
      textInput(inputId = "ticker", label = "Select equity:", value = "AAPL"),
      checkboxInput("showgrid", label = "Show Grid", value = TRUE)
      
    ),
    mainPanel(
      dygraphOutput("dygraph")
    )
  )
)

server <- function(input, output) {
  
  dat <- reactive({
    tq_get(input$ticker) %>%
    tk_xts(close, date)
  })
  
  output$dygraph <- renderDygraph({
    dygraph(dat(), main = input$ticker) %>%
      dyRangeSelector() %>%
      dyOptions(drawGrid = input$showgrid)
  })
  
}

shinyApp(ui, server)