library(tidyquant)
library(lubridate)
library(shiny)
library(zoo)
library(shinydashboard)
library(dygraphs)


ui <- fluidPage(
  
  titlePanel("stocks and other marvelous things"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      textInput(inputId = "ticker", label = "Ticker symbol:", value = "AAPL"),
      
      sliderInput("date", "Dates:",
                  min = today() - years(10),
                  max = today(),
                  value = c(today() - years(1), today()),
                  timeFormat = "%Y-%m-%d"),
      
      #sliderInput(inputId = "date", label = "Date:",
                  #min = min(tesla$date), max = max(tesla$date),
                  #value = c(min(tesla$date), max(tesla$date))),
      
      sliderInput(inputId = "window1", label = "Rolling Average (Long-Term):",
                  min = 1, max = 365,
                  value = 100),
      
      sliderInput(inputId = "window2", label = "Rolling Average (Short-Term):",
                  min = 1, max = 365,
                  value = 28)
    ),
    
    mainPanel(
      
      plotOutput(outputId = "timeSeries")
      
    )
  )
)


server <- function(input, output) {
  
  dat <- reactive({
    tq_get(input$ticker)
  })
  
  
  
  output$timeSeries <- renderPlot({
  
    dat() %>%
      filter(between(date, input$date[1], input$date[2])) %>%
      ggplot(aes(x = date, y = adjusted)) +
      geom_line(color = "snow", size = 2) +
      geom_line(color = "darkred") + 
      scale_y_continuous(labels = scales::dollar) + 
      #scale_x_continuous(limits = input$date) +
      #coord_cartesian(xlim = input$date) +
      geom_line(aes(y = rollmean(adjusted, input$window1, fill = NA)), color = "gold") +
      geom_line(aes(y = rollmean(adjusted, input$window2, fill = NA)), color = "forestgreen") +
      theme_tq()
      #labs(title = paste(input$ticker, "Stock Prices"), 
           #x = "Date", y = "Adjusted Close Price (Daily)")
    
    })

}

shinyApp(ui, server)
