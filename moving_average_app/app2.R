library(tidyquant)
library(lubridate)
library(shiny)
library(dygraphs)

ui <- fluidPage(
  
  titlePanel("stocks and other marvelous things"),
  
  plotOutput(outputId = "timeSeries"),
  
  plotOutput(outputId = "Volume", height = "100px"),
  
  fluidRow(
    column(2, textInput(inputId = "ticker", 
                        label = "Ticker symbol:", 
                        value = "AAPL")
    ),
    column(10, sliderInput("date", "Dates:",
                           min = today() - years(10),
                           max = today(),
                           value = c(today() - years(1), today()),
                           timeFormat = "%Y-%m-%d",
                           width = '95%')
           )
    ),
  
  fluidRow(
    column(6, sliderInput(inputId = "window1", label = "Rolling Average (Long-Term):",
                          min = 1, max = 365,
                          value = 100,
                          width = '95%')
           ),
    column(6, sliderInput(inputId = "window2", label = "Rolling Average (Short-Term):",
                          min = 1, max = 365,
                          value = 28,
                          width = '95%')
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
  
  output$Volume <- renderPlot({
    
    dat() %>%
      filter(between(date, input$date[1], input$date[2])) %>%
      tq_transmute(select = volume, 
                   mutate_fun = apply.weekly,
                   FUN = sum,
                   na.rm = TRUE,
                   col_rename = "volume") %>%
      ggplot() +
      geom_bar(aes(x = date, y = volume), stat = "identity", color = "blue") +
      theme_tq()
    
  })
  
}

shinyApp(ui, server)
