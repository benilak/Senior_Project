library(cowplot)
library(tidyquant)
library(lubridate)
library(shiny)
library(dygraphs)

ui <- fluidPage(
  
  titlePanel("stocks and other marvelous things"),
  
  plotOutput(outputId = "cow"),
  
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
  start_date <- reactive({
    input$date[1]
  })
  end_date <- reactive({
    input$date[2]
  })
  dat_filt <- reactive({
    dat() %>% 
      filter(between(date, start_date(), end_date()))
  })
  dat_filt1 <- reactive({
    dat() %>%
      filter(between(date, start_date() - input$window1, end_date() + input$window1))
  })
  dat_filt2 <- reactive({
    dat() %>%
      filter(between(date, start_date() - input$window2, end_date() + input$window2))
  })
  
  plot1 <- reactive({
    dat_filt() %>%
      ggplot(aes(x = date, y = adjusted)) +
      geom_line(color = "snow", size = 2) +
      geom_line(color = "darkred") + 
      scale_y_continuous(labels = scales::dollar) + 
      geom_line(data = dat_filt1(),
                aes(y = rollmean(adjusted, input$window1, fill = NA, align = "center")),
                color = "gold") +
      geom_line(data = dat_filt2(),
                aes(y = rollmean(adjusted, input$window2, fill = NA, align = "center")),
                color = "forestgreen") +
      coord_cartesian(xlim = input$date, expand = c(0,0)) +
      theme_tq() +
      theme(axis.title = element_blank(),
            axis.text.x = element_blank())
  })
  
  plot2 <- reactive({
    dat_filt() %>%
      tq_transmute(select = volume, 
                   mutate_fun = period.apply,
                   INDEX = endpoints(.$date, 'days', 
                                     k = as.numeric(difftime(end_date(), start_date()))/50),
                   FUN = sum,
                   na.rm = TRUE,
                   col_rename = "volume") %>%
      ggplot() +
      geom_bar(aes(x = date, y = volume/1000000), stat = "identity", width = 1, color = "blue") +
      scale_y_continuous(labels = scales::comma) +
      labs(y = "Volume (millions)") +
      theme_tq() +
      theme(axis.title = element_blank())
  })
  
  output$cow <- renderPlot({
    
    plot_grid(plot1(), plot2(), align = "v", nrow = 2, rel_heights = c(3/4, 1/4))
    
  })
  
}

shinyApp(ui, server)
