library(tidyquant)
library(shiny)
library(shinydashboard)
library(cowplot)

ui <- dashboardPage(
  
  dashboardHeader(title = "stocks and other marvelous things"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Analysis", tabName = "analysis", icon = icon("analysis")),
      menuItem("Widgets", tabName = "widgets", icon = icon("th"))
    )
  ),
  
  ## Body content
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "analysis",
              
              fluidRow(
                textInput(inputId = "symbols", 
                          label = "Input symbols:", 
                          value = ""),
                actionButton("add", "Add"),
                actionButton("remove", "Remove")
              ),
              
              fluidRow(
                box(verbatimTextOutput("symbols"))
              ),
              
              fluidRow(
                box(plotOutput(outputId = "scores_plot"))
              )
              
      ),
      
      # Second tab content
      tabItem(tabName = "widgets",
              
              fluidRow(
                box(plotOutput(outputId = "cow"), width = 12)
              ),
              
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
                box(sliderInput(inputId = "window1", label = "Rolling Average (Long-Term):",
                                min = 1, max = 365,
                                value = 100,
                                width = '95%'),
                    width = 6),
                
                box(sliderInput(inputId = "window2", label = "Rolling Average (Short-Term):",
                                min = 1, max = 365,
                                value = 28,
                                width = '95%'),
                    width = 6)
              )
      )
    )
  )
)


server <- function(input, output) {
  
  add_symbols <- function(x, y) {
    str_split(x, ",") %>%
      unlist() %>%
      toupper() %>%
      append(y) %>%
      unique()
  }
  rm_symbols <- function(x, y) {
    str_split(x, ",") %>%
      unlist() %>%
      toupper() %>%
      unique() %>%
      match(y)
      # something something
  }
  if (!exists("symbols_list")) {
    symbols_list <- character()
  }
  symbols <- eventReactive(input$add, {
    # browser()
    symbols_list <- add_symbols(input$symbols, symbols_list)
    add_symbols(input$symbols, symbols_list)
  })
  output$symbols <- renderText({
    print(symbols())
  })
  
  # rank symbols by 5Y Ann. Yield
  stock_dat <- reactive({
    tq_get(symbols())
  })
  stock_annualized <- reactive({
    stock_dat() %>%
      filter(between(date, today()-years(5), today())) %>%
      group_by(symbol) %>%
      tq_transmute(select = adjusted, # gets monthly returns
                   mutate_fun = periodReturn, 
                   period     = "monthly") %>%
      # annualizes
      tq_performance(Ra = monthly.returns, performance_fun = table.AnnualizedReturns) %>%
      ungroup() %>%
      mutate(rankReturn = rank(-AnnualizedReturn),
             rankSD = rank(AnnualizedStdDev))
  }) 
  
  # rank PE and dividend yield
  stock_stats <- reactive({
    get_industry(symbols(), get = "compare") %>%
      filter(Key == "Percentile") %>%
      mutate(percentilePE = parse_number(`P/E (TTM)`),
             percentileDividend = parse_number(`Dividend Yield (Annualized)`),
             rankPE = rank(percentilePE),
             rankDividend = rank(-percentileDividend, na.last = "keep")) %>%
      rename(symbol = Symbol) %>%
      select(symbol, percentilePE, percentileDividend, rankPE, rankDividend)
  }) 
  
  # final ranks
  wghts <- c(rankPE = 1, rankDividend = 2.5, rankReturn = 4.5, rankSD = 2)
  ranks <- reactive({
    full_join(stock_annualized(), stock_stats(), by = "symbol") %>%
      gather(names(wghts), key = "metric", value = "rank") %>%
      mutate(metric = factor(metric, levels = names(wghts))) %>%
      select(-`AnnualizedSharpe(Rf=0%)`)
  })
  ranks_final <- reactive({
    ranks() %>%
      group_by(symbol) %>%
      arrange(metric) %>% # must arrange by metric, which must be a factor ordered by weight names
      summarise(final = weighted.mean(rank, wghts, na.rm = TRUE)) %>%
      mutate(final = rank(final))
  })
  
  # ranks final plot
  output$scores_plot <- renderPlot({
    ranks_final() %>%
      mutate(score = 1/sqrt(final)) %>%
      ggplot(aes(x = reorder(symbol, final), y = score)) +
      geom_bar(stat = "identity") +
      scale_y_continuous(labels = scales::percent) + 
      labs(x = "Stock", title = "Recommendation Score", y = "")
  })
  
  
  
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
