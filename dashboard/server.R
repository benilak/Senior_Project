
tickers.list <- character()

# server
server <- function(input, output) {
  
  # update checkbox list of tickers
  v <- reactiveValues(tickers = NULL)
  observeEvent(input$searchButton, {
    tickers.list <<- str_split(input$tickers.text, ",") %>%
      unlist() %>%
      str_replace_all("\\s", "") %>%
      toupper() %>%
      append(tickers.list) %>%
      unique()
    v$tickers <- tickers.list
  })
  output$tickers.checkbox <- renderUI({
    box(width = 12, background = "black",
        fluidRow(column(6, checkboxGroupInput("tickers.checked", "Tickers:",
                                               choices = v$tickers, selected = v$tickers)),
                 column(6, actionButton("clear", label = "", icon = icon("redo")))
                 ))
  })
  observeEvent(input$clear, {
    v$tickers <- input$tickers.checked
    tickers.list <<- input$tickers.checked
  })
  output$chart.tickers <- renderUI({
    fluidRow(
      column(12, box(width = 12, checkboxGroupInput("chart.tickers", "Tickers:",
                                        choices = v$tickers, selected = v$tickers, inline = TRUE)))
    )
  })
  
  # headers 
  output$visual.header <- renderText({
    paste("Custom Visualizations:", input$searchText, sep = "  ")
  })
  output$data.header <- renderText({
    paste("Data Tables:", input$searchText, sep = "  ")
  })
  
  # price history
  stocks <- reactive({
    req(input$searchButton)
    if (length(tickers.list) == 1) {
      tq_get(tickers.list) %>%
        mutate(symbol = tickers.list[1])
    } else {
      tq_get(tickers.list)
    }
  })
  stock <- reactive({
    stocks() %>% filter(symbol == input$chart.tickers[1])
  })
  stocks.multi <- reactive({
    stocks() %>% filter(symbol %in% input$chart.tickers)
  })
  start_date <- reactive(input$date[1])
  end_date <- reactive(input$date[2])
  stock_filt <- reactive({
    stock() %>% filter(between(date, start_date(), end_date()))
  })
  multi_filt <- reactive({
    stocks.multi() %>% filter(between(date, start_date(), end_date()))
  })
  stock_filt1 <- reactive({
    stock() %>% filter(between(date, start_date() - input$window1, end_date() + input$window1))
  })
  stock_filt2 <- reactive({
    stock() %>% filter(between(date, start_date() - input$window2, end_date() + input$window2))
  })
  
  # metrics
  annualized <- reactive({
    stocks() %>%
      filter(between(date, today() - years(5), today())) %>%
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
  industry.dat <- reactive({
    get_industry(tickers.list, get = "compare")
  })
  stock.stats <- reactive({
    industry.dat() %>% 
      filter(Key == "Percentile") %>%
      mutate(percentileDividend = parse_number(`Dividend Yield (Annualized)`),
             percentilePE = parse_number(`P/E (TTM)`),
             percentileEPS = parse_number(`EPS Growth (TTM vs Prior TTM)`),
             rankPE = rank(percentilePE),
             rankDividend = rank(-percentileDividend, na.last = "keep"),
             rankEPS = rank(-percentileEPS)) %>%
      rename(symbol = Symbol) %>%
      select(symbol, percentilePE, percentileDividend, percentileEPS, 
             rankPE, rankDividend, rankEPS)
  })
  wghts <- reactive({
    c(rankDividend = input$divYield.weight,
      rankEPS = input$EPS.weight,
      rankPE = input$PE.weight,
      rankReturn = input$return.weight, 
      rankSD = input$SD.weight)
  })
  ranks <- reactive({
    full_join(annualized(), stock.stats(), by = "symbol") %>%
      gather(names(wghts()), key = "metric", value = "rank") %>%
      mutate(metric = factor(metric, levels = names(wghts()))) %>%
      select(-`AnnualizedSharpe(Rf=0%)`)
  })
  ranks.final <- reactive({
    ranks() %>%
      group_by(symbol) %>%
      arrange(metric) %>% # must arrange by metric, which must be a factor ordered by weight names
      summarise(final = weighted.mean(rank, wghts(), na.rm = TRUE)) %>%
      mutate(final = rank(final))
  })
  
  ## analyze plots
  
  # parallel plots
  output$parallel <- renderPlot({
    ranks() %>%
      ggplot(aes(x = metric, y = rank, color = symbol, group = symbol)) +
      geom_point(size = 2) +
      geom_path(size = 1) +
      scale_x_discrete(expand = c(0,1), labels = c("EPS Growth", "Dividend Yield", "PE Ratio", 
                                                   "Return (Annualized)", "Std. Deviation")) +
      scale_y_reverse(breaks = 1:10) +
      labs(title = "Parallel Plot of metrics", subtitle = "ranks as 1st, 2nd, etc...",
           y = "Rank", x = "") +
      theme_tq() +
      theme(legend.position = "none",
            panel.grid.major.y = element_line(linetype = "dashed"),
            axis.text = element_text(size = 12),
            legend.text = element_text(size = 12),
            title = element_text(size = 15, face = "bold")) +
      geom_dl(aes(label = symbol), method = list(dl.trans(x=x+0.5), "last.points"))
  }) 
  
  # risk vs reward
  output$risk.reward <- renderPlot({
    annualized() %>%
      ggplot(aes(x = AnnualizedStdDev, y = AnnualizedReturn)) +
      geom_point() +
      geom_label_repel(aes(label = symbol, color = symbol)) +
      theme_tq() +
      theme(legend.position = "none",
            panel.grid.minor = element_line(linetype = "dashed"),
            axis.text = element_text(size = 12),
            legend.text = element_text(size = 12),
            title = element_text(size = 15, face = "bold")) +
      labs(title = "Risk vs. Reward", x = "Standard Deviation of Returns", y = "Annualized Return")
  })
  
  # ranks final plot
  output$final1 <- renderPlot({
    ranks.final() %>%
      mutate(score = 1/sqrt(final)) %>%
      ggplot(aes(x = reorder(symbol, final), y = score)) +
      geom_bar(stat = "identity", color = "blue", fill = "skyblue") +
      scale_y_continuous(labels = scales::percent) + 
      labs(x = "Stock", title = "Recommendation Score", y = "") +
      theme(axis.text = element_text(size = 12),
            legend.text = element_text(size = 12),
            title = element_text(size = 15, face = "bold"))
  })
  
  output$final2 <- renderPlot({
    ranks.final() %>%
      mutate(Score = 1/sqrt(final)) %>%
      ggplot() +
      geom_bar(aes(x = reorder(symbol, final), y = Score),
               stat = "identity", alpha = 0.4, width = .9) +
      geom_segment(aes(x = symbol, 
                       xend = symbol, 
                       y = Score,
                       yend = Score), 
                   color = "purple") +
      geom_bar(data = mutate(ranks(), score = 1/sqrt(rank)*.6),
               aes(x = symbol, y = score, fill = metric, group = metric), 
               stat = "identity", position = "dodge", alpha = 0.8) +
      scale_y_continuous(labels = scales::percent) + 
      labs(x = "Stock", title = "Recommendation Score", y = "", fill = "") +
      scale_fill_brewer(palette = "Accent", 
                        labels = c("EPS Growth", "Dividend Yield", "PE Ratio", 
                                   "Return (Annualized)", "Std. Deviation")) +
      theme_tq() +
      theme(axis.text = element_text(size = 12),
            legend.text = element_text(size = 12),
            title = element_text(size = 15, face = "bold"))
  })
  
  output$final3 <- renderPlot({
    ranks.final() %>%
      mutate(Score = 1/sqrt(final)) %>%
      ggplot() +
      geom_bar(aes(x = reorder(symbol, final), y = Score*0.1),
               stat = "identity", alpha = 0.4, width = .7) +
      geom_bar(data = mutate(ranks(), score = 1/sqrt(rank)*(1/4)),
               aes(x = symbol, y = score, fill = metric, group = metric), 
               stat = "identity", position = "stack") +
      scale_y_continuous(labels = NULL) + 
      labs(x = "Stock", subtitle = "Relative metric scores", y = "", fill = "") +
      scale_fill_brewer(palette = "Accent", labels = c("EPS Growth", "Dividend Yield", "PE Ratio", 
                                                 "Return (Annualized)", "Std. Deviation")) +
      theme_tq() +
      theme(axis.text = element_text(size = 12),
            axis.text.y = element_blank(),
            legend.text = element_text(size = 12),
            title = element_text(size = 15, face = "bold"))
  })

  
  
  # stock plots
  stock.plot <- reactive({
    stock_filt() %>%
      ggplot(aes(x = date, y = adjusted)) +
      geom_line(color = "snow", size = 2) +
      geom_line(aes(color = "darkred")) + 
      scale_y_continuous(labels = scales::dollar) + 
      geom_line(data = stock_filt1(),
                aes(y = rollmean(adjusted, input$window1, fill = NA, align = "center"),
                color = "gold")) +
      geom_line(data = stock_filt2(),
                aes(y = rollmean(adjusted, input$window2, fill = NA, align = "center"),
                color = "forestgreen")) +
      coord_cartesian(xlim = input$date, expand = c(0,0)) +
      scale_color_manual(values = c("darkred" = "darkred", "gold" = "gold", 
                                    "forestgreen" = "forestgreen"),
                         labels = c("Price", "Rolling Average 1", "Rolling Average 2"),
                         name = NULL) +
      labs(title = paste("Price History: ", input$chart.tickers[1])) +
      theme_tq() +
      theme(axis.title = element_blank(),
            axis.text.x = element_blank(),
            legend.position = "top",
            legend.text = element_text(size = 12),
            title = element_text(size = 15, face = "bold"))
  })
  
  volume.plot <- reactive({
    stock_filt() %>%
      tq_transmute(select = volume, 
                   mutate_fun = period.apply,
                   INDEX = endpoints(.$date, 'days', 
                                     k = as.numeric(difftime(end_date(), start_date()))/50),
                   FUN = sum,
                   na.rm = TRUE,
                   col_rename = "volume") %>%
      ggplot() +
      geom_bar(aes(x = date, y = volume/1000000), stat = "identity", width = 1, color = "blue") +
      coord_cartesian(xlim = input$date, expand = c(0,0)) +
      scale_y_continuous(labels = scales::comma) +
      labs(title = "Volume", y = "Volume (millions)") +
      theme_tq() +
      theme(axis.title = element_blank())
  })
  
  output$cow <- renderPlot({
    plot_grid(stock.plot(), volume.plot(), align = "v", nrow = 2, rel_heights = c(3/4, 1/4))
  })
  
  output$multi.plot <- renderPlot({
    multi_filt() %>%
      ggplot(aes(x = date, y = adjusted, color = symbol)) +
      geom_line() +
      coord_cartesian(xlim = input$date, expand = c(0,0)) +
      scale_y_continuous(labels = scales::dollar) + 
      labs(title = "Price History", color = "") +
      theme_tq() +
      theme(axis.title = element_blank(),
            legend.position = "top",
            legend.text = element_text(size = 13),
            title = element_text(size = 15, face = "bold"))
  })
  
  output$which.plot <- renderUI({
    if (input$multi.stocks) {
      plotOutput("multi.plot")
    } else {
        plotOutput("cow")
    }
  })
  
  # stockrow data & custom plots
  output$radio <- renderUI({
    box(width = 12, 
        radioButtons("radio.butts", "Ticker:", choices = tickers.list, selected = tickers.list[1],
                     inline = TRUE))
  })
  # dat.income <- reactive({
  #   req(input$clear)
  #   get_stockrow(tickers.list, section = "income")
  # })
  output$income.table <- renderDataTable({
    income <- dat.income()
    return(income[input$radio.butts])
  })
  dat.balance <- reactive({
    req(input$clear)
    get_stockrow(tickers.list, section = "balance")
  })
  output$balance.table <- renderDataTable({
    balance <- dat.balance()
    return(balance[input$radio.butts])
  })
  dat.cash <- reactive({
    req(input$clear)
    get_stockrow(input$searchText, section = "cash")
  })
  output$cash.table <- renderDataTable({
    cash <- dat.cash()
    return(cash[input$radio.butts])
  })
  dat.metrics <- reactive({
    req(input$clear)
    get_stockrow(input$searchText, section = "metrics")
  })
  output$metrics.table <- renderDataTable({
    metrics <- dat.metrics()
    return(metrics[input$radio.butts])
  })
  dat.growth <- reactive({
    req(input$clear)
    get_stockrow(input$searchText, section = "growth")
  })
  output$growth.table <- renderDT({
    growth <- dat.growth()
    return(growth[input$radio.butts])
  })
  
}
