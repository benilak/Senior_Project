# ui
library(shiny)
library(shinydashboard)
library(tidyquant)
library(DT)
library(cowplot)
library(ggrepel)
library(directlabels)
source("../get_stockrow.R")
source("../get_industry.R", encoding = "utf-8") # to read trademark symbol (R) in webpage
source("../get_keystats.R")

# sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Stock Chart", tabName = "stockchart", icon = icon("chart-line")),
    menuItem("Analyze", tabName = "analyze", icon = icon("chart-bar")),
    menuItem("Data", icon = icon("th"), tabName = "data"),
    sidebarSearchForm(textId = "tickers.text", buttonId = "searchButton",
                      label = "Ticker symbols"),
    uiOutput("tickers.checkbox")
    
  )
)

# body
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "stockchart",
            uiOutput("chart.tickers"),
            box(width = 12,
                uiOutput("which.plot"),
                fluidRow(
                  column(offset = 1, width = 11, sliderInput("date", "Dates:",
                                         min = today() - years(10),
                                         max = today(),
                                         value = c(today() - years(1), today()),
                                         timeFormat = "%Y-%m-%d",
                                         width = '95%'))
                )
            ),
            box(title = "Chart Options", width = 12, collapsible = TRUE,
                fluidRow(column(4, 
                                numericInput("window1", label = "Rolling Average 1 (Days):", 
                                                value = 100, width = "50%")),
                         column(4, numericInput("window2", label = "Rolling Average 2 (Days):", 
                                                value = 31, width = "50%")),
                         column(4, checkboxInput("multi.stocks", label = "Show multiple stocks"))
                )
            )
    ),     
            
    # Analyze Tab
    tabItem(tabName = "analyze",
            h3("Select your weights:"),
            h4("Please apply a number to each metric based on their importance to you. Weights are
               relative to each other - a higher number will weigh the metric more heavily; a zero
               means the metric will not be included in the analysis at all."),
            box(title = "Metric Weights:", width = 12,
                fluidRow(column(2, numericInput("divYield.weight", "Dividend Yield", value = 1)),
                         column(2, numericInput("EPS.weight", "EPS Growth", value = 1)),
                         column(2, numericInput("PE.weight", "PE Ratio", value = 1)),
                         column(3, numericInput("return.weight", "Return (Annualized)", value = 1)),
                         column(3, numericInput("SD.weight", "Standard Deviation (Annualized)", 
                                                value = 1))
                )
            ),
            tabBox(width = 12,
              tabPanel(title = "Final Recommendation", width = 12,
                       plotOutput("final1")),
              tabPanel(title = "Parallel Plot", width = 12,
                  plotOutput("parallel")),
              tabPanel(title = "Risk vs. Reward", width = 12,
                       plotOutput("risk.reward")),
              tabPanel(title = "Metric Scores", width = 12,
                  plotOutput("final2"),
                  plotOutput("final3"))
            )
    ),
    
    # Data Tables Tab
    tabItem(tabName = "data",
            h2(textOutput(outputId = "data.header")),
            # fluidRow(
            #   column(9, h2(textOutput(outputId = "data.header"), width = 10)),
            #   column(3, downloadButton("downloadData", "Download"))
            # ),
            
            tabBox(
              title = "",
              # The id lets us use input$tabset1 on the server to find the current tab
              id = "tabset2", width = 12,
              tabPanel("Income Statement",
                       uiOutput("radio"),
                       fluidRow(column(10, ""), 
                                column(2, downloadButton("download.income", "Download"))),
                       dataTableOutput("income.table")
              ),
              tabPanel("Balance Sheet",
                       fluidRow(column(10, ""), 
                                column(2, downloadButton("download.balance", "Download"))),
                       dataTableOutput("balance.table")
              ),
              tabPanel("Cash Flow",
                       fluidRow(column(10, ""), 
                                column(2, downloadButton("download.cash", "Download"))),
                       dataTableOutput("cash.table")),
              tabPanel("Metrics",
                       fluidRow(column(10, ""), 
                                column(2, downloadButton("download.metrics", "Download"))),
                       dataTableOutput("metrics.table")),
              tabPanel("Growth",
                       fluidRow(column(10, ""), 
                                column(2, downloadButton("download.growth", "Download"))),
                       dataTableOutput("growth.table"))
            )
    )
  )
)


# ui
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Securities Analysis"),
  sidebar,
  body
)
