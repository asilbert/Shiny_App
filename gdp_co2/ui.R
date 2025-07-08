library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(ggplot2)
library(dplyr)
library(ggiraph)
library(bslib)

ui <- dashboardPage(
  
  dashboardHeader(title = "Sustainable Growth"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("globe")),
      menuItem("Charts", tabName = "charts", icon = icon("chart-line")),
      menuItem("Tables", tabName = "tables", icon = icon("table"))
    )
  ),
  
  dashboardBody(
    shinyDashboardThemes(theme = "blue_gradient"),
    tags$head(
      tags$style(HTML(".box { box-shadow: 0 2px 10px rgba(0,0,0,0.05); border-radius: 12px; }"))
    ),
    
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                box(title = "🌍 Project Overview", width = 12, status = "primary", solidHeader = TRUE,
                    h4("Reducing Emissions Without Sacrificing Growth")
                )
              ),
              fluidRow(
                box(title = "🎯 Project Goal", width = 12, status = "success", solidHeader = TRUE,
                    p("Develop a data-driven methodology to identify countries achieving GHG reduction without sacrificing economic output."),
                    p("Forecast the next potential leaders in GHG-efficient performance.")
                )
              ),
              fluidRow(
                box(title = "📊 Project Summary", width = 12, status = "info", solidHeader = TRUE,
                    p("This analysis challenges the perceived trade-off between environmental responsibility and economic productivity."),
                    p("It highlights nations reducing emissions while maintaining growth, providing evidence-based examples for future global policy.")
                )
              ),
              fluidRow(
                box(title = "📂 Data Source", width = 12, status = "warning", solidHeader = TRUE,
                    p("Based on the dataset:"),
                    tags$b("Global Data on Sustainable Energy (2000–2020)"), br(),
                    tags$a(href = "https://www.kaggle.com/datasets/anshtanwar/global-data-on-sustainable-energy",
                           "View on Kaggle", target = "_blank")
                )
              )
      ),
      
      tabItem(tabName = "charts",
              tabsetPanel(
                tabPanel("Line Chart", source("modules/line_chart.R", local = TRUE)$value),
                tabPanel("Bubble Plot", source("modules/bubble_plot.R", local = TRUE)$value)
              )
      ),
      
      tabItem(tabName = "tables",
              tabsetPanel(
                tabPanel("Table",
                         fluidRow(
                           column(12,
                                  switchInput("summarize", label = "Summarize Averages", labelWidth = "150px"),
                                  checkboxInput('bar', 'Select All Columns'),
                                  checkboxGroupInput("show_vars", "Columns to show:", names(df), selected = names(df), inline = TRUE)
                           )
                         ),
                         fluidRow(
                           column(4, selectInput("ent", "Country:", c("All", unique(as.character(df$Country))))),
                           column(4, selectInput("continent", "Continent:", c("All", unique(as.character(df$continent))))),
                           column(4, selectInput("region", "Region:", c("All", unique(as.character(df$region)))))
                         ),
                         div(style = 'overflow-x: scroll', DT::dataTableOutput("table"))
                )
              )
      )
    )
  )
)
