#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(ggplot2)
library(shinydashboard)

dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(),
  dashboardBody(
    tabsetPanel(
      tabPanel("Table",
               checkboxGroupInput("show_vars", "Columns to show:",
                                  names(df), selected = names(df),
                                  inline = TRUE),
               checkboxInput('bar','All/None'),

    fluidRow(
      column(4,
             selectInput("ent",
                         "Entity:",
                         c("All",
                           unique(as.character(df$Entity))))
      ),
      column(4,
             selectInput("continent",
                         "continent:",
                         c("All",
                           unique(as.character(df$continent))))
      ),
      column(4,
             selectInput("region",
                         "region:",
                         c("All",
                           unique(as.character(df$region))))
      )
    ),
    # Create a new row for the table.
    div(style = 'overflow-x: scroll', DT::dataTableOutput("table"))
  ),
  tabPanel("TEST"))
  ))