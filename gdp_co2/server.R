library(shiny)
library(dplyr)
library(ggplot2)
library(ggiraph)
library(plotly)

source("modules/line_chart_server.R")
source("modules/bubble_plot_server.R")


server <- function(input, output, session) {
  
  line_chart_server(input, output, session, df, drops)
  bubble_plot_server(input, output, session, df, drops)

  
  # Table filtering logic
  observe({
    updateCheckboxGroupInput(
      session,
      'show_vars',
      choices = names(df),
      selected = if (input$bar) names(df),
      inline = TRUE
    )
  })
  
  output$table <- DT::renderDataTable(DT::datatable({
    data <- df[, input$show_vars, drop = FALSE]
    if (input$ent != "All") data <- data[data$Country == input$ent, ]
    if (input$continent != "All") data <- data[data$continent == input$continent, ]
    if (input$region != "All") data <- data[data$region == input$region, ]
    if (input$summarize) {
      data <- data %>% group_by(Country) %>% summarize(across(everything(), mean, na.rm = TRUE))
    }
    data
  }, options = list(paging = FALSE)))
}
