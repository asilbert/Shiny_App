#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define server logic required to draw a histogram
library(ggplot2)

function(input, output, session) {
  
  observe({
    updateCheckboxGroupInput(
      session, 'show_vars', choices = names(df),
      selected = if (input$bar) names(df), inline = TRUE
    )
  })
  
  # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable({
    
    
    
    data <- df[, input$show_vars, drop = FALSE]
    if (input$ent != "All") {
      data <- data[data$Entity == input$ent,]
    }
    if (input$continent != "All") {
      data <- data[data$continent == input$continent,]
    }
    if (input$region != "All") {
      data <- data[data$region == input$region,]
    }
    data
  }, options = list(paging = FALSE)))
  
}
