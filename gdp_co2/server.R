
library(shiny)
library(dplyr)
library(ggplot2)
library(ggiraph)
library(plotly)

# Source modules
source("modules/line_chart_server.R")
source("modules/bubble_plot_server.R")

server <- function(input, output, session) {

  # Use reactive() for reusable filtered data
  filtered_data <- reactive({
    data <- df
    if (!"All" %in% input$ent) data <- data[data$Country %in% input$ent, ]
    if (input$continent != "All") data <- data[data$continent == input$continent, ]
    if (input$region != "All") data <- data[data$region == input$region, ]
    data
  })

  # Observe "All" toggle
  observeEvent(input$ent, {
    if ("All" %in% input$ent && length(input$ent) > 1) {
      new_selection <- setdiff(input$ent, "All")
      updateSelectInput(session, "ent", selected = new_selection)
    }
  })

  # Dynamic checkbox group for columns
  observe({
    updateCheckboxGroupInput(
      session,
      "show_vars",
      choices = names(df),
      selected = if (input$bar) names(df) else character(0),
      inline = TRUE
    )
  })

  # Render DataTable with optional summarization
  output$table <- DT::renderDataTable({
    data <- filtered_data()
    if (input$summarize) {
      data <- data %>% group_by(Country) %>% summarize(across(everything(), mean, na.rm = TRUE))
    }
    DT::datatable(data[, input$show_vars, drop = FALSE], options = list(paging = FALSE))
  })

  # Call modular server functions
  line_chart_server(input, output, session, df, drops)
  bubble_plot_server(input, output, session, df, drops)
}
