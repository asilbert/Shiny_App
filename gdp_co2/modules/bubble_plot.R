library(shinyWidgets)
library(ggiraph)

tagList(
  box(
    pickerInput(
      inputId = "picker_x",
      label = "X - Variable",
      choices = sort(names(df[, !(names(df) %in% drops)])),
      options = pickerOptions(container = "body", liveSearch = TRUE),
      width = "100%",
      selected = "CO2 Growth"
    ),
    pickerInput(
      inputId = "picker_y",
      label = "Y - Variable",
      choices = sort(names(df[, !(names(df) %in% drops)])),
      options = pickerOptions(container = "body", liveSearch = TRUE),
      width = "100%",
      selected = "GDP Growth"
    ),
    pickerInput(
      inputId = "picker_bar",
      label = "Bar - Variable",
      choices = sort(names(df[, !(names(df) %in% drops)])),
      options = pickerOptions(container = "body", liveSearch = TRUE),
      width = "100%",
      selected = "GDP per CO2"
    ),
    checkboxInput("invert", "Invert", FALSE),
    collapsible = TRUE,
    title = "Variable Selection",
    width = 6
  ),
  
  box(
    layout_columns(
      prettyCheckboxGroup(
        inputId = "check_bubble",
        label = "Growth Filter",
        choices = list("Fastest" = 0, "Slowest" = 1)
      ),
      numericInput("num_bubble", "Number for Filter", value = 5),
      selectInput("color", "Color", choices = drops, selected = "continent"),
      selectInput("size", "Size", choices = names(df %>% select(-c(Country, Year))), selected = "Density (P/Km2)"),
      col_widths = c(1, 1, 1)
    ),
    checkboxInput("logx", "Log Scale X-Axis", FALSE),
    collapsible = TRUE,
    title = "Plot Options",
    width = 6
  ),
  
  box(
    girafeOutput('girafe_output_bubble', width = "100%", height = "auto"),
    width = 12
  ),
  
  box(
    plotlyOutput("scatter_plot", width = "100%", height = "600px"),
    width = 12
  )
)
