dashboardPage(
  dashboardHeader(title = "GDP per CO2"),
  dashboardSidebar(sidebarMenu(
    menuItem(
      "Dashboard",
      tabName = "dashboard",
      icon = icon("dashboard")
    ),
    menuItem(
      "Charts",
      tabName = "charts",
      icon = icon("chart-simple")
    ),
    menuItem("Tables", tabName = "tables", icon = icon("th"))
  )),
  dashboardBody(tabItems(
    #First tab content
    tabItem(tabName = "dashboard", h2("TEMP DASHBOARD")),
    tabItem(tabName = "charts", tabsetPanel(
      tabPanel(
        "Line Chart",
        
        layout_columns(
          pickerInput(
            inputId = "picker",
            label = "Variable",
            choices = sort(names(df[,!(names(df) %in% drops)])),
            options = pickerOptions(container = "body", liveSearch = TRUE),
            width = "100%",
            selected = "gdp_per_co2"
          ),
          radioGroupButtons(
            inputId = "radio",
            label = "Group Filter",
            choices = list(
              "Global" = 1,
              "Continental" = "continent",
              "Regional" = "region",
              "Country" = "Entity"
            ),
            direction = "vertical"
          ),
          
          
          layout_columns(
            virtualSelectInput(
              inputId = "vselect",
              label = "Country choices :",
              choices = list(
                Africa = unlist(
                  df %>% filter(continent == "Africa") %>% select(Entity) %>% unique(),
                  use.names = FALSE
                ),
                Americas = unlist(
                  df %>% filter(continent == "Americas") %>% select(Entity) %>% unique(),
                  use.names = FALSE
                ),
                Asia = unlist(
                  df %>% filter(continent == "Asia") %>% select(Entity) %>% unique(),
                  use.names = FALSE
                ),
                Europe = unlist(
                  df %>% filter(continent == "Europe") %>% select(Entity) %>% unique(),
                  use.names = FALSE
                ),
                Oceania = unlist(
                  df %>% filter(continent == "Oceania") %>% select(Entity) %>% unique(),
                  use.names = FALSE
                )
              ),
              multiple = TRUE,
              width = "100%",
              dropboxWrapper = "body"
            ),
            pickerInput(
              inputId = "picker2",
              label = "Growth Filter Variable",
              choices = sort(names(df[,!(names(df) %in% drops)])),
              options = pickerOptions(container = "body", liveSearch = TRUE),
              width = "100%",
              selected = "gdp_per_co2"
            ),
            layout_columns(
              prettyCheckboxGroup(
                inputId = "check",
                label = "Growth Filter",
                choices = list("Fastest" = 0, "Slowest" = 1),
                selected = 0
              ),
              numericInput("num", label = "Number for Filter", value = 5)
            ),
            col_widths = c(1, 1)
          )
          
        ),
        girafeOutput('girafe_output_line', width = "100%", height = "90%")
      ),
      
      tabPanel(
        "Bubble Plot",
        box(
          pickerInput(
            inputId = "picker_x",
            label = "X - Variable",
            choices = names(df),
            options = pickerOptions(container = "body", liveSearch = TRUE),
            width = "100%",
            selected = "co2_growth"
          ),
          pickerInput(
            inputId = "picker_y",
            label = "Y - Variable",
            choices = names(df),
            options = pickerOptions(container = "body", liveSearch = TRUE),
            width = "100%",
            selected = "gdp_growth"
          ),
          pickerInput(
            inputId = "picker_bar",
            label = "Bar - Variable",
            choices = names(df),
            options = pickerOptions(container = "body", liveSearch = TRUE),
            width = "100%",
            selected = "gdp_per_co2"
          ),
          col_widths = c(12, 12), collapsible = TRUE, title = "Variable Selection"
        ),
        
        box(
          # layout_columns(
          # switchInput(
          #   inputId = "year_switch_bubble",
          #   label = "By Year",
          #   labelWidth = "80px"
          # ),
          # sliderInput(
          #   "year_slider_bubble",
          #   "Year",
          #   min = min(df$Year),
          #   max = max(df$Year),
          #   value = min(df$Year),
          #   step = 1,
          #   sep = "",
          #   animate = TRUE
          # )
          # 
          # ),
          # layout_columns(
          #   switchInput(
          #     inputId = "axes_switch",
          #     label = "Lock Axes",
          #     labelWidth = "80px"
          #   ),
          # numericRangeInput(
          #   "x_axis",
          #   "X Axis",
          #   c(-100,100),
          #   separator = " to "
          # ),
          # numericRangeInput(
          #   "y_axis",
          #   "Y Axis",
          #   c(-100,100),
          #   separator = " to "
          # )),

          
          layout_columns(
            prettyCheckboxGroup(
              inputId = "check_bubble",
              label = "Growth Filter",
              choices = list("Fastest" = 0, "Slowest" = 1)
            ),
            numericInput("num_bubble", label = "Number for Filter", value = 5)
          ),
          
          collapsible = TRUE, title = "Plot Options"
        ),
            box(
              dropdownButton(
                
                tags$h3("List of Inputs"),
                
                selectInput(inputId = 'color',
                            label = 'Color',
                            choices = names(df),
                            selected = 'continent'
                            ),
                
                selectInput(inputId = 'size',
                            label = 'Size',
                            choices = names(df),
                            selected = "Density.n.P.Km2."),
                
                circle = TRUE,
                icon = icon("gear"),
                
                tooltip = tooltipOptions(title = "Click for more inputs")
              ),
              
              
              girafeOutput('girafe_output_bubble', width = "100%", height = "100%"), width = 12),
        
        box(
        plotlyOutput("scatter_plot", width = "100%", height = "100%"), width = 12)
      )
    ), ),
    tabItem(tabName = "tables", tabsetPanel(
      tabPanel(
        "Table",
        checkboxGroupInput(
          "show_vars",
          "Columns to show:",
          names(df),
          selected = names(df),
          inline = TRUE
        ),
        checkboxInput('bar', 'All/None'),
        
        fluidRow(
          column(4, selectInput("ent", "Entity:", c(
            "All", unique(as.character(df$Entity))
          ))),
          column(4, selectInput(
            "continent", "continent:", c("All", unique(as.character(df$continent)))
          )),
          column(4, selectInput("region", "region:", c(
            "All", unique(as.character(df$region))
          )))
        ),
        # Create a new row for the table.
        div(style = 'overflow-x: scroll', DT::dataTableOutput("table"))
      ),
      tabPanel("TEST")
    ))
  ), )
)