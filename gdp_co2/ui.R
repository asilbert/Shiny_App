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
    tabItem(tabName = "dashboard", 
            fluidRow(
              box(
                title = "🌍 Project Overview", 
                width = 12, 
                status = "primary", 
                solidHeader = TRUE,
                h4("Reducing Emissions Without Sacrificing Growth")
              )
            ),
            
            fluidRow(
              box(
                title = "🎯 Project Goal", 
                width = 12, 
                status = "success", 
                solidHeader = TRUE,
                p("Develop a data-driven methodology to identify which countries are achieving the greatest impact on Greenhouse Gas (GHG) reduction while maintaining high levels of economic output."),
                p("Additionally, the project aims to forecast the next potential leader in GHG-efficient economic performance based on current trends.")
              )
            ),
            
            fluidRow(
              box(
                title = "📊 Project Summary", 
                width = 12, 
                status = "info", 
                solidHeader = TRUE,
                p("A common criticism of climate policy is the perceived trade-off between environmental responsibility and economic productivity."),
                p("This analysis challenges that assumption by identifying nations that successfully reduce emissions without compromising growth."),
                p("By highlighting these top-performing countries, the project provides evidence-based examples for how climate progress and prosperity can go hand-in-hand — offering a benchmark for future global policy.")
              )
            )
            
            
            
            
            
            
            ),
    
    
    
    
    
    
    
    tabItem(tabName = "charts", tabsetPanel(
      tabPanel(
        "Line Chart",
        
        box(
          pickerInput(
            inputId = "picker",
            label = "Variable",
            choices = sort(names(df[, !(names(df) %in% drops)])),
            options = pickerOptions(container = "body", liveSearch = TRUE),
            width = "100%",
            selected = "GDP per CO2"
          ),
          radioGroupButtons(
            inputId = "radio",
            label = "Group Filter",
            choices = list(
              "Global" = 1,
              "Continental" = "continent",
              "Regional" = "region",
              "Country" = "Country"
            )
          ),
          collapsible = TRUE,
          width = 4
        ),
        
        box(
          layout_columns(
            layout_columns(
              virtualSelectInput(
                inputId = "vselect",
                label = "Country choices :",
                choices = list(
                  Africa = unlist(
                    df %>% filter(continent == "Africa") %>% select(Country) %>% unique(),
                    use.names = FALSE
                  ),
                  Americas = unlist(
                    df %>% filter(continent == "Americas") %>% select(Country) %>% unique(),
                    use.names = FALSE
                  ),
                  Asia = unlist(
                    df %>% filter(continent == "Asia") %>% select(Country) %>% unique(),
                    use.names = FALSE
                  ),
                  Europe = unlist(
                    df %>% filter(continent == "Europe") %>% select(Country) %>% unique(),
                    use.names = FALSE
                  ),
                  Oceania = unlist(
                    df %>% filter(continent == "Oceania") %>% select(Country) %>% unique(),
                    use.names = FALSE
                  )
                ),
                multiple = TRUE,
                width = "100%",
                dropboxWrapper = "body"
              ),
              
              virtualSelectInput(
                inputId = "vselect1",
                label = "Country Search",
                choices = df %>% select(Country) %>% unique(),
                search = TRUE,
                markSearchResults = TRUE,
                width = "100%",
                dropboxWrapper = "body",
                hideClearButton = FALSE,
                multiple = TRUE,
                disableOptionGroupCheckbox = TRUE,
                disableSelectAll = TRUE
                
              ),
              col_widths = c(12, 12)
            ),
            layout_columns(
              pickerInput(
                inputId = "picker2",
                label = "Growth Filter Variable",
                choices = sort(names(df[, !(names(df) %in% drops)])),
                options = pickerOptions(container = "body", liveSearch = TRUE),
                width = "100%",
                selected = "GDP per CO2"
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
              col_widths = c(4, 2, 2)
            )
          ),
          
          collapsible = TRUE,
          width = 8
        ),
        
        box(
          girafeOutput('girafe_output_line', width = "100%", height = "90%"),
          width = 12
        )
      ),
      
      tabPanel(
        "Bubble Plot",
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
          column(
            width = 10,
            pickerInput(
              inputId = "picker_bar",
              label = "Bar - Variable",
              choices = sort(names(df[, !(names(df) %in% drops)])),
              options = pickerOptions(container = "body", liveSearch = TRUE),
              width = "100%",
              selected = "GDP per CO2"
            )
          ),
          checkboxInput(
            inputId = "invert",
            label = "Invert",
            value = FALSE
          ),
          col_widths = c(12, 12),
          collapsible = TRUE,
          title = "Variable Selection"
        ),
        
        box(
          layout_columns(
            prettyCheckboxGroup(
              inputId = "check_bubble",
              label = "Growth Filter",
              choices = list("Fastest" = 0, "Slowest" = 1)
            ),
            numericInput("num_bubble", label = "Number for Filter", value = 5),
            selectInput(
              inputId = 'color',
              label = 'Color',
              choices = drops,
              selected = 'continent'
            ),
            selectInput(
              inputId = 'size',
              label = 'Size',
              choices = names(df %>% select(-c(Country, Year))),
              selected = "Density (P/Km2)"
            ),
            col_widths = c(1, 1, 1)
          ),
          
          checkboxInput(
            inputId = "logx",
            label = "Log Scale X-Axis",
            value = FALSE
          ),
          
          collapsible = TRUE,
          title = "Plot Options"
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
        switchInput(
          inputId = "summarize",
          label = "Summarize",
          labelWidth = "80px"
        ),
        
        fluidRow(
          column(4, selectInput("ent", "Country:", c(
            "All", unique(as.character(df$Country))
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
      
    ))
  ), )
)