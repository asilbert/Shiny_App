dashboardPage(
  dashboardHeader(title = "GDP per CO2"),
  dashboardSidebar(sidebarMenu(
    menuItem(
      "Dashboard",
      tabName = "dashboard",
      icon = icon("dashboard")
    ),
    menuItem("Charts", tabName = "charts", icon = icon("bar-chart-o")),
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
            choices = names(df),
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
              choices = names(df),
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
        girafeOutput('girafe_output_line', width = "100%")
      ),
      
      tabPanel(
        "Bubble Plot",
        girafeOutput('girafe_output_bubble', width = "100%", height = "100%")
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