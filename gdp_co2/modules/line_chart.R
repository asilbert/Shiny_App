library(shinyWidgets)
library(ggiraph)

tagList(
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
          label = "Country choices:",
          choices = list(
            Africa = unique(df$Country[df$continent == "Africa"]),
            Americas = unique(df$Country[df$continent == "Americas"]),
            Asia = unique(df$Country[df$continent == "Asia"]),
            Europe = unique(df$Country[df$continent == "Europe"]),
            Oceania = unique(df$Country[df$continent == "Oceania"])
          ),
          multiple = TRUE,
          width = "100%",
          dropboxWrapper = "body"
        ),
        virtualSelectInput(
          inputId = "vselect1",
          label = "Country Search",
          choices = unique(df$Country),
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
          numericInput("num", label = "Number for Filter", value = 5),
          col_widths = c(4, 2)
        ),
        col_widths = c(4, 6)
      )
    ),
    collapsible = TRUE,
    width = 8
  ),
  box(
    girafeOutput('girafe_output_line', width = "100%", height = "90%"),
    width = 12
  )
)
