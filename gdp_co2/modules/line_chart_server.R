library(dplyr)
library(ggplot2)
library(ggiraph)
library(hrbrthemes)
library(stringr)

line_chart_server <- function(input, output, session, df, drops) {
  output$girafe_output_line <- renderGirafe({
    data <- NULL
    plot <- NULL
    
    # Prepare plot data depending on grouping selection
    if (input$radio == 1) {
      # Global average over years
      data <- df %>% group_by(Year) %>% summarize(across(everything(), mean, na.rm = TRUE))
      plot <- ggplot(data, aes(x = Year, y = get(input$picker)))
    }
    
    # Average by continent or region
    if (input$radio %in% c("continent", "region")) {
      data <- df %>% group_by(!!sym(input$radio), Year) %>% summarize(across(everything(), mean, na.rm = TRUE))
      plot <- ggplot(data, aes(
        x = Year,
        y = get(input$picker),
        color = get(input$radio),
        tooltip = get(input$radio),
        data_id = get(input$radio)
      ))
    }
    
    if (input$radio == "Country") {
      # Calculate slope models for growth filtering
      models <- df %>% select(Country, Year, input$picker2) %>% drop_na() %>%
        group_by(Country) %>%
        do(model = lm(get(input$picker2) ~ Year, data = .)) %>%
        summarise(Country = Country, slope = coef(model)[2]) %>%
        arrange(desc(slope))
      
      country_list_1 <- c()
      country_list_2 <- c()
      
      # Select top/bottom countries by slope based on user selection
      if (isTRUE(input$check == 0)) {
        country_list_1 <- head(models, input$num)$Country
      }
      if (isTRUE(input$check == 1)) {
        country_list_2 <- tail(models, input$num)$Country
      }
      
      country_list <- c(country_list_1, country_list_2)
      
      # Filter data for selected countries (including those selected via inputs)
      data <- df %>%
        filter(Country %in% country_list |
                 Country %in% input$vselect |
                 Country %in% input$vselect1)
      
      # Add interactive line geometry and styling
      plot <- ggplot(data, aes(
        x = Year,
        y = get(input$picker),
        color = Country,
        tooltip = Country,
        data_id = Country
      ))
    }
    
    plot <- plot +
      geom_line_interactive(hover_nearest = TRUE) +
      theme_ipsum() +
      labs(color = str_to_title(input$radio), y = input$picker) +
      theme(axis.title.x = element_text(size = 15), aspect.ratio = 3/4)
    
    # Hide legend if too many lines to avoid clutter
    if (nrow(unique(data["Country"])) > 10) {
      plot <- plot + theme(legend.position = "none")
    }
    # Convert to interactive girafe plot with hover options
    interactive_plot <- girafe(ggobj = plot)
    
    girafe_options(
      interactive_plot,
      opts_hover(css = "stroke:#69B3A2; stroke-width: 3px; transition: all 0.3s ease;"),
      opts_hover_inv("opacity:0.5;filter:saturate(10%);"),
      opts_toolbar(saveaspng = FALSE)
    )
  })
}
