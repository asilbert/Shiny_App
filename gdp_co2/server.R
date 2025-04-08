function(input, output, session) {
  output$girafe_output_line <- renderGirafe({
    # data_id in the aes mapping
    
    if (input$radio == 1) {
      df <- df %>% group_by(Year) %>% summarize(across(everything(), mean, na.rm =
                                                         TRUE))
      data <- df
      plot <- data %>%
        ggplot(mapping = aes(x = Year, y = get(input$picker)))
      
    }
    
    if (input$radio %in% c("continent", "region")) {
      df <- df %>% group_by_(input$radio, "Year") %>% summarize(across(everything(), mean, na.rm =
                                                                         TRUE))
      data <- df
      plot <- data %>%
        
        ggplot(mapping = aes(
          x = Year,
          y = get(input$picker),
          color = get(input$radio),
          tooltip = get(input$radio),
          data_id = get(input$radio)
        ))
    }
    
    
    if (input$radio == "Entity") {
      models <- df %>% select(Entity, Year, input$picker2) %>% drop_na() %>%
        group_by(Entity) %>%
        do(model = lm(get(input$picker2) ~ Year, data = .)) %>%
        summarise(Entity = Entity, slope = coef(model)[2]) %>% arrange(desc(slope))
      
      country_list <- c("")
      country_list_1 <- c("")
      country_list_2 <- c("")
      
      # if (isTRUE(input$check.isnull)) {
      #   country_list <- c("")
      # }
      
      if (isTRUE(input$check == 0)) {
        country_list_1 <- head(models, input$num)$Entity
      } else if (isTRUE(input$check == 1)) {
        country_list_2 <- tail(models, input$num)$Entity
      } else if (sum(as.numeric(input$check)) == 1) {
        country_list_1 <- head(models, input$num)$Entity
        country_list_2 <- tail(models, input$num)$Entity
      }
      
      country_list <- c(country_list_1, country_list_2)
      
      
      df <- df %>% filter(Entity %in% country_list |
                            Entity %in% input$vselect)
      
      data <- df
      plot <- data %>%
        ggplot(mapping = aes(
          x = Year,
          y = get(input$picker),
          color = get(input$radio),
          tooltip = get(input$radio),
          data_id = get(input$radio)
        ))
      
    }
    
    plot <- plot + geom_line_interactive(hover_nearest = TRUE) + theme_ipsum() + labs(color = str_to_title(input$radio), y =
                                                                                        input$picker) +
      theme(axis.title.x = element_text(size = 15))
    
    
    interactive_plot <- girafe(ggobj = plot)
    
    interactive_plot <- girafe_options(
      interactive_plot,
      opts_hover(css = "stroke:#69B3A2; stroke-width: 3px; transition: all 0.3s ease;"),
      opts_hover_inv("opacity:0.5;filter:saturate(10%);"),
      opts_toolbar(saveaspng = FALSE)
    )
    
    interactive_plot
    
    
  })
  
  output$girafe_output_bubble <- renderGirafe({
    # data_id in the aes mapping
    
    df <- df %>% group_by(Entity,continent,region) %>% summarize(across(everything(),mean, na.rm=TRUE))
    
    p1 <- ggplot(df, aes(co2_growth, gdp_growth, tooltip = paste("Country:", df$Entity,"<br>GDP per CO2:", df$gdp_per_co2
    ), data_id = df$Entity, size = df$Density.n.P.Km2., fill = df$continent)) +
      geom_point_interactive(alpha = 0.5, shape = 21, color = "black") + theme(legend.position = "none")
    
    # data_id in the aes mapping
    
    temp_df <- head(df %>% arrange(desc(gdp_per_co2)),15)
    p2 <- ggplot(temp_df, aes(x = reorder(temp_df$Entity, temp_df$gdp_per_co2), y = gdp_per_co2, 
                              tooltip = paste("Country:", temp_df$Entity,"<br>GDP per CO2:", gdp_per_co2), data_id = temp_df$Entity,
                              fill = temp_df$continent)) +
      geom_col_interactive() + coord_flip() + 
      scale_x_discrete(
        labels = function(x) str_wrap(x, width = 20),
        drop = FALSE
      ) + theme(axis.text.y=element_text(size=7, hjust=1 ),legend.position = "none", axis.title.y=element_blank(),
                axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
    
    combined_plot <- p1 + p2
    
    combined_plot <- combined_plot + plot_layout(widths = c(10,2), heights = c(3,-1))
    
    # + theme(aspect.ratio=10/10)
    # + plot_layout(widths = unit(c(100,25), c("null","null")), heights = unit(c(1,1),c("null","null")))
    
    
    
    girafe(ggobj = combined_plot,  options = list(
      opts_selection_inv("opacity:0.5;filter:saturate(10%);")
    ))})
  
  
  
  
  
  
  observe({
    updateCheckboxGroupInput(
      session,
      'show_vars',
      choices = names(df),
      selected = if (input$bar)
        names(df),
      inline = TRUE
    )
  })
  
  # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable({
    data <- df[, input$show_vars, drop = FALSE]
    if (input$ent != "All") {
      data <- data[data$Entity == input$ent, ]
    }
    if (input$continent != "All") {
      data <- data[data$continent == input$continent, ]
    }
    if (input$region != "All") {
      data <- data[data$region == input$region, ]
    }
    data
  }, options = list(paging = FALSE)))
  
}
