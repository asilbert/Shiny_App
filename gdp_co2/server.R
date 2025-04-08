function(input, output, session) {
  
  output$girafe_output <- renderGirafe({
    # data_id in the aes mapping
    
    if (input$radio == 1){
      df <- df %>% group_by(Year) %>% summarize(across(everything(),mean, na.rm=TRUE))
      
      
      data <- df
      plot <- data %>%
        ggplot(mapping = aes(
          x = Year,
          y = get(input$picker)
        ))
      
    }
    
    if (input$radio %in% c("continent","region")){
      df <- df %>% group_by_(input$radio, "Year") %>% summarize(across(everything(),mean, na.rm=TRUE))
      
      
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
        summarise(Entity = Entity,
                  slope = coef(model)[2]
        ) %>% arrange(desc(slope))
      
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
      
      country_list <- c(country_list_1,country_list_2)
      
      
      df <- df %>% filter(Entity %in% country_list | Entity %in% input$vselect)
      
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
    
    plot <- plot + geom_line_interactive(hover_nearest = TRUE) + theme_ipsum() + labs(color = str_to_title(input$radio), y=input$picker) +
      theme(axis.title.x = element_text(size=15))
    
    
    interactive_plot <- girafe(ggobj = plot)
    
    interactive_plot <- girafe_options(
      interactive_plot,
      opts_hover(css = "stroke:#69B3A2; stroke-width: 3px; transition: all 0.3s ease;"),
      opts_hover_inv("opacity:0.5;filter:saturate(10%);"),
      opts_toolbar(saveaspng = FALSE)
    )
    
    interactive_plot
    
    
  })
  
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
