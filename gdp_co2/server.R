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
      
      if (isTRUE(input$check == 0)) {
        country_list_1 <- head(models, input$num)$Entity
      } else if (isTRUE(input$check == 1)) {
        country_list_2 <- tail(models, input$num)$Entity
      } else if (sum(as.numeric(input$check)) == 1) {
        country_list_1 <- head(models, input$num)$Entity
        country_list_2 <- tail(models, input$num)$Entity
      }
      
      country_list <- c(country_list_1, country_list_2)
      
      
      data <- df %>% filter(Entity %in% country_list |
                            Entity %in% input$vselect)
      
   
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
      theme(axis.title.x = element_text(size = 15), aspect.ratio = 3/4)
    
    if (nrow(data %>% select(Entity) %>% unique()) > 10){
      plot <- plot + theme(legend.position="none")
    }
    
    
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
    
    
    models <- df %>% select(Entity, Year, input$picker_bar) %>% drop_na() %>%
      group_by(Entity) %>%
      do(model = lm(get(input$picker_bar) ~ Year, data = .)) %>%
      summarise(Entity = Entity, slope = coef(model)[2]) %>% arrange(desc(slope))

    country_list <- c("")
    country_list_1 <- c("")
    country_list_2 <- c("")


    if (isTRUE(input$check_bubble == 0)) {
      country_list_1 <- head(models, input$num_bubble)$Entity
    } else if (isTRUE(input$check_bubble == 1)) {
      country_list_2 <- tail(models, input$num_bubble)$Entity
    } else if (sum(as.numeric(input$check_bubble)) == 1) {
      country_list_1 <- head(models, input$num_bubble)$Entity
      country_list_2 <- tail(models, input$num_bubble)$Entity
    }

    country_list <- c(country_list_1, country_list_2)
    
  
    df <- df %>% group_by(Entity,continent,region) %>% summarize(across(everything(),mean, na.rm=TRUE))


    if (isTRUE(is.null(input$check_bubble))){
      plot_df <- df
    } else (plot_df <- df %>% filter(Entity %in% country_list))
    
    
    p1 <- ggplot(plot_df, aes(get(input$picker_x), get(input$picker_y), tooltip = paste("Country:", Entity,"<br>GDP per CO2:", gdp_per_co2
    ), data_id = Entity, size = get(input$size), fill = get(input$color))) +
      geom_point_interactive(alpha = 0.5, shape = 21, color = "black") + 
      theme(legend.position = "none", aspect.ratio = 3/4) + xlab(input$picker_x) + ylab(input$picker_y)
      
    
    
    # if (input$axes_switch){
    #   p1 <- p1 + xlim(input$x_axis[1],input$x_axis[2]) +
    #     ylim(input$y_axis[1], input$y_axis[2])
    #   
    # }
    
    
    
    temp_df <- head(df %>% arrange(desc(get(input$picker_bar))),15)
    p2 <- ggplot(temp_df, aes(x = reorder(temp_df$Entity, get(input$picker_bar)), y = get(input$picker_bar), 
                              tooltip = paste("Country:", temp_df$Entity,"<br>GDP per CO2:", get(input$picker_bar)), data_id = temp_df$Entity,
                              fill = temp_df$continent)) +
      geom_col_interactive() + coord_flip() + 
      scale_x_discrete(
        labels = function(x) str_wrap(x, width = 20),
        drop = FALSE
      ) + theme(axis.text.y=element_text(size=7, hjust=1 ),legend.position = "none", axis.title.y=element_blank(),
                axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), aspect.ratio = 4) + ylab(input$picker_bar)
    
    combined_plot <- p1 + p2
    
    girafe(ggobj = combined_plot,  options = list(
      opts_selection_inv("opacity:0.5;filter:saturate(10%);")
    ))})
  
  
  output$scatter_plot <- renderPlotly({
    models <- df %>% select(Entity, Year, input$picker_bar) %>% drop_na() %>%
      group_by(Entity) %>%
      do(model = lm(get(input$picker_bar) ~ Year, data = .)) %>%
      summarise(Entity = Entity, slope = coef(model)[2]) %>% arrange(desc(slope))

    country_list <- c("")
    country_list_1 <- c("")
    country_list_2 <- c("")


    if (isTRUE(input$check_bubble == 0)) {
      country_list_1 <- head(models, input$num_bubble)$Entity
    } else if (isTRUE(input$check_bubble == 1)) {
      country_list_2 <- tail(models, input$num_bubble)$Entity
    } else if (sum(as.numeric(input$check_bubble)) == 1) {
      country_list_1 <- head(models, input$num_bubble)$Entity
      country_list_2 <- tail(models, input$num_bubble)$Entity
    }

    country_list <- c(country_list_1, country_list_2)
    
    if (isTRUE(is.null(input$check_bubble))){
      plot_df <- df
      color_var = plot_df$continent
    } else {
      plot_df <- df %>% filter(Entity %in% country_list);
      color_var = plot_df$Entity
    }
    
    p <- ggplot(plot_df, aes(get(input$picker_x), get(input$picker_y), color = color_var)) +
      geom_point(aes(size = get(input$size), frame = Year, ids = Entity)) + 
      xlab(input$picker_x) + ylab(input$picker_y)

    fig <- ggplotly(p) %>%
      animation_opts(frame = 2000
      )

  })
  
  
  
  
  
  
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
