library(dplyr)
library(ggplot2)
library(ggiraph)
library(patchwork)

bubble_plot_server <- function(input, output, session, df, drops) {
  output$girafe_output_bubble <- renderGirafe({
    
    models <- df %>% select(Country, Year, input$picker_bar) %>% drop_na() %>%
      group_by(Country) %>%
      do(model = lm(get(input$picker_bar) ~ Year, data = .)) %>%
      summarise(Country = Country, slope = coef(model)[2]) %>%
      arrange(desc(slope))
    
    country_list_1 <- c()
    country_list_2 <- c()
    
    if (isTRUE(input$check_bubble == 0)) {
      country_list_1 <- head(models, input$num_bubble)$Country
    }
    if (isTRUE(input$check_bubble == 1)) {
      country_list_2 <- tail(models, input$num_bubble)$Country
    }
    
    country_list <- c(country_list_1, country_list_2)
    
    df_grouped <- df %>%
      group_by(Country, continent, region) %>%
      summarize(across(everything(), mean, na.rm = TRUE))
    
    plot_df <- if (is.null(input$check_bubble)) df_grouped else df_grouped %>% filter(Country %in% country_list)
    
    p1 <- ggplot(plot_df, aes(get(input$picker_x), get(input$picker_y),
                              tooltip = paste("Country:", Country, "<br>", input$picker_bar, ":", get(input$picker_bar)),
                              data_id = Country,
                              size = get(input$size),
                              fill = get(input$color))) +
      geom_point_interactive(alpha = 0.5, shape = 21, color = "black") +
      theme(legend.position = "none", aspect.ratio = 3/4) +
      xlab(input$picker_x) + ylab(input$picker_y) + geom_hline(yintercept = 0) + geom_vline(xintercept = 0)
    
    if (input$logx) {
      p1 <- p1 + scale_x_continuous(trans = 'log')
    }
    
    temp_df <- head(plot_df %>% arrange(desc(get(input$picker_bar))), 15)
    
    if (input$invert) {
      temp_df <- head(plot_df %>% arrange(get(input$picker_bar)), 15)
    }
    
    p2 <- ggplot(temp_df, aes(
      x = reorder(temp_df$Country, get(input$picker_bar)),
      y = get(input$picker_bar),
      tooltip = paste("Country:", temp_df$Country, "<br>", input$picker_bar, ":", get(input$picker_bar)),
      data_id = temp_df$Country,
      fill = get(input$color)
    )) +
      geom_col_interactive() + coord_flip() +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 20), drop = FALSE) +
      theme(axis.text.y = element_text(size = 7, hjust = 1), legend.position = "none",
            axis.title.y = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
            aspect.ratio = 4) +
      ylab(input$picker_bar)
    
    combined_plot <- p1 + p2
    
    girafe(ggobj = combined_plot, options = list(opts_selection_inv("opacity:0.5;filter:saturate(10%);")))
  })
  
  output$scatter_plot <- renderPlotly({
    
    models <- df %>% select(Country, Year, input$picker_bar) %>% drop_na() %>%
      group_by(Country) %>%
      do(model = lm(get(input$picker_bar) ~ Year, data = .)) %>%
      summarise(Country = Country, slope = coef(model)[2]) %>%
      arrange(desc(slope))
    
    country_list_1 <- c()
    country_list_2 <- c()
    
    if (isTRUE(input$check_bubble == 0)) {
      country_list_1 <- head(models, input$num_bubble)$Country
    }
    if (isTRUE(input$check_bubble == 1)) {
      country_list_2 <- tail(models, input$num_bubble)$Country
    }
    
    country_list <- c(country_list_1, country_list_2)
    
    plot_df <- if (is.null(input$check_bubble)) df else df %>% filter(Country %in% country_list)
    
    p <- ggplot(plot_df, aes(get(input$picker_x), get(input$picker_y), color = get(input$color),
                             text = paste(input$picker_x, ": ", get(input$picker_x), "<br>",
                                          input$picker_y, ": ", get(input$picker_y), "<br>",
                                          str_to_title(input$color), ": ", get(input$color), "<br>",
                                          input$size, ": ", get(input$size), "<br>",
                                          "Year: ", Year, "<br>",
                                          "Country: ", Country))) +
      geom_point(aes(size = get(input$size), frame = Year, ids = Country)) +
      xlab(input$picker_x) + ylab(input$picker_y) + labs(color = "", size = "")
    
    if (input$logx) {
      p <- p + scale_x_continuous(trans = 'log')
    }
    
    ggplotly(p, tooltip = "text") %>%
      animation_opts(frame = 2000, transition = 1500)
  })
}
