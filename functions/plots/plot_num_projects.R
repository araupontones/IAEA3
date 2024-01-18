
plot_num_projects <- function(.data,
                              theme,
                              y_variable = Improvement,
                              regroup = F,
                              num_projects_theme,
                              label_size = 5,
                              hjust = 1.5,
                              unit_title = "Projects"){
  unit_subtitle <- str_to_lower(unit_title)
  
  d <- .data
  
  if(regroup){
    
    d <- .data %>% group_by(Improvement, region) %>% summarise(projects = sum(projects), .groups = 'drop')
  }
  
  d %>%
  ggplot(
    aes(x = projects,
        y = reorder({{y_variable}}, projects),
        label = prettyNum(projects, big.mark = ','))
  ) +
    geom_col(fill = blue,
             width = .8
    ) +
    geom_text(hjust =hjust,
              size = label_size,
              family = "Open Sans",
              color = "white") +
    scale_y_discrete(label = label_wrap_gen(30)) +
    scale_fill_brewer(palette = 'Greens') +
    scale_x_continuous(labels = function(x)prettyNum(x, big.mark = ",")) +
    labs(title = glue("Number of {unit_title} Supported by the TCP in the 21st Century."),
         subtitle = glue('{theme}: Distribution of the {num_projects_theme} {unit_subtitle} supported.'),
         caption = "Source: Administrative Data.")+
    theme_main() 
  
}
