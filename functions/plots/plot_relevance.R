plot_relevance <- function(.data,
                           titulo = "% of NLOs that consider that the TCP has contributed in results in their countries.",
                           subtitulo = "Between countries"){
  
  
  .data %>% 
    ggplot(
      aes(x = value,
          y = reorder(theme,sort),
          fill = name
      )
    ) +
    geom_col(position =  'dodge') +
    lapply(c(.25,.5,.75), function(x){
      
      geom_vline(xintercept = x,
                 linetype = 'dashed')
    })+
    scale_y_discrete(label = label_wrap_gen(25)) +
    scale_fill_brewer(breaks = categories,
                      labels = rev(c( "Initial or Established Stage",
                                      "Master Stage or Regional Reference")
                      )) +
    scale_x_continuous(labels = scales::percent,
                       limits = c(0,1)) +
    labs(title = titulo,
         subtitle = subtitulo)+
    theme_main()
  
}
