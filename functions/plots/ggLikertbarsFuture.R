#Plot likert bars

ggLikertbarsFuture <- function(.data,
                               titulo,
                               subtitulo){
  
  
  
  .data %>%
  ggplot(aes(x = perc,
          y = reorder(theme,significant),
          fill = likert)) +
  geom_col(width = .8) +
  geom_vline(xintercept = .5,
             linetype = 'dashed') +
  
  labs(title = titulo,
       subtitle = subtitulo)+
  scale_y_discrete(labels = label_wrap_gen(25)) +
  scale_x_continuous(expand = c(0,0),
                     labels = function(x)percent(x)) +
  scale_fill_brewer(breaks = rev(categories),
                    labels = rev(categories)) +
  theme_main()
  
}
