#themes

theme_main <- function(){

theme( axis.text.y = element_text(hjust = 0),
       axis.title.y = element_blank(),
       axis.title.x = element_blank(),
       axis.ticks = element_blank(),
       legend.position = 'bottom',
       legend.title = element_blank(),
       panel.background = element_rect(fill = 'white'),
       plot.background = element_rect(fill = 'white'),
       plot.title.position = 'plot',
       plot.title = element_text(size = 16),
       plot.subtitle =  element_text(size = 14),
       text = element_text(family = "Open Sans"),
       
)
}
