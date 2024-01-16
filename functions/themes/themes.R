#themes
library(extrafont)


#colors
blue <- "#01558B"
blue_navy <- '#183668'
blue_sky <- "#007DBC"
blue_light <- "#00AED9"
gray_dark <- "#4D4D4D"
gray_light <- "#F2F2F2"
purple_bright <- "#E11484"
yellow <- "#FDB713"
green <- "#279B48"
green_light <- "#5DBB46"
red <- "#EB1C2D"



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
       plot.subtitle =  element_text(size = 14, color = '#757575'),
       text = element_text(family = "Open Sans"),

)
}
