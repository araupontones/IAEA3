
library(ggplot2)
library(tidyverse)
library(rio)
library(janitor)
gmdacr::load_functions('functions')
gmdacr::load_functions('functions/themes/')
lkp_countries <- import('data/9.lookups/countries.rds')

library(rnaturalearth)
library(rnaturalearthdata)
source('functions/clean_countries.R')

#import world map
world <- world_sf()
#get responses by role and survey
responses <- get_responses()




#check that countries responded to which
responses2 <- responses_surveys()


data_plot <- responses2 %>%
  full_join(world, by = 'country'
            ) %>%
  sf::st_as_sf() %>%
  mutate(#status = ifelse(is.na(status), 'N/A', status),
         status = factor(status,
                     levels =c('all_surveys', 'only_cp',
                            'cp_nlo1', 'cp_nlo2',
                            'nlos_no_cp', 'only_nlo2'
                            ),
                     ordered = T)
         )


names(data_plot)

ggplot(data = data_plot,
       aes(fill = status)) +
  geom_sf() +
  scale_fill_brewer(palette = 'Set3',direction=-1) +
  theme_bw()+
  theme_main()+
  labs(title = "Participation to the Data Collection Instruments.") +
  theme(legend.position = 'bottom',
        plot.title = element_text(size = 12),
        legend.title = element_blank(),
        axis.text.x = element_blank())
 
theme_main
ggsave('report/sample/map_all.png', last_plot())

export()

