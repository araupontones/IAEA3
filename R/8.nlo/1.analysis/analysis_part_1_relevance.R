library(rio)
library(ggplot2)
library(RColorBrewer)
library(extrafont)

relevance <- import('data/7.NLO/2.raw_formatted/Part_1_relevance.rds')
missing <- scales::percent(sum(is.na(relevance$value)) / nrow(relevance))


glue('{missing} of the questions were missing')

#QA --------------------------------------------------------------------------

names(relevance)
qa <- relevance %>% 
  group_by(id) %>%
  summarise(questions = n(),
            unanswered = sum(is.na(value)),
            perc_unanswered = unanswered/questions
  )
            
completed <- sum(qa$perc_unanswered == 0)
completed



#point jitter ----------------------------------------------------------------

data_jitter <- relevance %>%
  filter(theme == "FOOD and AGRICULTURE") %>%
  filter(!is.na(value),
         value != "Not applicable")



ggplot(data_jitter,
       aes(x = period,
           y = value,
           color= value))+
  geom_point(position = position_jitter(.3), 
             alpha = .8) +
  facet_wrap(~region) + 
  scale_color_brewer() +
  guides(color=guide_legend(ncol=2,
                            override.aes = list(size = 4))) +
  labs(y = "",
       x = "",
       title = "Relevance",
       subtitle = "Degree to which the TCP has contributed in achieving results."
  ) +
  theme_minimal() +
  theme(plot.title.position = "plot",
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 90),
        axis.text.y = element_text(hjust = 0),
        legend.position = 'bottom',
        text = element_text(family = "Open Sans"),
        strip.background = element_rect(fill = '#F1F3F4'),
        strip.text = element_text(size = 8)
  ) 



ggsave('plots/relevance_test.png',
       last_plot())

#test tiles ==================================================================

data_plot_tiles <- relevance %>%
  filter(!is.na(value)) %>%
  group_by(value, period) %>%
  summarise(total = n())


ggplot(data_plot,
       aes(x = period,
           y = value,
           fill = total)
) +
  geom_tile() +
  scale_fill_distiller(palette = "Blues", direction = 1) +
  labs(y = "Stage",
       x = "Period",
       title = "Relevance",
       subtitle = "Degree to which the TC Programme has contributed") +
  theme(axis.text.x = element_text(angle = 90),
        plot.title.position = 'plot')



#check all callors
display.brewer.all()
?geom_point

