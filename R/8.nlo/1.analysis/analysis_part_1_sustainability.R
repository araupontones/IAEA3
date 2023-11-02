library(rio)
library(ggplot2)
library(RColorBrewer)
library(extrafont)

sustainability <- import('data/7.NLO/2.raw_formatted/Part_1_sustainability.rds')
missing <- scales::percent(sum(is.na(sustainability$period)) / nrow(sustainability))


glue('{missing} of the questions were missing')

#QA --------------------------------------------------------------------------

names(sustainability)
qa <- sustainability %>% 
  group_by(id) %>%
  summarise(questions = n(),
            unanswered = sum(is.na(period)),
            perc_unanswered = unanswered/questions
  )

completed <- sum(qa$perc_unanswered == 0)
completed

data_plot <- sustainability %>%
  filter(theme == "FOOD and AGRICULTURE") %>%
  filter(!is.na(period),
         period != "N/A") %>%
  group_by(theme, improvement, period, region) %>%
  summarise(total = n())




#plot
ggplot(data_plot,
       aes(
         y = improvement,
         x = period,
         fill = total)) +
 geom_tile() +
  labs(y = "",
       x = "",
       title = "Sustainability",
       subtitle = "Estimated period when the improvements introduced by TCP produced tangible results."
       ) +
  scale_y_discrete(labels = label_wrap(35)) +
  scale_fill_distiller(palette = "Blues", direction = 1) +
  facet_wrap(~region) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90),
        axis.text.y = element_text(hjust = 0, size = 8),
        legend.position = 'bottom',
        legend.title = element_blank(),
        plot.title.position = 'plot',
        text = element_text(family = "Open Sans"))


ggsave('plots/sustainability_test.png',
       last_plot())
