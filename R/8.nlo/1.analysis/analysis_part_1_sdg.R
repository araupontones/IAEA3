library(rio)
library(ggplot2)
library(RColorBrewer)
library(extrafont)
library(glue)
library(dplyr)

sdg <- import('data/7.NLO/2.raw_formatted/Part_1_sdgs.rds')
missing <- scales::percent(sum(is.na(sdg$sdg)) / nrow(sdg))


glue('{missing} of the questions were missing')

#QA --------------------------------------------------------------------------

names(sdg)
qa <- sdg %>% 
  group_by(id) %>%
  summarise(questions = n(),
            unanswered = sum(is.na(sdg)),
            perc_unanswered = unanswered/questions
  )

empty <- sum(qa$perc_unanswered == 1)
empty

data_plot <- sdg %>%
  filter(!is.na(sdg)) %>%
  group_by(region, sdg, area) %>%
  summarise(total = n()) %>%
  ungroup() %>%
  group_by(region,area) %>%
  mutate(answers = sum(total),
         perc= total/answers)
  


#plot
ggplot(data_plot,
       aes(
         y = reorder(sdg,perc),
         x = perc,
         fill = area)) +
 geom_col(position = position_dodge()) +
  facet_wrap(~region) +
  scale_x_continuous(labels = function(x)scales::percent(x),
                     expand = c(0,0)) +
  scale_fill_brewer(labels = c("In Achieving SDG", 
                               "In Reporting on SDG Progress")) +
  theme(legend.position = 'bottom') +
labs(y = "",
       x = "",
       title = "CONTRIBUTION TO SUSTAINABLE DEVELOPMENT GOALS",
       subtitle = "Contribution of nuclear technologies supported by TC programme."
       ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5),
        axis.text.y = element_text(hjust = 0, size = 8),
        legend.position = 'bottom',
        legend.title = element_blank(),
        plot.title.position = 'plot',
        text = element_text(family = "Open Sans"))


ggsave('plots/sdg_test.png',
       last_plot())
