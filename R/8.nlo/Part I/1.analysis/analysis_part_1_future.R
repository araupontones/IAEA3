library(rio)
library(ggplot2)
library(RColorBrewer)
library(extrafont)

future <- import('data/7.NLO/2.raw_formatted/Part_1_future.rds')
missing <- scales::percent(sum(is.na(future$likert)) / nrow(future))


glue('{missing} of the questions were missing')

#QA --------------------------------------------------------------------------

names(future)
qa <- future %>% 
  group_by(id) %>%
  summarise(questions = n(),
            unanswered = sum(is.na(likert)),
            perc_unanswered = unanswered/questions
  )

completed <- sum(qa$perc_unanswered == 0)
completed

data_plot <- future %>%
  filter(theme == "FOOD and AGRICULTURE") %>%
  filter(!is.na(likert)) %>%
  group_by(foa, region, likert) %>%
  summarise(total = n()) %>%
  ungroup() %>%
  group_by(foa, region) %>%
  mutate(answers = sum(total),
         perc= total/answers) %>%
  ungroup()




#plot
ggplot(data_plot,
       aes(
         x = perc,
         y = foa,
         fill = likert)) +
 geom_col() +
  scale_y_discrete(labels = label_wrap_gen(40)) +
  scale_x_continuous(labels = function(x)scales::percent(x)) +
  scale_fill_brewer(breaks = rev(categories),
                    labels = rev(categories)) +
  
  facet_wrap(~region) +
  theme(legend.position = 'bottom') +
labs(y = "",
       x = "",
       title = "Future",
       subtitle = "How important will the contribution of the TCP be in achieving results over the next 5-10 years."
       ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90),
        axis.text.y = element_text(hjust = 0, size = 8),
        legend.position = 'bottom',
        legend.title = element_blank(),
        plot.title.position = 'plot',
        text = element_text(family = "Open Sans"))


ggsave('plots/future_test.png',
       last_plot())
