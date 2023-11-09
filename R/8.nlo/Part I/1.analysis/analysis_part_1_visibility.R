library(rio)
library(ggplot2)
library(RColorBrewer)
library(extrafont)

visibility <- import('data/7.NLO/2.raw_formatted/Part_1_visibility.rds')
missing <- scales::percent(sum(is.na(visibility$likert)) / nrow(visibility))


glue('{missing} of the questions were missing')

#QA --------------------------------------------------------------------------

names(visibility)
qa <- visibility %>% 
  group_by(id) %>%
  summarise(questions = n(),
            unanswered = sum(is.na(likert)),
            perc_unanswered = unanswered/questions
  )

empty <- sum(qa$perc_unanswered == 1)
empty

data_plot <- visibility %>%
  filter(theme == "FOOD and AGRICULTURE") %>%
  filter(!is.na(likert)) %>%
  group_by(foa, region, likert) %>%
  summarise(total = n()) %>%
  ungroup() %>%
  group_by(foa, region) %>%
  mutate(answers = sum(total),
         perc= total/answers) %>%
  ungroup() %>%
  group_by(foa) %>%
  mutate(  h = ifelse(likert == "Highly", perc, 0),
           h = max(h)) %>%
  ungroup() %>%
  mutate(foa = forcats::fct_reorder(foa, h))

str(data_plot)


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
  lapply(c(.25, .5,.75), function(x){
    
    geom_vline(xintercept = x,
               linetype = 'dashed')
  })  +
  
  facet_wrap(~region) +
  theme(legend.position = 'bottom') +
labs(y = "",
       x = "",
       title = "Visibility",
       subtitle = "How visible is the contribution of the TCP to the socio-economic development of your country."
       ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5),
        axis.text.y = element_text(hjust = 0, size = 8),
        legend.position = 'bottom',
        legend.title = element_blank(),
        plot.title.position = 'plot',
        text = element_text(family = "Open Sans"))


ggsave('plots/visibility_test.png',
       last_plot())
