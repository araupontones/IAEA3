library(rio)
library(ggplot2)
library(RColorBrewer)
library(extrafont)

regionalCoop <- import('data/7.NLO/2.raw_formatted/Part_1_regionalCoop.rds')
missing <- scales::percent(sum(is.na(regionalCoop$value)) / nrow(regionalCoop))


glue('{missing} of the questions were missing')

#QA --------------------------------------------------------------------------

names(regionalCoop)

#count interviews that completely skipped this section
qa <- regionalCoop %>% 
  group_by(id, country) %>%
  summarise(questions = n(),
            unanswered = sum(is.na(value)),
            skipped_section = questions == unanswered
  )

skipped_section <- sum(qa$skipped_section)
glue('{skipped_section} respondents did not answer a single question of this section')

#create a table with percetages
data_plot <- regionalCoop %>%
  left_join(qa) %>%
  filter(!skipped_section) %>%
  filter(theme == "FOOD and AGRICULTURE")  %>%
  mutate(value = ifelse(is.na(value), 0, value)) %>%
  group_by(foa, value_added) %>%
  summarise(perc = mean(value)) %>%
  mutate(value_added2 = ifelse(str_detect(value_added, "\\/"), 
                               str_extract(value_added, "^[^\\/]*"), 
                               value_added),
         value_added2  = case_when(str_detect(value_added2, "actions") ~ "National Actions",
                                   str_detect(value_added2, "harmonization ") ~ "Regional Harmonization",
                                   str_detect(value_added2, "expertise") ~ "Sharing regional expertise",
                                   str_detect(value_added2, "technical capacities") ~ "Technical capacities",
                                   T ~ value_added2
                                   ),
         value_added = str_trim(value_added2)
         )
  


ggplot(data_plot, 
       aes(
         x = perc,
         y = reorder(value_added, perc)
         
       )) +
  
  geom_col(fill = '#8AB4F8') +
  facet_wrap(~foa, 
             labeller = label_wrap_gen(width = 30),
             scales = "free_x") +
  labs(y = "",
       x = "% that report TCP value added.",
       title = "Regional Cooperation Agreements",
       subtitle = "Value added by participating in activities led by the RCA in the context of the TCP."
       )+
  theme_minimal() +
  scale_x_continuous(labels = function(x)scales::percent(x)) +
    theme(axis.text.x = element_text(angle = 90),
        axis.text.y = element_text(hjust = 0),
        legend.position = 'bottom',
        legend.title = element_blank(),
        plot.title.position = 'plot',
        text = element_text(family = "Open Sans"),
        strip.text = element_text(hjust = 0, vjust = 1, face = 'bold'))




ggsave('plots/regionalCoop_test.png',
       last_plot())
