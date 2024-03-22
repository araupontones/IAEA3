library(rio)
library(ggplot2)
library(RColorBrewer)
library(extrafont)

sustainability <- import('data/7.NLO/2.raw_formatted/Part_1_sustainability.rds')

#missing <- scales::percent(sum(is.na(effectiveness$period)) / nrow(effectiveness))



#glue('{missing} of the questions were missing')
names(sustainability)

#data for power BI =============================================================

powerbi_sus <- sustainability %>%
  #drop missing foas (we need to associate the missing ones in the mapping_foas)
  filter(!is.na(foa)) %>%
  group_by(country,region, theme,improvement, foa,outcome, int_outcome) %>%
  summarise(when_outcome = min(period, na.rm = T),
            achieved_outcome = max(achieved),
            .groups = 'drop'
            ) %>%
  select(country,theme, foa, outcome,int_outcome, when_outcome, achieved_outcome) %>%
  #join with efficiency to get outcomes and int_outcomes in one page
  left_join(powerbi_eff) %>%
  relocate(when_int_outcome,.after = when_outcome) %>%
  mutate(id = as.character(paste0("A",row_number())), .before = country)




export(powerbi_sus, glue('data/11.powerbi/sustainability.csv'))


powerbi_long <- powerbi_sus %>%
  mutate(id_outcome = int_outcome) %>%
  pivot_longer(c(outcome, int_outcome),
               names_to = "level",
               values_to = "indicator") %>%
  mutate(when = ifelse(level== "outcome", as.character(when_outcome), as.character(when_int_outcome))) %>%
  mutate(achieved = ifelse(level== "outcome", as.character(achieved_outcome), as.character(achieved_int_outcome))) %>%
  relocate(c(when,achieved, level, indicator), .after = when_int_outcome) %>%
  select(-when_int_outcome, -when_outcome, - achieved_outcome, - achieved_int_outcome)
  

export(powerbi_long, glue('data/11.powerbi/sustainability_long.csv'))


View(powerbi_long)

#sustainability wider

# 
# data_plot <- effectiveness %>%
#   filter(theme == "FOOD and AGRICULTURE") %>%
#   filter(!is.na(period),
#          period != "N/A") %>%
#   group_by(theme, outcome, period, region) %>%
#   summarise(total = n())
# 
# tabyl(effectiveness, period)
# 
# View(effectiveness)
# 
# min(effectiveness$period)
# #plot
# ggplot(data_plot,
#        aes(
#          y = outcome,
#          x = period,
#          fill = total)) +
#  geom_tile() +
#   labs(y = "",
#        x = "",
#        title = "Effectiveness",
#        subtitle = "Time range when improvements where introduce by TCP."
#        ) 
#   scale_y_discrete(labels = label_wrap(30)) +
#   scale_fill_distiller(palette = "Blues", direction = 1) +
#   facet_wrap(~region) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 90),
#         axis.text.y = element_text(hjust = 0),
#         legend.position = 'bottom',
#         legend.title = element_blank(),
#         plot.title.position = 'plot',
#         text = element_text(family = "Open Sans"))
# 
# 
# ggsave('plots/effectiveness_test.png',
#        last_plot())
