library(rio)
library(ggplot2)
library(RColorBrewer)
library(extrafont)
library(janitor)

#this data is cleaned in R/8.nlo/PartI/0.format
effectiveness <- import('data/7.NLO/2.raw_formatted/Part_1_effectiveness.rds')
#missing <- scales::percent(sum(is.na(effectiveness$period)) / nrow(effectiveness))

names(effectiveness)
tabyl(effectiveness, foa, theme)

nrow(effectiveness)
tabyl(effectiveness, foa)

glue('{missing} of the questions were missing')
names(effectiveness)

#data for power BI =============================================================

powerbi_eff <- effectiveness %>%
  #drop missing foas (we need to associate the missing ones in the mapping_foas)
  filter(!is.na(foa)) %>%
  group_by(country,region, theme,improvement, foa,int_outcome, foa_nlo1_effectiveness) %>%
  summarise(when_int_outcome = min(period, na.rm = T),
            achieved_int_outcome = max(achieved),
            .groups = 'drop'
            
            )



warnings()
names(powerbi_eff)
warnings()
tabyl(powerbi_sus, foa)
nrow(powerbi_eff)
sections
#View(powerbi)


export(powerbi_eff, glue('data/11.powerbi/{sections}.csv'))


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
