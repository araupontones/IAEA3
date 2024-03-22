#Constraints Based on the Evaluation Framework developed in SCHEMA toc FOOD AND AGRICULTURE

library(dplyr)
library(rio)
library(janitor)
library(tidyr)
library(stringr)
library(ggplot2)
gmdacr::load_functions('functions')
gmdacr::load_functions('functions/themes/')

# LOAD DATA =====================================================================

#load foas' roster collected in the CP survey
foas <-import('data/6.data-collection/4.clean/foas.rds')
names(foas)


names(foas)
tabyl(foas, theme)
# PREPARE DATA ================================================================

#development level.
#variables: dev_2023, dev_2000, and dev_reason (in case dev_2023 < dev_2000)
dev_level <- foas %>%
  select(interview__id,theme, starts_with('dev'), region, foa, improvement, tcp_contribution) 
#%>%
  #mutate(theme = names_themes_2(theme)) %>%
 # filter(theme == "Food and Agriculture") 
tabyl(dev_level, tcp_contribution)
sum(!is.na(dev_level$dev_reason))
#change in development
change_dev <- dev_level %>%
  #select(dev_2000, dev_2023) %>%
  pivot_longer(c(dev_2000, dev_2023),
               names_to = "period") %>%
  filter(!is.na(value)) %>%
  mutate(period = str_remove(period, "dev_"),
         period = ifelse(period == "2000", "Before TCP", period),
         status = case_when(str_detect(value, "EARLY") ~ "1.Early Stage",
                           str_detect(value, "OPERATIONAL") ~ "2.Operational",
                           str_detect(value, "EXPANSION") ~ "3.Expansion",
                           T ~ "0.No Capacity"
                           ),
         status = factor(status,
                        levels = c("0.No Capacity", "1.Early Stage", "2.Operational", "3.Expansion"),
                        ordered = T
                        ),
         
         tcp_contribution = case_when(str_detect(tcp_contribution, "not contributed") ~ "0.Did not contribute",
                                      str_detect(tcp_contribution, "extent") ~ "1.To some extent",
                                      str_detect(tcp_contribution, "substantially ") ~ "2.Contributed substantially",
                                      str_detect(tcp_contribution, "not possible") ~ "3.Impossible without TCP",
                                      T ~ tcp_contribution
         ),
         shrinked = ifelse(!is.na(dev_reason), "Shrinked","Improved" )
         )


tabyl(change_dev, tcp_contribution)
export(change_dev, "data/11.powerbi/status_development.csv")
#   group_by(name, value) %>%
#   count() %>%
#   group_by(name) %>%
#   mutate(total = sum(n)) %>%
#   ungroup() %>%
#   mutate(perc = n/total)
#   
# 
# 
# change_dev  %>%
#   ggplot(aes(x = value,
#              y = perc,
#              fill = name)) +
#   labs(title = "Status of development of Institutions.") +
#   geom_col(position = position_dodge2()) +
#   scale_y_continuous(labels = function(x)scales::percent(x)) +
#   theme_main()
# 
# 
# dev_level %>% 
#   select(dev_reason) %>%
#   filter(!is.na(dev_reason)) %>%
#   mutate(dev_reason = susor::susor_get_stata_labels(dev_reason)) %>%
#   count(dev_reason) %>%
#   ggplot(aes(x = n,
#              y = dev_reason)) +
#   geom_col() +
#   theme_main() +
#   labs(title = "Reasons that best explain the shrink of the level of development")
