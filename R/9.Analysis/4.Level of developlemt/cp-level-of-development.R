#Constraints

library(dplyr)
library(rio)
library(janitor)
library(tidyr)
library(stringr)
library(ggplot2)
gmdacr::load_functions('functions')


foas <-import('data/6.data-collection/4.clean/foas.rds')

names(foas)

theme
dev_2000
dev_2023

dev_level <- foas %>%
  select(theme, starts_with('dev')) %>%
  mutate(theme = names_themes_2(theme)) %>%
  filter(theme == "FOOD and AGRICULTURE") 
  
change_dev <- dev_level %>%
  select(dev_2000, dev_2023) %>%
  mutate(dev_2000 = susor::susor_get_stata_labels(dev_2000),
         dev_2023 = susor::susor_get_stata_labels(dev_2023)) %>%
  mutate(id = row_number()) %>%
  pivot_longer(-id) %>%
  filter(!is.na(value)) %>%
  mutate(value = case_when(str_detect(value, "EARLY") ~ "Early Stage",
                           str_detect(value, "OPERATIONAL") ~ "Operational",
                           str_detect(value, "EXPANSION") ~ "Expansion",
                           T ~ "No Capacity"
                           ),
         value = factor(value,
                        levels = c("No Capacity", "Early Stage", "Operational", "Expansion"),
                        ordered = T
                        )
         ) %>%
  group_by(name, value) %>%
  count() %>%
  group_by(name) %>%
  mutate(total = sum(n)) %>%
  ungroup() %>%
  mutate(perc = n/total)
  


change_dev  %>%
  ggplot(aes(x = value,
             y = perc,
             fill = name)) +
  labs(title = "Status of development of Institutions.") +
  geom_col(position = position_dodge2()) +
  scale_y_continuous(labels = function(x)scales::percent(x)) +
  theme_main()


dev_level %>% 
  select(dev_reason) %>%
  filter(!is.na(dev_reason)) %>%
  mutate(dev_reason = susor::susor_get_stata_labels(dev_reason)) %>%
  count(dev_reason) %>%
  ggplot(aes(x = n,
             y = dev_reason)) +
  geom_col() +
  theme_main() +
  labs(title = "Reasons that best explain the shrink of the level of development")
