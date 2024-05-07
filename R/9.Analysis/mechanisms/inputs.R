library(rio)
library(dplyr)
library(tidyr)
library(stringr)

foas <- import('data/6.data-collection/4.clean/foas.rds')

categories_e <- import('questionnaires/categories/effectiveness.xlsx') %>%
  mutate_all(as.character)
 
names(foas)
inputs <- foas %>%
  select(region,theme, starts_with('effecti')) %>%
  mutate_all(as.character) %>%
  pivot_longer(-c(theme,region),
               names_to = 'id') %>%
  filter(!is.na(value)) %>%
  mutate(id = str_remove(id, "effectiveness__"),
         value = as.numeric(value)) %>%
  left_join(categories_e) %>%
  select(-id) %>%
  group_by(region, theme, input) %>%
  mutate(total= n()) %>%
  ungroup() %>%
  group_by(region, theme, total, input) %>%
  summarise(value = sum(value), .groups = 'drop') %>%
  mutate(prop = value/total)
            

sum(mechanism$value)


export(inputs, 'data/11.powerbi/inputs_cp.csv')

mutate