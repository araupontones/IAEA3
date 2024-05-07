library(rio)
library(dplyr)
library(tidyr)
library(stringr)
library(janitor)

foas <- import('data/6.data-collection/4.clean/foas.rds')
categories_m <- import('questionnaires/categories/mechanisms.xlsx') %>%
  rename(id = value,
         mechanism = title) %>%
  mutate(id = as.character(id))


mechanism <- foas %>%
  mutate(interview__key = paste0(interview__key, foa)) %>%
  select(region,theme, starts_with('mechani'), interview__key) %>%
  mutate_all(as.character) %>%
  pivot_longer(-c(theme,region, interview__key),
               names_to = 'id') %>%
  filter(!is.na(value)) %>%
  mutate(id = str_remove(id, "mechanism__"),
         value = as.numeric(value)) %>%
  left_join(categories_m) %>%
  select(-id, - mechanism)   %>%
  rename(mechanism = label)
  # group_by(region, theme,label) %>%
  # summarise(value = sum(value),.groups = 'drop') %>%
  # group_by(region, theme) %>%
  # mutate(total = sum(value)) %>%
  # rename(mechanism = label)



export(mechanism, 'data/11.powerbi/mechanisms_cp.csv')



#===========================================================================
#Sustainability
categories_s <- import('questionnaires/categories/sustainability.xlsx') %>%
  rename(id = value,
         sust = title) %>%
  mutate(id = as.character(id))

sustain <- foas %>%
  mutate(interview__key = paste0(interview__key, foa)) %>%
  select(starts_with('sustain'), interview__key) %>%
  mutate_all(as.character) %>%
  pivot_longer(-c(interview__key),
               names_to = 'id') %>%
  filter(!is.na(value)) %>%
  mutate(id = str_remove(id, "sustain__"),
         value = as.numeric(value)) %>%
  left_join(categories_s, by= 'id') %>%
  select(interview__key, sustain = label, value_sus = value, mechanism) %>%
  left_join(mechanism) %>%
  mutate(mechanism = ifelse(is.na(mechanism), "NAP", mechanism)) %>%
  filter(mechanism != "Upgrade infrasctructure") %>%
  mutate(mechanism = ifelse(mechanism == "NAP", NA_character_, mechanism))
  

  
#get mechanisms of not yet
mechs_yet <- mechanism %>% filter(value == 1) %>% filter(mechanism != "Upgrade infrasctructure") 
not_yet <- sustain %>% filter(is.na(mechanism)) %>% select(interview__key, sustain, value_sus) %>%
  left_join(mechs_yet)




final <- sustain %>%
  filter(!is.na(mechanism)) %>%
  rbind(not_yet) 



f_m <- final %>% 
  group_by(region, theme, mechanism)%>%
  mutate(total = n()) %>%
  ungroup() %>%
  group_by(region, theme, mechanism, total) %>%
  summarise(value = sum(value), .groups = 'drop') %>%
  mutate(prop = value/total) %>%
  select(-total, -value)
  

f_s <- final %>%
  select(-interview__key) %>%
  group_by(region, theme, sustain, mechanism)%>%
  mutate(total = n(),
         v_sus = sum(value_sus)) %>%
  ungroup() %>%
  group_by(region, theme,sustain, mechanism) %>%
  # mutate(prop_sus = v_sus/total,
  #     ) %>%
  #group_by(region, theme, sustain, mechanism) %>%
  slice(1) %>%
  ungroup() %>%
  group_by(mechanism, region, theme) %>%
  mutate(ok = sum(v_sus)) %>%
  ungroup() %>%
  mutate( prop_sus = v_sus/ok) %>%
  # filter(region == "Africa",
  #        mechanism == "Support professionalization",
  #        theme == "Food and Agriculture") %>%
  left_join(f_m) %>%
  select(-v_sus, -ok)



sort(names(f_s))

export(f_s, 'data/11.powerbi/mechanism_cp.csv')


