library(googlesheets4)
library(dplyr)
library(rio)
library(tidyr)
library(stringr)

url_gs <- 'https://docs.google.com/spreadsheets/d/15SY9gpr5ELo0dLRYZNjqzNzOKbF8JYtkI42eBIZDe68/edit#gid=597634711'

#Impact ========================================================================
raw_impact <- read_sheet(url_gs, 'impact_path')


cat_impact <- raw_impact %>%
  select(value = impact_code,
         title = Impact)
 


export(cat_impact,'questionnaires/categories/impacts.xlsx', sheetName = "Categories", overwrite = T)



#impact questions ==============================================================

cat_questions <- raw_impact %>%
  filter(!is.na(impact_code),
         !is.na(`Impact Q1`)) %>%
  select(impact_code,
         starts_with("Impact Q")
         ) %>%
  pivot_longer(-impact_code,
               values_to = 'title') %>%
  select(-name) %>%
  filter(!is.na(title)) %>%
  mutate(title2 = str_remove(title, "To the best of your knowledge, "),
         title3 = str_remove_all(title2, "has any |have|\\?|has the |are the"),
         title4 = str_to_sentence(title3),
         title5 = str_trim(title4)) %>%
  select(impact_code, title = title5) %>%
  group_by(impact_code) %>%
  mutate(value = paste0(impact_code, row_number())) %>%
  ungroup() %>%
  select(value, title)

export(cat_questions,'questionnaires/categories/impactsyesno.xlsx', sheetName = "Categories", overwrite = T)
  