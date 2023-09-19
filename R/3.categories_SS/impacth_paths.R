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
         title = Impact) %>%
  filter(!is.na(value))
 


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
  filter(!is.na(title),
         title != "AS ABOVE ALL CELLS"
         ) %>%
  mutate(title2 = str_remove(title, "To the best of your knowledge, "),
         title3 = str_remove_all(title2, "has any |have|\\?|has the |are the"),
         #title4 = str_to_sentence(title3),
         title5 = str_trim(title3),
         title6 = str_replace(title5, "Iaea", "IAEA")) %>%
  select(impact_code, title = title6) %>%
  group_by(impact_code) %>%
  mutate(value = paste0(impact_code, row_number())) %>%
  ungroup() %>%
  select(value, title) %>%
  mutate(title = str_replace_all(title, "iaea", "IAEA")) %>%
  filter(
    #this is the same as 114. Thus, it should 114 should be enabled when impact 13 is selected
    !(value== "131" &
             title == "Reduced post-harvest losses in food production with the application of irradiation technology")
    
    
    )
 



export(cat_questions,'questionnaires/categories/impactsyesno.xlsx', sheetName = "Categories", overwrite = T)


##transform to wide so I can show IAEA how to write the impact questions

questions_wide <- cat_questions  %>%
  mutate(value = str_sub(value,1,2)) %>%
  group_by(value) %>%
  mutate(row = row_number()) %>%
  ungroup() %>%
  pivot_wider(id_cols = value,
              names_prefix = "question_",
              values_from = title,
              names_from = row) 
names(questions_wide)

export(questions_wide, 'questionnaires/CP/alternatives_impact_questions.xlsx', overwrite = T)
