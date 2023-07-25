library(rio)
library(dplyr)
library(tidyr)
gmdacr::load_functions('functions')

#read criterions from master file
raw <- import('questionnaires/CP/list_of_achievements.xlsx', sheet = "Improvements")

#create theme ID
themes <- raw %>%
  group_by(Theme) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(theme_id = ids_themes(Theme)) %>%
  select(title = Theme,
         value =theme_id
  ) %>%
  arrange(value)

cat_themes <- themes %>%
  mutate(parentvalue = "",
         attachmentname = ""
  )

export(cat_themes,'questionnaires/categories/themes.xlsx', sheetName = "Categories", overwrite = T)

names(raw)

names(raw)
#create improvements ids --------------------------------------------------------
improvements <- raw %>%
  left_join(themes, by = c("Theme"='title')) %>%
  select(title = Improvement,
         parentvalue = value) %>%
  group_by(parentvalue) %>%
  mutate(value = paste0(parentvalue, row_number()),
         attachmentname = "")
  rename(parentid = value) 

  export(improvements,'questionnaires/categories/improvements.xlsx', sheetName = "Categories", overwrite = T)
  




