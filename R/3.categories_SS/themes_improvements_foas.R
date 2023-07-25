library(googlesheets4)
library(dplyr)
library(rio)
library(tidyr)

url_gs <- 'https://docs.google.com/spreadsheets/d/15SY9gpr5ELo0dLRYZNjqzNzOKbF8JYtkI42eBIZDe68/edit#gid=597634711'

#themes ========================================================================
raw_themes <- read_sheet(url_gs, 'themes')

names(raw_themes)
cat_themes <- raw_themes %>%
  rename(title = Theme,
         value = theme_code)


export(cat_themes,'questionnaires/categories/themes.xlsx', sheetName = "Categories", overwrite = T)

#Improvements ==================================================================

raw_improvements <- read_sheet(url_gs, "improvements") 

cat_improvements <- raw_improvements %>%
  group_by(Improvement) %>%
  slice(1) %>%
  ungroup() %>%
  select(title = Improvement,
         value = improvement_code) %>%
  arrange(value)



export(cat_improvements,'questionnaires/categories/improvements.xlsx', sheetName = "Categories", overwrite = T)


#Improvement levels ============================================================
#this are the likert scale for each improvement 
cat_levels <- raw_improvements %>%
  select(improvement_code,
         starts_with("Level")) %>%
  pivot_longer(-improvement_code,
               names_to = 'level',
               values_to = 'title') %>%
  filter(!is.na(title)) %>%
  mutate(improvement_code = ifelse(is.na(improvement_code), 90, improvement_code),
         level = stringr::str_remove(level,"Level "),
         value = paste0(improvement_code, level)
         )


export(select(cat_levels, title, value),'questionnaires/categories/improvements_levels.xlsx', sheetName = "Categories", overwrite = T )


#look up table whether improvement has custom categories =======================
lkp_levels <- cat_improvements %>%
  mutate(has_level = value %in% unique(cat_levels$improvement_code),
         has_level = ifelse(has_level,1,0)) %>%
  filter(!is.na(value)) %>%
  select(rowcode = value,
         has_level)

export(lkp_levels,'questionnaires/categories/lkp_improvement_levels.txt', sep = "\t" )



#look up table FOAs of improvement =============================================
lkp_foas <- raw_improvements %>%
  select(FoA_code, rowcode = improvement_code) %>%
  group_by(rowcode) %>%
  mutate(row = row_number()) %>%
  ungroup() %>%
  pivot_wider(rowcode,
              values_from = FoA_code,
              names_from = row,
              names_prefix = 'foa_') %>%
  filter(!is.na(rowcode))

export(lkp_foas,'questionnaires/categories/lkp_improvement_foas.txt', sep = "\t" )
