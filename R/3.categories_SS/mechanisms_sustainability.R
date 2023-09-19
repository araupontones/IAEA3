library(googlesheets4)
library(dplyr)
library(rio)
library(tidyr)
library(stringr)

url_gs <- 'https://docs.google.com/spreadsheets/d/15SY9gpr5ELo0dLRYZNjqzNzOKbF8JYtkI42eBIZDe68/edit#gid=597634711'

#Mechanisms ========================================================================
raw_mech <- read_sheet(url_gs, 'Mechanisms')



cat_mech <- raw_mech %>%
  filter(!is.na(m_id)) %>%
  select(m_id,Mechanism) %>%
  distinct() %>%
  select(value = m_id,
         title = Mechanism)
 


export(cat_mech,'questionnaires/categories/mechanisms.xlsx', sheetName = "Categories", overwrite = T)



#sustinability ==============================================================

cat_sust <- raw_mech %>%
  filter(!is.na(s_id)) %>%
  select(s_id,Sustainability) %>%
  distinct() %>%
  select(value = s_id,
         title = Sustainability)

export(cat_sust,'questionnaires/categories/sustainability.xlsx', sheetName = "Categories", overwrite = T)
  