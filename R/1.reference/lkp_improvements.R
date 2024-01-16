#Create lookup for improvements
library(rio)
library(dplyr)
library(tidyr)
library(janitor)
library(stringr)
library(googlesheets4)


#Get foas from questionnaire 
url_gs <- 'https://docs.google.com/spreadsheets/d/15SY9gpr5ELo0dLRYZNjqzNzOKbF8JYtkI42eBIZDe68/edit#gid=597634711'



#get improvements 
#from questionnaire
improvements_raw <- read_sheet(url_gs, 'improvements') %>%
  mutate(FOACode_new = as.character(FoA_code),
         FOACode_new = ifelse(str_length(FOACode_new) == 1, paste0('0', FOACode_new), FOACode_new)
  ) %>%
  select(FOACode_new, Improvement, improvement_code, theme_code) %>%
  filter(!is.na(FOACode_new)) %>%
  arrange(FOACode_new)


export(improvements_raw, 'data/9.lookups/improvements.rds')
