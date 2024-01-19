#Create LookUP tables of FOAs
#There are different ids across different tools
#We need to link Themes and FOAs to the ones used in the surveys

library(rio)
library(dplyr)
library(tidyr)
library(janitor)
library(stringr)
library(googlesheets4)
gmdacr::load_functions('functions')


#read raw data ================================================================
#All Projects 
projects <- import('data/1.reference/Copy of CPs_2022_09_12.xlsx') 



#get mapping given by IAEA
mapping <- import('data/1.reference/Copy of FOA mapping - old to new.xlsx')




#Get foas from questionnaire 
url_gs <- 'https://docs.google.com/spreadsheets/d/15SY9gpr5ELo0dLRYZNjqzNzOKbF8JYtkI42eBIZDe68/edit#gid=597634711'

#Get foas from questionnaires and from projects================================

#from questionnaire
foas_questonnaire_raw <- read_sheet(url_gs, 'FOAs') 



#From projects

foas_projects <- projects %>% count(FOACode,FOADescription, name = "total_projects") %>%
  filter(!is.na(FOACode))


#Get the new IDs of FOAS ========================================================

foas_mapped <- foas_projects %>%
  #join FOAs from prjects with mapping
  full_join(mapping, by = c("FOACode" = "old" )) %>%
  #There are some new codes that do not have old one
  mutate(FOACode_new = ifelse(is.na(new), FOACode, new)) %>%
  #drop missing from projects (some)
  relocate(FOACode_new)  %>%
 
  select(FOACode_new, FOACode_old = FOACode, 
        FOA_old = FOADescription,
        FOA_nlo1 = foa_nlo1,
         total_projects) 
  




#Get the names of the new FOAs as they are in the questionnaire
foas_joint <- foas_questonnaire_raw %>%
  select(FOACode_new = FoA_code, 
         FOA_new = `FoA new name`, 
         theme_code, 
         theme = `Thematic Area`) %>%
  #Make the format consistent
  mutate(FOACode_new = as.character(FOACode_new),
         FOACode_new = ifelse(str_length(FOACode_new) == 1, paste0('0', FOACode_new), FOACode_new)
        )   %>%
  full_join(foas_mapped) %>%
  arrange(FOACode_new) %>%
  relocate(FOACode_new, FOACode_old,
           FOA_new, FOA_old,
           FOA_nlo1) %>%
  mutate(theme = names_themes(theme_suffix(theme_code))) 


View(foas_joint)



#Export
#THis to check with IAEA the missing new FOAS
export(foas_joint, 'data/1.reference/checks/FOAs_translation.xlsx', overwrite = T)
#This to keep as the lookups
export(foas_joint, 'data/9.lookups/foas.rds')
