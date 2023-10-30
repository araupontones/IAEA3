# Data for follow-up campaigns


library(dplyr)
library(tidyr)
library(rio)
library(janitor)
library(susor)
library(openxlsx)
library(scales)
library(glue)
gmdacr::load_functions('functions')

#define paths =================================================================
raw_dir <- 'data/6.data-collection/1.raw/cp'
raw_data <- import(file.path(raw_dir, 'cp.rds'))







data_assignments <- raw_data %>%
  filter(interview__status %in% c("Not Started", "Responding")) %>%
  select(interview__status,
         country = country_id, 
         counterpart
  ) %>%
  mutate(`_quantity` = -1,
         `_responsible` = "andres_int",
         `_webmode` = 1,
         cp_hidden = counterpart
  ) %>%
  select(-interview__status)





#export(data_assignments,'questionnaires/assignments/cps_follow_up_26_octubre.txt')




#archive old assignments assignments ========================================================
# 
# version <- 21
# susor_server = "https://www.pulpodata.solutions"
# times_to_iterate = 50 
# 
# for( i in 1:times_to_iterate){
#   
#   #get assignments
#   assignments_ <- susor_get_assignments(version, 'cp' , susor_limit = 100)
#   
#   ids <- assignments_$Id
#   
#   lapply(ids, function(id){
#     
#     url = glue("{susor_server}/api/v1/assignments/{id}/archive")
#     PATCH(url,
#           authenticate(user = 'araupontones',
#                        password ="Seguridad1" ))
#     
#     
#   })
#   
#   message(glue('batch {i}'))
 
  
 
 
#}


#create data campaign =========================================================



s <- rio::import('data/2.sample/cps_sample_corrected.csv', encoding = 'UTF-8')
#before running, download the interview links.
links <- read.delim('data/3.Assignments/follow-up/interviews.tab')





clean_links <- links %>%
  select(url = assignment__link,
         cp_id = cp_hidden
  ) %>%
  left_join(select(s, Name, Country_name, email, region, director, cp_id)) %>%
  relocate(Name, email,Country_name, region, director) %>%
  select(-cp_id) %>%
  rename(Country = Country_name)




#campaigns

#export(clean_links, 'data/4.campaigns/cps_follow_up_26_oct.csv')









