
library(rio)
library(dplyr)

list.dirs('data/3.Assignments')

#read sample ---------------------------------------------------------------------
sample <- import('data/2.sample/cps_sample_corrected.csv', encoding = "UTF-8") %>% select(cp_id , Name)
  


#read sample --------------------------------------------------------------------
raw_data <- import('data/6.data-collection/1.raw/cp/cp.rds') %>% select(cp_id = counterpart, interview__status)


#define campaigns ---------------------------------------------------------------
campaigns <- list.files('data/4.campaigns/', pattern = "10|oct")

campaigns <- import('data/4.campaigns/cps_follow_up_2023-11-07.csv', encoding = "UTF-8")



#https://www.pulpodata.solutions/primary/WebInterview/721f62acd8e643909a35bd30e9826930/Section/84064e533a8be47b271b845a5d388e7d


# appended_campaigns <- lapply(campaigns, function(x){
#   
#   
#   date = str_extract(x,"2023-10-[0-9]{1,}")
#   
#  
#   c <- import(file.path('data/4.campaigns',x), encoding = 'UTF-8') %>%
#     mutate(date = lubridate::ymd(date)) 
#   
# }) %>% do.call(plyr::rbind.fill,.) %>%
#   select(-counterpart)
# 
# nrow(appended_campaigns)

#create new campaign ----------------------------------------------------------

new_campaign <- campaigns %>%
  left_join(sample) %>% 
  group_by(email) %>%
  slice(1) %>%
  left_join(raw_data) %>%
  filter(interview__status %in% c("Not Started", "Responding")) %>%
  select(-c(cp_id, interview__status))
 

  
·export(new_campaign, 'data/4.campaigns/cps_follow_up_2023-11-07.csv')




