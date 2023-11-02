


list.dirs('data/3.Assignments')

#read sample ---------------------------------------------------------------------
sample <- import('data/2.sample/cps_sample_corrected.csv', encoding = "UTF-8") %>% select(cp_id , Name)
  


#read sample --------------------------------------------------------------------
raw_data <- import('data/6.data-collection/1.raw/cp/cp.rds') %>% select(cp_id = counterpart, interview__status)


#define campaigns ---------------------------------------------------------------
campaigns <- list.files('data/4.campaigns/', pattern = "10|oct")


appended_campaigns <- lapply(campaigns, function(x){
  
  
  date = str_extract(x,"2023-10-[0-9]{1,}")
  
 
  c <- import(file.path('data/4.campaigns',x), encoding = 'UTF-8') %>%
    mutate(date = lubridate::ymd(date)) 
  
}) %>% do.call(plyr::rbind.fill,.) %>%
  select(-counterpart)

nrow(appended_campaigns)

#create new campaign ----------------------------------------------------------

new_campaign <- appended_campaigns %>%
  left_join(sample) %>%
  group_by(email) %>%
  filter(date == max(date)) %>%
  slice(1) %>%
  mutate(count = n()) %>%
  left_join(raw_data ) %>%
  filter(interview__status %in% c("Not Started", "Responding")) %>%
  select(-c(date,cp_id, count, interview__status))
 

export(new_campaign, 'data/4.campaigns/cps_follow_up_2023-11-02.csv')




