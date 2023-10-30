
#load dependencies --------------------------------------------------------------
#source("R_/5.QA/utils_QA.R")
library(dplyr)
library(httr)
library(stringr)
library(lubridate)
library(rio)
library(janitor)
library(susor)

#dfine parameters --------------------------------------------------------------
#sample_girls<- rio::import(survey_girls_sample_path)
questionnaire <- "cp"
sample_file <- 'data/2.sample/cps_sample_corrected.csv'
sample <- import(sample_file)
bounces <- import('data/4.campaigns/bounces_Oct_16_2023.csv') %>%
  select(email =`Email Address`,
         bounce = `Bounce Type`)





#download data ----------------------------------------------------------------

#login
#Import list of interviewers logged in Survey Solutions -----------------------



susor::susor_login(susor_server = "https://www.pulpodata.solutions",
                   susor_user = "araupontones",
                   susor_password = "Seguridad1",
                   susor_dir_downloads = 'data/6.data-collection/0.downloads',
                   susor_dir_raw = 'data/6.data-collection/1.raw',
                   limit = 100,
                   ofset = 1
                   
)




#Download all versions of the questionnaire ----------------------------------
#get versions ---------------------------------------------------------------
versions <- susor_questionnaires$Version[susor_questionnaires$Variable == questionnaire]
  

downloads <- lapply(versions, function(x){
  
  
  susor::susor_export_file(susor_qn_variable = questionnaire,
                           susor_qn_version = x,
                           susor_format = "STATA"
  )
  
  
  
})
  

#append all versions ---------------------------------------------------------
susor::susor_append_versions(susor_qn_variable = questionnaire,
                             susor_format = "STATA")


#read raw data -----------------------------------------------------

raw_data <- rio::import(file.path(susor_dir_raw, questionnaire, 'cp.dta')) 
#count missing values
raw_data$na_count <- apply(raw_data, 1, function(x) sum(is.na(x)))

raw_no_dupes<- raw_data %>%
  #Keep only one observation by counterpart
  group_by(counterpart) %>%
  filter(na_count == min(na_count)) %>%
  filter(n_questions_unanswered == min(n_questions_unanswered)) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  relocate(counterpart, na_count, n_questions_unanswered, interview__status)


#count number of  duplicates
dupes <- get_dupes(raw_no_dupes, counterpart) 

#join with sample
raw_with_sample <- raw_no_dupes %>%
  right_join(sample, by = c('counterpart' = 'cp_id')) %>%
  mutate(across(c(interview__status, country), function(x)susor_get_stata_labels(x))) %>%
  left_join(bounces) %>%
  mutate(interview__status = case_when(!is.na(bounce) ~ "Bounced",
                                       is.na(interview__status) ~ "Not Started",
                                       #cases when user did not "completed the interview"
                                       interview__status == "InterviewerAssigned" & 
                                         na_count <= 70 & 
                                         n_questions_unanswered <=3 ~ "Submitted",
                                       interview__status == "InterviewerAssigned" ~ "Responding",
                                       interview__status == "Completed" ~ "Submitted"
  ))


tabyl(raw_with_sample, interview__status)

#export raw data

export(raw_with_sample, file.path(susor_dir_raw, questionnaire, 'cp.rds'))

