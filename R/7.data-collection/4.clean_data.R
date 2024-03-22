#clean data from CP survey
library(janitor)
library(dplyr)
library(tidyr)
library(rio)
library(forcats)



# Load data ===================================================================
# main section
raw_main <- import('data/6.data-collection/1.raw/cp/cp.rds') 
#roster of foas
raw_foas <- import('data/6.data-collection/1.raw/cp/foas.dta')

#look up countries
countries_lkp <- import('data/9.lookups/countries.rds') %>% select(-region)

#Categories of foas in questionnaire
cat_foas <- import('questionnaires/categories/improvements.xlsx') %>%
  select(improvement = title,
         foas__id = value)

#Dictionary of foas for the whole evaluation

dictionary_foas <- import('data/1.reference/mapping_foas.xlsx') %>%
  select(foa, improvement,theme ) %>%
  group_by(improvement, theme) %>%
  slice(1) %>%
  ungroup()





# Clean main data ==============================================================
#keep completed interviews only 
# Clean countries to make it consistent with sample
completed_main <- raw_main %>%
  filter(interview__status == 'Submitted') %>%
  filter(!is.na(theme)) %>%
  #correct country name discrepancies
  mutate(country = fct_recode(country, 
                              'Saint Vincent & the Grenadines' = "Saint Vincent &amp; the Grenadines" )
         ) %>%
  left_join(countries_lkp) %>%
  #get labels of variables
  mutate(across(c(theme),function(x)as.character(susor::susor_get_stata_labels(x))))
  


#total of 2674 interviews



#check that all countries are in the countries_lkp

# cp_countries <- unique(completed_d$country)
# setdiff(cp_countries, countries_lkp$country)


#Clean roster of foas ==========================================================

#count missing values
raw_foas$na_count <- apply(raw_foas, 1, function(x) sum(is.na(x)))
  
#get information from main questionnaire
#keep only completed interviews
completed_foas <- raw_foas %>%
  left_join(select(completed_main, 
                   interview__id, interview__status, 
                   country, region,theme)) %>%
  relocate(country, region,interview__status, na_count, theme) %>%
  #drop interviews not in completed cp
  filter(!is.na(interview__status)) %>%
  #drop empty sections 
  filter(na_count < 36) %>%
  #get variable labels
  mutate(across(c(starts_with('dev'),tcp_contribution),
                function(x)as.character(susor::susor_get_stata_labels(x))))

message(paste("Before:", nrow(completed_foas)))
head(completed_foas$theme)


#get the name of the foas as they are in the evaluation framework
foas_consistent <- completed_foas %>%
  #get the value of the foas (improvement) from the categories of the questionnaire
  left_join(cat_foas) %>%
  #get the foa from the dictionary
  left_join(dictionary_foas)


message(paste("After:", nrow(foas_consistent)))


#export clean files
export(completed_main, 'data/6.data-collection/4.clean/cps.rds')
export(foas_consistent, 'data/6.data-collection/4.clean/foas.rds')
