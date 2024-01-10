#clean CP main
library(janitor)
library(dplyr)
library(tidyr)
library(rio)
library(forcats)



raw_d <- import('data/6.data-collection/1.raw/cp/cp.rds') 
raw_foas <- import('data/6.data-collection/1.raw/cp/foas.dta')
countries_lkp <- import('data/9.lookups/countries.rds') %>% select(-region)


names(raw_d)
tabyl(raw_d, interview__status)

#keep completed interviews only -----------------------------------------------
completed_d <- raw_d %>%
  filter(interview__status == 'Submitted') %>%
  filter(!is.na(theme)) %>%
  #correct country name discrepancies
  mutate(country = fct_recode(country, 
                              'Saint Vincent & the Grenadines' = "Saint Vincent &amp; the Grenadines" )
         ) %>%
  left_join(countries_lkp)
  


  names(completed_d)
#total of 2674 interviews


#check that all countries are in the countries_lkp

# cp_countries <- unique(completed_d$country)
# setdiff(cp_countries, countries_lkp$country)

#check that the join with countries is correct
completed_d$ldc[completed_d$country == 'Saint Vincent & the Grenadines']
#These countries are not in the year of TC recipients
# not_in_programme <- sort(unique(completed_d$country[is.na(completed_d$joined)]))
# 
# tibble(
#   
#   country = not_in_programme
# ) %>%
#   export(., 'data/1.reference/checks/not_in_TC_recipients_21_century.xlsx')
# 
# length(not_in_programme)
# tabyl(completed_d, joined)

#Clean roster of foas ==========================================================
#count missing values
raw_foas$na_count <- apply(raw_foas, 1, function(x) sum(is.na(x)))

completed_foas <- raw_foas %>%
  left_join(select(completed_d, 
                   interview__id, interview__status, country, theme)) %>%
  relocate(country, interview__status, na_count, theme) %>%
  #drop interviews not in completed cp
  filter(!is.na(interview__status)) %>%
  #drop empty sections 
  filter(na_count < 36)



#export clean files
export(completed_d, 'data/6.data-collection/4.clean/cps.rds')
export(completed_foas, 'data/6.data-collection/4.clean/foas.rds')
