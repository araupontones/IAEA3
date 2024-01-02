library(rio)
library(dplyr)
library(janitor)
gmdacr::load_functions('functions/')




#load data =======================================================================
sample <- import('data/2.sample/cps_sample.csv', encoding = 'UTF-8')

#list of ldcs given by eloisa

ldcs <- import('data/1.reference/countries/Copy of IAEA members as at 14-12-2023.xlsx') %>%
  select(country= cName,
         ldc = cLDCFlag) %>%
  #clean the names to make them consistent with the sample
  clean_countries() #function saved in functions/


#lookup table countries =======================================================
#keep unique country names from the sample

countries <- sample %>%
  group_by(Country_name) %>%
  slice(1) %>%
  select(country= Country_name,
         region)




#get ldcs and recipients
countries_ldc <- countries %>%
  left_join(ldcs) %>%
  mutate(joined = case_when(
    #Countries with no programme in the 21st century -----------------------------
    country %in% 
      c('Austria', 'Belgium', 'Denmark', 'Finland', 'Netherlands', 'United Kingdom') 
    ~ 'No programme in 21st century',
    
    #Countries that joined Agency since 2020 and should not be considered when referring to ‘future aspects’
    country %in% c('Comoros', 'Samoa') ~ 'Since 2020',
    
    #Countries with programme ONLY AFTER 2015:
    country %in% c('Barbados', 'Kingdom of Eswatini') ~ "After 2015",
    
    #Countries with programme ONLY DURING 2000-2009:
    country %in% c('Cyprus', 'Russian Federation') ~ "2000-2009",
    
    #Countries with programme ONLY DURING 2000-2015:
    
    country %in% c('China') ~ '2000-20015',
    #Countries with programme ONLY AFTER 2009:
    country %in% c('Bahrain', 'Burundi', 'Cambodia', 'Congo, Rep. of',
                   'Lesotho', 'Montenegro', 'Oman', 'Lao P.D.R.',
                   'Dominica', 'Fiji', 'Papua New Guinea', 'Rwanda', 'Togo',
                   'Trinidad and Tobago', 'Bahamas', 'Brunei Darussalam',
                   'Antigua and Barbuda', 'Barbados', 'Djibouti', 'Guyana', 'Vanuatu', 
                   'Turkmenistan', 'Saint Vincent & the Grenadines', 'Grenada', 'Saint Lucia') ~ 'After 2009'
    
    
  )
  )







export(countries_ldc,"data/9.lookups/countries.rds" )
export(countries_ldc,"data/9.lookups/countries.xlsx" )


#export summary for eloisa

sum_ldc <- tabyl(countries_ldc, ldc) %>%
  select(category = ldc,
         n) %>%
  mutate(category = ifelse(category == "Yes", "LDC", category)) %>%
  filter(category == "LDC")




tabyl(countries, region) %>%
  select(category = region,
         n) %>%
  rbind(sum_ldc)




