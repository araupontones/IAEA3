#Clean data for Projects
# All institutions for a single project come together in a single cell
# Thus, it is neccessary to unnest them and to crate a table where each row is
# an institution supported in each project

library(rio)
library(dplyr)
library(tidyr)
library(janitor)
library(stringr)
gmdacr::load_functions('functions')





#read raw data ================================================================
#All Projects 
projects <- import('data/1.reference/Copy of CPs_2022_09_12.xlsx') 

foas <- import('data/9.lookups/foas.rds')
countries <- import('data/9.lookups/countries.rds')



clean_projects <- projects %>%
  #Keep only one record for each project
  #the data contains all the CPs by project.
  group_by(ProjectNumber) %>%
  slice(1) %>%
  ungroup() %>%
  #rename consistently across
  select(FOACode_old  = FOACode,
         Index,
         country = Country, 
         ProjectNumber
         ) %>%
  #Get the new foa ID
  left_join(select(foas, -total_projects)) %>%
  #Get information of countries
  clean_countries() %>%
  left_join(countries) %>%
  #Fix region for regional projects
  mutate(region = case_when(country == 'Regional Africa' ~ "Africa",
                            country == 'Regional Asia & the Pacific' ~ "Asia and the Pacific",
                            country == 'Regional Europe' ~ "Europe",
                            country == "Regional Latin America" ~ " Latin America and the Caribbean",
                            country == "Interregional" ~ "Interregional",
                            T ~ region
                            )
         
         
         )









export(clean_projects, 'data/1.reference/clean/projects.rds')

