#Clean data for Institutions
# All institutions for a single project come together in a single cell
# Thus, it is neccessary to unnest them and to crate a table where each row is
# an institution supported in each project

library(rio)
library(dplyr)
library(tidyr)
library(janitor)
library(stringr)
gmdacr::load_functions('functions')

projects <- import('data/1.reference/Copy of CPs_2022_09_12.xlsx')





#Check nationalities ============================================================
# Doing this because some projects have a region as a country
# Thus, I wanted to check if the variable Country could be used
#However, Not all the Nationalities match with the Country of the project




#Drop observations with missing contact information ===========================
#create_df_complete is in /functions
df_complete <- create_df_complete(projects)

  

#Institutions ==================================================================
#There's more than one institution by project,
#Thus, I will create a llong list of institutions so we can have the unique values
  
institutions_id <- df_complete %>%
    #Select relevant variables
  select(Index, ProjectNumber, FOADescription,Country,Area,Institution, Name, EMail, Nationality) %>%
 #create a row by each institution of the project 
  separate(Institution,paste0("inst_",c(1:9)), ";") %>%
  pivot_longer(-c(Index, Country,Area, Name, EMail,ProjectNumber, FOADescription, Nationality),
               values_to = "Institution") %>%
  select(-name) %>%
  filter(!is.na(Institution)) %>%
  #clean some institutions,
  #this could take a long time. There are many cases of the same institution written 
    #differently
  mutate(Institution = stringr::str_trim(Institution)) %>%
  distinct() %>%
  filter(!Institution %in% c('.', "", "•")) %>%
  mutate(Institution = case_when(str_detect(Institution, "COMENA") ~ "Commissariat à l'énergie atomique (COMENA)",
                                 str_detect(Institution, "AAEHC") ~ "Afghanistan Atomic Energy High Commission (AAEHC)",
                                 str_detect(Institution, "AMARAP") ~ "Agence Malienne de Radioprotection (AMARAP)",
                                 T ~ Institution))




  
export(institutions_id, 'data/1.reference/clean/institutions.csv')
  
#ID of CPS =====================================================================
  #check that counterparts have a single nationality
 



#sample size 
nrow(cps_ids)
#11,635


  
  
cps_nationalities <- cps_ids %>%
  group_by(Country_name) %>%
  summarise(CPs = n()) 
  adorn_totals()


mean(cps_nationalities$CPs)
export(cps_nationalities, 'data/reference/checks/sample_size_by_country.xlsx')

tabyl(cps_ids, Nationality)
  # check <- institutions_id %>%
#   select(Institution) %>%
#   distinct() %>%
#   arrange(Institution)
# 
# 
# 
# ?pivot_longer
# tabyl(area_id, Area, area_id)

  
  
#Export cases to Eloisa ========================================================
names(institutions_id)
#1 Countries:
  #There is more than one row by project number
  #Multiple Institutions by Projects
  #More counterparts than projects
  
  countries <- institutions_id %>%
    group_by(Country) %>%
    summarise(projects = length(unique(ProjectNumber)),
              instutions = length(unique(Institution)),
              counterparts = length(unique(Name)),
              foas = length(unique(FOADescription))) %>%
    janitor::adorn_totals()
  

  
  foas <- projects %>%
  count(FOADescription, FOACode)
  
  
  export(countries, 'data/reference/checks/countries.xlsx')
  export(foas, 'data/reference/checks/foas.xlsx')
  
  
  
#Check alternatives for ID system
#check nationalities of CPS
#It works fine! All CPs have a single Nationality
  nat <- institutions_id %>%
    group_by(Name) %>%
    summarise(n = length(unique(Nationality)))


#Check number of institutions
# A CP can be associated with more than one institution
  insts <- institutions_id %>%
    group_by(Name) %>%
    summarise(n = length(unique(Institution)))
  
  # CHECK FOA
  foa <- institutions_id %>%
    group_by(Name) %>%
    summarise(n = length(unique(FOADescription)))
  
  

#id of names =================================================================
names_id <- lapply(projects_ids$Country, function(c){
  
  
  country <- projects_ids %>%
    filter(Country == c) %>%
    mutate(name_id = group_indices(., Name))
    
    
    
    
    
})


#cps IDs ======================================================================\


names(projects)



categories <- dplyr::tibble(
  title = countries,
  value = seq(1, length(countries), 1)
)


export(categories, 'questionnaires/categories/countries.xlsx')
names(projects)
View(projects)
