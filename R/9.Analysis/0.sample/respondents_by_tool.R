#Using "questionnaires/NLO/Part I.PDF" as a reference and guidance
library("haven")
library("gmdacr")
library(dplyr)
library(susor)
library(stringr)
library(glue)
library(rio)
library(stringi)
library(tidyr)
library(openxlsx)
library(janitor)

#read file, it was given by Eloisa via email
sav_file <- "data/7.NLO/1.raw/IAEA TC PROGRAMME ACHIEVEMENTS IN THE 21ST CENTURY - PART I.sav"
sav_file2 <- "data/7.NLO/1.raw/IAEA TC PROGRAMME ACHIEVEMENTS IN THE 21ST CENTURY - PART II.sav"
raw_data <- read_sav(sav_file)
raw_data2 <- read_sav(sav_file2)
raw_cps <- import('data/6.data-collection/4.clean/cps.rds')


names(raw_cps)


#Define functions ==============================================================

#Count number of interviews sent by...
# and count number of answers provided in each 
create_wide <- function(.data,by){
  
  .data %>%
    #mutate_all(as.character) 
    rename(country = q0002,
           role = q0001) %>%
    mutate(country = susor_get_stata_labels(country),
           role = susor_get_stata_labels(role),
           name = glue("{toupper(stri_trans_general(q0003_0002, 'Latin-ASCII'))}, {str_to_sentence(q0003_0001)}")
    ) %>%
    relocate(country, name, role) %>%
    select(-starts_with("Collector"),
           -starts_with("Custom"),
           -starts_with('q0003'),
           -starts_with('p0')) %>%
    mutate_all(as.character) %>%
    mutate_all(function(x){ifelse(x == "", NA_character_, x)}) %>%
    rowwise() %>%
    mutate(answered = sum(!is.na(across(starts_with('q'))))) %>%
    arrange(country,name, role,StartDate) %>%
    group_by_at(by) %>%
    mutate(times_submitted = n(),
           int = row_number()) %>%
    relocate(country, name,role, answered, times_submitted,  int) %>%
    .[,1:8] %>%
    pivot_wider(id_cols = c(all_of(by), times_submitted),
                names_prefix = "answers_",
                names_from = int,
                values_from = answered) %>%
    mutate(answers = across(starts_with("answers_")) %>% rowSums(na.rm = T))
  
  
}


# count number of interviews sent by role -------------------------------------

count_by_role <- function(.data, keep_min_answer= 15, survey){
  
  .data %>% create_wide(by = c("country", "name", "role")) %>%
    #keep only complete interviews
    filter(answers >=keep_min_answer) %>%
    group_by(country, role) %>%
    summarise({{survey}} := n(), .groups = 'drop')
  
}


#Count respondents by survey ----------------------------------------------------
ints_nlo1 <- raw_data %>% count_by_role(20, nlo1)
ints_nlo2 <- raw_data2 %>% count_by_role(15, nlo2)



cps <- raw_cps %>%
  group_by(country) %>%
  summarise(cps = n()) %>%
  mutate(role = "Counterpart")

surveys2 <- ints_nlo1 %>%
  full_join(ints_nlo2) %>%
  full_join(cps) %>%
  pivot_longer(-c(country, role),
               names_to = 'survey') %>%
  mutate(role = str_remove_all(role, " - National Liaison Assistant|NLA and |NLO and | - National Liaison Officer"),
         role = str_trim(role),
         type = paste(survey, role, sep = "-")) %>%
  select(-role, -survey) %>%
  filter(!is.na(value)) %>%
  group_by(country, type) %>%
  summarise(value = sum(value), .groups = 'drop') %>%
  pivot_wider(id_cols = country,
              names_from = type,
              values_from = value) %>%
  relocate(country, starts_with("nlo1"),
           starts_with("nlo2")) 





export(surveys2, 'report/sample/responses.xlsx')


#counts

length(unique(cps$country))

unique_countries <- function(.data){
  
  .data %>%
    group_by(country) %>%
    slice(1) %>%
    select(-role)
}

c <- unique_countries(ints_nlo1) %>%
  full_join(unique_countries(ints_nlo2)) %>%
  mutate(all = sum(nlo1, nlo2)) %>%
  filter(!is.na(all))


knitr::combine_words(c$country)
