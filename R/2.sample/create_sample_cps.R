
library(rio)
library(dplyr)
library(tidyr)
library(janitor)
library(stringr)
gmdacr::load_functions('functions')

projects <- import('data/1.reference/Copy of CPs_2022_09_12.xlsx')
#internal testers of the questionnare
testers <- import('data/1.reference/internal_testers.xlsx')


directors <- import('data/1.reference/directors.xlsx')

#drop missing cases
df_complete <- create_df_complete(plyr::rbind.fill(projects, testers))

#get county codes
country_codes <- import('data/1.reference/Copy of Country_Names_2023_06_12.xlsx') %>%
  select(Nationality = CountryCode,
         Country_name = Country,
         region = Area
  ) %>%
  #get name of regional director
  left_join(directors, by = 'region')


sort(unique(country_codes$Nationality))


# create ids -------------------------------------------------------------------
cps_ids <- df_complete %>%
  filter(!Nationality %in% c("SOI", "KIR")) %>%
  group_by(Name) %>%
  slice(1) %>%
  ungroup() %>%
  left_join(country_codes) %>%
  select(Name, Country_name, Nationality, EMail, region,director, tester, keep_sample) %>%
  group_by(Country_name) %>%
  mutate(country_id = cur_group_id(),
         country_id = case_when(country_id < 10 ~ paste0("00", country_id),
                                between(country_id, 10, 99) ~ paste0("0", country_id),
                                T ~ as.character(country_id)
         )
         
  ) %>%
  arrange(country_id, Name) %>%
  ungroup() %>%
  group_by(Country_name, country_id) %>%
  mutate(row =  row_number(),
         cp = case_when(row < 10 ~ paste0("00", row),
                        between(row, 10, 99) ~ paste0("0", row),
                        T ~ as.character(row)
         ),
         cp_id = paste0(country_id, cp)
         
  ) %>%
  ungroup() %>%
  select(Name, Country_name, country_id, cp_id, email = EMail, region, director, tester, keep_sample) %>%
  #drop North American CPs
  filter(region != "North America") %>%
  mutate(Country_name = ifelse(Country_name == "T.T.U.T.J of T. Palestinian A.", "State of Palestine", Country_name)) %>%
  #from the testers, only keep the one I want (in internal_testers.xlsx)
  dplyr::filter(keep_sample | is.na(keep_sample)) %>%
  select(-keep_sample)





#final sample
rio::export(cps_ids, "data/2.sample/cps_sample.csv")

#tester sample 
rio::export(filter(cps_ids, tester), 'data/2.sample/testers_sample.csv')
