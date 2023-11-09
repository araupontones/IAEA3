library(rio)
library(dplyr)
library(tidyr)
library(janitor)
library(stringr)
gmdacr::load_functions('functions/')




#import raw data creted in 0.format/format_names_part_1.R
#the above script formats the data from SPSS to R so it is readable

raw_data <- import('data/7.NLO/1.raw/Part_1.rds') 


p <- raw_data %>%
  select(id = RespondentID,
         country,
         region,
         role,
         starts_with(glue('partnerships'))
  ) %>%
  tidyr::pivot_longer(-c( country, role, id, region)) %>%
  mutate(theme = str_extract(name, '__(.*?)_'),
         theme = str_remove_all(theme, "_"),
         sector = str_remove_all(name, glue('partnerships__{theme}_')),
         sector = trim(sector),
         theme = str_trim(theme),
         value = forcats::fct_recode(value, "Not applicable" = "N/A")
         
         ) %>%
  select(-name)
  



#export ------------------------------------------------------------------------
export(p, 'data/7.NLO/2.raw_formatted/Part_1_partnerships.rds')
