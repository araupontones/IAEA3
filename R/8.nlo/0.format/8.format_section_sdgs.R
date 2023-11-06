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
         starts_with(glue('sdg'))
  ) %>%
  tidyr::pivot_longer(-c( country, role, id, region),
                      values_to = "sdg") %>%
  mutate(area = str_remove_all(name, "sdg_|____[0-9]{1,}")
         
         ) %>%
  select(-name)
  


names(p)
View(p)
#export ------------------------------------------------------------------------
export(p, 'data/7.NLO/2.raw_formatted/Part_1_sdgs.rds')
tabyl(p, value) %>% arrange(n)
