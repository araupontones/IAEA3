#create assignments
library(dplyr)
library(rio)

assignment <- import("data/2.sample/cps_sample.csv") %>%
  filter(tester) %>%
  select(country = country_id,
         counterpart = cp_id) %>%
  mutate(`_quantity` = -1,
         `_responsible` = "andres_int",
         `_webmode` = 1,
         cp_hidden = counterpart
         )


export(assignment,'questionnaires/assignments/testers.txt' )


