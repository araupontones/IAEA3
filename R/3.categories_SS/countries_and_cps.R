#create categories for countries and cps
#This files are used in Survey Solutions as IDs
sample <-rio::import("data/2.sample/cps_sample.csv")
names(sample)

countries <- sample %>%
  select(title = Country_name, 
         value = country_id) %>%
  group_by(title, value) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(parentvalue = "",
         attachmentname = "")



export(countries, 'questionnaires/categories/countries.xlsx', overwrite = T)


cps <- sample %>%
  select(title = cp_id,
         value = cp_id,
         parentvalue = country_id) %>%
  mutate(attachmentname = "")

export(cps, 'questionnaires/categories/cps.xlsx', overwrite = T, sheetname = "Categories")
