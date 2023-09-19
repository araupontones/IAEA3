#check numnber of CPs by foa and country
#run create_sample first.

cps <- df_complete %>%
  filter(!Nationality %in% c("SOI", "KIR")) %>%
  left_join(country_codes) 


#1 number of counterparts by country
names(cps)

by_country <- cps %>%
  group_by(Country_name) %>%
  summarise(cps = length(unique(Name)))
  

export(by_country,"data/1.reference/checks/cps_by_country.xlsx")

by_foa <- cps %>%
  group_by(Country_name, FOADescription) %>%
  summarise(cps = length(unique(Name)),
            names = paste(sort(Name), collapse = ","))

export(by_foa, "data/1.reference/checks/cps_by_country_and_foa.xlsx")
