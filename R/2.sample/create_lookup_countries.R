sample <- import('data/2.sample/cps_sample.csv', encoding = 'UTF-8')

#lookup table countries

names(sample)

countries <- sample %>%
  group_by(Country_name) %>%
  slice(1) %>%
  select(country= Country_name,
         region)


export(countries,"data/9.lookups/countries.rds" )
