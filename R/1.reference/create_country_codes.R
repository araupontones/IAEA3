country_codes <- import('data/reference/Copy of Country_Names_2023_06_12.xlsx') %>%
  select(Nationality = CountryCode,
         Country_name = Country
  )


projects <- import('data/reference/Copy of CPs_2022_09_12.xlsx')



nationalities <- projects %>%
  select(Nationality) %>%
  distinct() %>%
  filter(!is.na(Nationality),
         !is.null(Nationality),
         Nationality != 'NULL') %>%
  left_join(country_codes)
