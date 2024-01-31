library(dplyr)
library(rio)
library(tidyr)
library(stringr)
library(janitor)
library(ggplot2)


responses <- import('report/sample/responses.xlsx')
lkp_countries <- import('data/9.lookups/countries.rds')
gmdacr::load_functions('functions/themes/')

#Get responses by role and survey
responses <-get_responses()

#get number the countries in region
countries_in_region <- countries_region()

#Clean responses 

resp_clean <- responses %>%
  pivot_longer(-country,
               values_to = 'responses' ) %>%
  mutate(survey = str_extract(name, ".+?(?=-)"),
         role = str_extract(name, "(?<=-).*")) %>%
  select(-name) %>%
  #drop combinations with no responses
  filter(!is.na(responses)) %>%
  filter(country != "TOTAL") %>%
  #get regional categories
  left_join(lkp_countries, by = 'country') %>%
  #Gambia didnt had a region, let's add it
  clean_regions()


#1. how many countries in each region by each survey?

resp_regions <- resp_clean %>%
  group_by(survey, region) %>%
  summarise(countries = length(unique(country)), .groups = 'drop') %>%
  #get number the countries in region
 left_join(countries_in_region, by = 'region') %>%
  #Count number of countries that did not respond in the region
  mutate(missing_countries = countries_in_region - countries) %>%
  pivot_longer(-c(region,survey, countries_in_region)) %>%
  mutate(name = factor(name))


#Plot --------------------------------

surveys = unique(resp_regions$survey)

categories <- levels(resp_regions$name)


create_labels <- function(cat){
  resp_regions %>%
    filter(name == cat) %>%
    filter(value > 0)
  
}

labels_countries <- create_labels('countries')

labels_m_countries <- create_labels('missing_countries')


resp_regions %>%
  ggplot(
    aes(x = value,
        y = reorder(region,countries_in_region),
        fill = reorder(name, value),
        label = value)
  ) +
  geom_col(width = .7) +
  #label countries
  geom_text(data = labels_countries,
            hjust =1.8,
            family= "Open Sans",
            color = 'white') +
  #label missing countries
  geom_text(data = labels_m_countries,
            hjust = 1,
            family= "Open Sans",
            color = 'gray',
            aes(x = countries_in_region)
            ) +
  #geom_text() +
  facet_wrap(~ survey) +
  #labels
  labs(title = "Surveys' Distribution of Responses.",
       subtitle = "Number of countries that responded and did not respond in each region."
       ) +
 scale_fill_manual(breaks = categories,
                   labels = c("Responded", "Did not response"),
                   values = rev(c(gray_light, blue_sky))
    
   ) +
  theme_main() +
  theme(axis.text.x = element_blank(),
        strip.background = element_rect(fill = blue_navy),
        strip.text = element_text(color = 'white'),
        plot.subtitle = element_text(size = 12)
        )


ggsave('report/sample/responses_by_country.png',
       units = 'cm',
       width = 16)





