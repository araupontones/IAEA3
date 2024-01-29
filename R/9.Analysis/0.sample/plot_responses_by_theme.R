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
gmdacr::load_functions('functions')
gmdacr::load_functions('functions/themes/')

#read file, it was given by Eloisa via email
sav_file <- "data/7.NLO/1.raw/IAEA TC PROGRAMME ACHIEVEMENTS IN THE 21ST CENTURY - PART I.sav"
sav_file2 <- "data/7.NLO/1.raw/IAEA TC PROGRAMME ACHIEVEMENTS IN THE 21ST CENTURY - PART II.sav"
raw_data <- read_sav(sav_file)
raw_data2 <- read_sav(sav_file2)
raw_cps <- import('data/6.data-collection/4.clean/cps.rds')

countries_in_region <- countries_region()

keep_areas <- function(.data, survey){
  .data %>%
  #mutate_all(as.character) 
  rename(country = q0002) %>%
  mutate(country = susor_get_stata_labels(country)) %>%
  select(country, starts_with('q0004')) %>%
  mutate(across(starts_with('q'), function(x)susor_get_stata_labels(x))) %>%
  pivot_longer(-country,
               values_to = 'theme') %>%
  filter(!is.na(theme)) %>%
  select(-name) %>%
  mutate(survey = survey)
}


#append all surveys ===========================================================
nlo1 <- raw_data %>% keep_areas('nlo1')
nlo2 <- raw_data2 %>% keep_areas('nlo2')
cp <- raw_cps %>% select(country, theme) %>%
  mutate(theme = susor_get_stata_labels(theme),
         survey = 'cp')



all <- do.call(rbind, list(nlo1, nlo2, cp)) %>%
  mutate(theme = str_to_sentence(theme),
         id = ids_themes(theme),
         theme = names_themes_2(id)) %>%
  clean_countries()

#count and graph
tabyl(resp_regions, region)
#1. how many countries in each region by each survey?
  
lkp_themes <- import('data/9.lookups/themes.csv')
themes <- sort(unique(all$id))
names(lkp_themes)
exdir = 'report/sample'
themes
lapply(themes, function(t){
  
  message(t)
  theme_name = find_in_lkp(db_lkp = lkp_themes, fetch = 'theme', when = "theme_code", equals = t)
  theme_short =find_in_lkp(db_lkp = lkp_themes, fetch = 'short', when = "theme_code", equals = t)
  
  message(theme_name)
  
  exfile = glue('{exdir}/responses_{theme_short}.png')

  resp_regions <- all %>%
    filter(id == t) %>%
    get_regions() %>%
    group_by(survey, region) %>%
    summarise(countries = length(unique(country)), .groups = 'drop') %>%
    #get number the countries in region
    left_join(countries_in_region, by = 'region') %>%
    #Count number of countries that did not respond in the region
    mutate(missing_countries = countries_in_region - countries) %>%
    pivot_longer(-c(region,survey, countries_in_region)) %>%
    mutate(name = factor(name))


  #create labels ------------------------------------------------------------
  create_labels <- function(cat){
    resp_regions %>%
      filter(name == cat) %>%
      filter(value > 0)

  }

  labels_countries <- create_labels('countries')

  labels_m_countries <- create_labels('missing_countries')
  categories <- levels(resp_regions$name)


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
    facet_wrap(~survey) +
    #labels
    labs(title = glue("{theme_name}: Surveys' Distribution of Responses."),
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

  ggsave(exfile,
         units = 'cm',
         width = 16)
  
})

  