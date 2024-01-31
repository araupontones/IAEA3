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
library(ggplot2)
gmdacr::load_functions('functions')
gmdacr::load_functions('functions/themes/')
lkp_themes <- import('data/9.lookups/themes.csv')


#check which countries responded to each survey and each theme
all <- countries_by_theme()





#1. how many countries in each region by each survey?
  

themes <- sort(unique(all$id_theme))


exdir = 'report/sample'

#create labels ------------------------------------------------------------
create_labels2 <- function(db,cat){
  db %>%
    filter(name == cat) %>%
    filter(value > 0)
  
}


plots <- lapply(themes, function(t){
  
  
  theme_name = find_in_lkp(db_lkp = lkp_themes, fetch = 'theme', when = "theme_code", equals = t)
  theme_short =find_in_lkp(db_lkp = lkp_themes, fetch = 'short', when = "theme_code", equals = t)
  
  message(theme_name)
  
  exfile = glue('{exdir}/responses_{theme_short}.png')

  resp_regions2 <- all %>%
    filter(id_theme == t) %>%
    get_regions() %>%
    group_by(survey, region) %>%
    summarise(countries = length(unique(country)), .groups = 'drop') %>%
    #get number the countries in region
    left_join(countries_in_region, by = 'region') %>%
    #Count number of countries that did not respond in the region
    mutate(missing_countries = countries_in_region - countries) %>%
    pivot_longer(-c(region,survey, countries_in_region)) %>%
    mutate(name = factor(name,
                         labels = c("Responded", "Did not response"),
                         ordered = T
                         ))




  labels_countries <- create_labels2(db = resp_regions2,cat = 'Responded')
  labels_m_countries <- create_labels2(db =resp_regions2,cat ='Did not response')
  categories <- levels(resp_regions2$name)
  print(labels_countries)
  # 
  # 
  # 
  resp_regions2 %>%
    ggplot(
      aes(x = value,
          y = reorder(region,countries_in_region),
          fill = name,
          label = value)
    ) +
    geom_col(width = .7,
             position = position_stack(reverse = T)
             ) +
    #label countries
    geom_text(data = labels_countries,
              hjust =1.8,
              size = 3,
              family= "Open Sans",
              color = 'white') +
    #label missing countries
    geom_text(data = labels_m_countries,
              hjust = 1,
              size = 3,
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
                      #labels = c("Responded", "Did not response"),
                      values = rev(c(gray_light, blue_sky))
                      ) +
    theme_main() +
    theme(axis.text.x = element_blank(),
          strip.background = element_rect(fill = blue_navy),
          strip.text = element_text(color = 'white'),
          plot.subtitle = element_text(size = 12)
    )
  # 
  ggsave(exfile,
         units = 'cm',
         width = 16)
  # 
  # return(last_plot())
})

plots[[1]]
  