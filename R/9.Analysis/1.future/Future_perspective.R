# Future perspectives

#==============???????????????????????????????????===============================
# Research question

#? how important will the contribution of the TC programme be in achievingresults
# in the following areas in your country over the next 5-10 years?




library(rio)
library(ggplot2)
library(RColorBrewer)
library(extrafont)
library(janitor)
library(dplyr)
library(scales)
library(glue)
library(stringr)

gmdacr::load_functions('functions/')
gmdacr::load_functions('functions/analysis_future/')
gmdacr::load_functions('functions/themes/')
gmdacr::load_functions('functions/plots//')

#import data ------------------------------------------------------------------
future_raw <- import('data/7.NLO/2.raw_formatted/Part_1_future.rds')
countries_lkp <- import('data/9.lookups/countries.rds') %>% select(-region)



#drop missing answers 
future_clean <- future_raw %>%
  dplyr::filter(!is.na(likert)) %>%
  left_join(countries_lkp)



#check countries that asked these questions
who_answered <- future_clean %>%
  group_by(country) %>%
  slice(1)

knitr::combine_words(sort(who_answered$country[who_answered$region == "Latin America and the Caribbean"]))
length(who_answered$country[who_answered$region == "Latin America and the Caribbean"])
#define categories of th likert scale, later used in the charts
categories <- levels(future_clean$likert)






#define aggregations ===========================================================


list_analysis <- list(
  #1. between thematic areas ---------------------------------------------------
  list(
    group_1= c('theme', 'likert'),
    group_2 = c('theme'),
    subtitle = "Between Themes."
  ),
  #2. between geographic areas ------------------------------------------------
  list(
    group_1= c('region','theme', 'likert'),
    group_2 = c('region','theme'),
    subtitle = "Between Themes & Regions."
    )
  ,
  
  #3. between LDC status ------------------------------------------------------
  list(
    group_1= c('ldc','theme', 'likert'),
    group_2 = c('ldc','theme'),
    subtitle = "Between Themes & LDC status."
    )
  ,
  
  #between year joined ---------------------------------------------------------
  list(
    group_1= c('joined','theme', 'likert'),
    group_2 = c('joined', 'theme'),
    subtitle = "Between Themes & Year Joined."
  )
  
  
  )





#create plots
plots_future  <- lapply(c(1:length(list_analysis)), function(i){
  
  pl <- charts_future(i, db = future_clean)
  
  subtitulo =  list_analysis[[i]]$subtitle
  #export chart
  ggsave(glue('analysis/plots/1.future/{subtitulo}.png'),
         pl,
         units = 'cm')
  
}

)


#By FOA ========================================================================


#define all themes in the data
temas <- levels(future_clean$theme)
temas

#loop over themes
analysis_themes <- lapply(temas, function(tema){
  
  message(tema)
  #define parameters of the theme (used to export and organize plots)
  theme_id <- ids_themes(str_to_title(tema))
  theme_sufix <- theme_suffix(theme_id)
  
  #filter data of the team
  data_theme <- filter(future_clean,theme == tema)  %>%
     mutate(theme = as.character(tema),
            foa = as.character(foa)) %>%
    #doing this so I dont have to modify the function used above
    select(-theme) %>%
     rename(theme = foa)
  
  
  
  
  plots_future  <- lapply(c(1:length(list_analysis)), function(i){
    
    
   #create plot
    pl <- charts_future(i, db = data_theme)

    pl_2 <- pl +
      scale_y_discrete(labels = label_wrap_gen(40))

    subtitulo =  list_analysis[[i]]$subtitle
    subtitulo = str_replace(subtitulo, "Theme", "FoA")
    message(subtitulo)
    #export chart
    ggsave(glue('analysis/plots/1.future/{theme_sufix}_{subtitulo}.png'),
           pl_2,
           units = 'cm')
    
  }
  
  )
  
  
  
  })


