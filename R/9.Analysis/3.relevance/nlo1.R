library(rio)
library(ggplot2)
library(RColorBrewer)
library(extrafont)

gmdacr::load_functions('functions')
gmdacr::load_functions('functions/themes/')


#How would you define the degree to which the TCP has contributed in
#achieving results in each are of support in your country
exdir <- 'analysis/plots/4.relevance'

relevance <- import('data/7.NLO/2.raw_formatted/Part_1_relevance.rds')

#attributes themes
lkp_themes <- import('data/9.lookups/themes.csv')



clean_relevance <- filter(relevance, !is.na(value)) %>%
  relocate(id,theme,FOA_new, period, value) 
  


#maximium degree of contribution -----------------------------------------------
degree_max <- clean_relevance %>%
  group_by(id, country, region, FOA_new, theme) %>%
  summarise(degree_contribution = max(value), .groups = 'drop') %>%
   mutate(regional_master = degree_contribution %in% c("Regional reference", "Master stage"),
          initial_established =  degree_contribution %in% c("Initial stage", "Established stage"),
          not_applicable = degree_contribution == "Not applicable"
   )


tabyl(degree_max, degree_contribution, FOA_new) 

#Increase at least from one step -----------------------------------------------

degree_increase <- clean_relevance %>%
  group_by(id, country, region, FOA_new, theme) %>%
  arrange(id,FOA_new,theme, period) %>%
  #check that the stage in period is greater than the last one
  summarise(periods = n(),
            min = min(value),
            max = max(value)
            ) %>%
  #check if the stage improve 
  mutate(better = ifelse(min < max, T, F)) %>%
  relocate(better, .after = theme)  %>%
  #only estimate for those that have more than one period
  dplyr::filter(periods > 1) %>%
  mutate(t = min < max)




#PLOTS==========================================================================


#Maximum dregree of contribution accross themes

data_theme <- degree_max %>% group_relevance(c('theme'))

data_theme %>%
  plot_relevance()

  ggsave(glue('{exdir}/relevance_btwn_themes.png'))


  #How would you define the degree to which the TCP has contributed in
  #achieving results in each are of support in your country
  



#By region
data_theme_region <- degree_max %>% group_relevance(c('region', "theme"))
  
data_theme_region %>%
  plot_relevance() +
  facet_wrap(~region)
  

ggsave(glue('{exdir}/relevance_btwn_themes_regions.png'))



#relevance by foa ============================================================
names(lkp_themes)
exdir_theme <-find_in_lkp(lkp_themes,fetch = 'dirname',when = 'theme', equals = 'Food and Agriculture')
exit <- file.path(exdir, exdir_theme)
if(!dir.exists(exit)){
  
  dir.create(exit)
}

foas <- degree_max %>%
  filter(theme == 'FOOD and AGRICULTURE') %>%
  group_perc(c('FOA_new'),
             perc_of = 'degree_contribution',
             sort_cat = c("Regional reference","Master stage")
             ) %>%
    ggLikertbarsRelevance(titulo = 'Degree to which the TCP has contributed in achieving results.',
                          subtitulo = 'Between FOAs of Food and Agriculutre.')
    
foas


#relevance by foa and by region ================================================

  foas_region  <- degree_max %>%
    filter(theme == 'FOOD and AGRICULTURE') %>%
    group_perc(c('region','FOA_new'),
               perc_of = 'degree_contribution',
               sort_cat = c("Regional reference","Master stage")
    ) %>% 
    ggLikertbarsRelevance(titulo = 'Degree to which the TCP has contributed in achieving results.',
                          subtitulo = 'Between FOAs of Food and Agriculutre.') +
    facet_wrap(~ region)


ggsave(glue('{exdir}/{exdir_theme}/between_foas.png'), foas)
ggsave(glue('{exdir}/{exdir_theme}/between_foas_region.png'), foas_region)


#increase ========================================================================
group_better <- function(.data, by){
  .data %>%
    group_by_at(by) %>%
    summarise(better = mean(better), .groups = 'drop') %>%
    arrange(better)
  
}


plot_better <- function(.data){
  
  .data %>%
    ggplot(
      aes(x = better,
          y = reorder(FOA_new, better),
          label = percent(better,1)
    ))+
    geom_col(fill = blue,
             width = .8) +
    geom_text(hjust = 1,
              color = 'white',
              family = "Open Sans") +
    scale_x_continuous(labels = percent,
                       limits = c(0,1),
                       expand = c(0,0)
                       ) +
    scale_y_discrete(label = label_wrap(25)) +
    labs(title = "Areas where the TCP contributed to increase development stage") +
    theme_main()
  
}

degree_increase %>% filter(theme == 'FOOD and AGRICULTURE' ) %>%
  group_better(by = "FOA_new") %>%
  plot_better()


ggsave(glue('{exdir}/{exdir_theme}/stages_increase.png'))

degree_increase %>% filter(theme == 'FOOD and AGRICULTURE' ) %>%
  group_better(by = c("FOA_new", 'region')) %>%
  plot_better() +
  facet_wrap(~region)

ggsave(glue('{exdir}/{exdir_theme}/stages_increase_region.png'))





  