library(glue)
lkp_themes = import('data/9.lookups/themes.csv')
exdir = 'report/sample'
#countries by theme is in functions/countries_by_theme_in_surveys
all <- countries_by_theme(min_answers = 15) #min answers allowed to keep

#check participation by theme
by_theme <- all %>%
 group_by(country, theme, id_theme, survey) %>%
  summarise(total = n(), .groups = 'drop') %>%
  pivot_wider(id_cols = c(country, theme, id_theme),
              values_from = total,
              names_from = survey) %>%
  #in util responses
  responses_surveys_themes()


themes <- unique(by_theme$id_theme)


plots <- lapply(themes, function(t){
  theme_name = find_in_lkp(db_lkp = lkp_themes, fetch = 'theme', when = "theme_code", equals =t)
  dirmane =find_in_lkp(db_lkp = lkp_themes, fetch = 'dirname', when = "theme_code", equals = t)
  
  exfile = glue('{exdir}/map_{dirmane}.png')
  message(exfile)
  
  
  data_plot <- by_theme %>%
    filter(id_theme == t) %>%
    full_join(world, by = 'country'
    ) %>%
    sf::st_as_sf() %>%
    mutate(#status = ifelse(is.na(status), 'N/A', status),
      status = factor(status,
                      levels =c('all_surveys', 'only_cp',
                                'cp_nlo1', 'cp_nlo2',
                                'nlos_no_cp', 'only_nlo2'
                      ),
                      ordered = T)
    )
  
  
  ggplot(data = data_plot,
         aes(fill = status)) +
    geom_sf() +
    scale_fill_brewer(palette = 'Set3',direction=-1) +
    theme_bw()+
    theme_main()+
    labs(title = glue("{theme_name}: Participation to Surveys.")) +
    theme(legend.position = 'bottom',
          plot.title = element_text(size = 12),
          legend.title = element_blank(),
          axis.text.x = element_blank())
  
  theme_main
  ggsave(exfile, last_plot())
  
})

