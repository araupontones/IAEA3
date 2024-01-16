#Number of projects by thematic area
#Creates plots that count the number of projects by thematic area, region, and foa
#exdir <- 'analysis/plots/2.projects'

library(rio)
library(dplyr)
library(tidyr)
library(janitor)
library(stringr)
library(glue)
library(ggplot2)
gmdacr::load_functions('functions')
gmdacr::load_functions('functions/themes/')

#Files are exported here:
exdir <- 'analysis/plots/2.projects'



#Load data ======================================================================
  
  projects <- import('data/1.reference/clean/projects.rds') %>%
  #Drop projects from theme 7 (not of interest for the report)
  filter(!is.na(theme))

#get the improvements so we can translate from FOA to improvement
  lkp_improvements <- import('data/9.lookups/improvements.rds')
  lkp_themes <- import('data/9.lookups/themes.csv')


#Total projects by thematic area
  total_projects = prettyNum(nrow(projects), big.mark = ',')
  
  projects %>%
    group_by(theme) %>%
    summarise(projects = n()) %>%
    plot_num_projects(theme = "Across Tematic Areas",
                      y_variable = theme,
                      num_projects_theme = total_projects) 
   
   ggsave(glue('{exdir}/total_projects.png'))

  
  
  projects %>%
    group_by(theme, region) %>%
    summarise(projects = n()) %>%
    plot_num_projects(theme = "Across Tematic Areas",
                      y_variable = theme,
                      num_projects_theme = total_projects,
                      label_size = 2,
                      hjust = 1) +
    facet_wrap(~region)
  
  ggsave(glue('{exdir}/total_projects_regions.png'))
  

  
  

#1. total projects within theme=====================================================


export_plots <- lapply(themes, function(theme_){
  
  message(theme_)
  
  #Fetch theme details from lookup 
  theme_code_ <- find_in_lkp(projects, fetch = 'theme_code', when = 'theme', equals = theme_)[1]
  theme_dir <- find_in_lkp(lkp_themes, 'dirname', 'theme_code', theme_code_)
  
  #define exit directory of this theme
  exdir_theme <- glue('{exdir}/{theme_dir}')
  
  if(!dir.exists(exdir_theme)){
    dir.create(exdir_theme)
    
  }
  
  
  #Define data of the theme ----------------------------------------------------
  db_theme <- projects %>% filter(theme == theme_)
  
  #Count projects by theme
  num_projects_theme <- prettyNum(nrow(db_theme), big.mark = ',')
  
  #Count projects by Improvement -------------------------------------------------
  projects_foa <- db_theme %>%
    group_by(FOACode_new, FOA_new) %>%
    summarise(projects = n(),
              .groups = 'drop') %>%
    #Get the name of the improvement
    left_join(filter(lkp_improvements, theme_code == theme_code_ ),
              by = "FOACode_new") %>%
    #Create this only to avoid problems in the future
    mutate(region = "1")
  
  
  #PLOTS by themes ------------------------------------------------------------
  #Plot projects by FOA (using improvement)
  plot_imp<- projects_foa %>%
    plot_num_projects(theme_, 
                      y_variable = Improvement,
                      #For some cases there are 1:m improvements to foas
                      #So data must be re-groupped and re summarise
                      regroup = T,
                      num_projects_theme)
  
  
  
  #Plot using FOA name
  plot_foa <- projects_foa %>%
    plot_num_projects(theme_, 
                      y_variable = FOA_new,
                      regroup = F,
                      num_projects_theme)
  
 
  
  ggsave(glue('{exdir_theme}/projects_foas.png'),plot_foa)
  cli::cli_alert_info("done")
  ggsave(glue('{exdir_theme}/projects_improvements.png'),plot_imp)
  
  
  
#By regions ====================================================================
  
  #by region 
  projects_foa_region <- db_theme %>%
    group_by(FOACode_new, FOA_new,region) %>%
    #Count projects by region and by foa
    summarise(projects = n(),
              .groups = 'drop') %>%
    left_join(filter(lkp_improvements, theme_code == theme_code_ )) 
  
  #plot by Improvement--------------------------------------------------
  plot_imp_reg <- projects_foa_region %>%
    plot_num_projects(theme_, 
                      y_variable = Improvement,
                      regroup = T,
                      num_projects_theme, 
                      label_size = 3) +
    facet_wrap(~ region)
  
  
  #Plot by foa name ----------------------------------------------------------------
  plot_foa_reg <- projects_foa_region %>%
    plot_num_projects(theme_, 
                      y_variable = FOA_new,
                      regroup = F,
                      num_projects_theme, 
                      label_size = 3) +
    facet_wrap(~ region)

  
  ggsave(glue('{exdir_theme}/projects_improvements_reg.png'),plot_imp_reg)
  ggsave(glue('{exdir_theme}/projects_foas_reg.png'),plot_foa_reg)
  
  
  
  
  
})


 



