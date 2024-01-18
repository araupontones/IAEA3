#Number of institutions by thematic area
#Creates plots that count the number of projects by thematic area, region, and foa
#exdir <- 'analysis/plots/3.institutions'

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
exdir <- 'analysis/plots/3.institutions'



#Load data ======================================================================
  
  institutions <- import('data/1.reference/clean/institutions.rds') %>%
  filter(!is.na(theme))
  
  
  names(institutions)
  
#Utils ----------------------------------------------------------------------
#count unique number of institution
unique_insts <- function(var){length(unique(var))}

#start counting ================================================================
  
  
  
  #Total number of institutions
  total_institutions = prettyNum(length(unique(institutions$Institution)), big.mark = ',')
  total_institutions
  
  
  
  
  institutions %>%
    group_by(theme) %>%
    #Count unique institutions by theme
    summarise(projects = unique_insts(Institution)) %>%
    plot_num_projects(theme = "Across Tematic Areas",
                      unit_title = "Institutions",
                      y_variable = theme,
                      num_projects_theme = total_institutions) 
   
   ggsave(glue('{exdir}/total_institutions.png'))

  
  
   institutions %>%
    group_by(theme, region) %>%
    summarise(projects = unique_insts(Institution),
              .groups = 'drop') %>%
    plot_num_projects(theme = "Across Tematic Areas",
                      unit_title = 'Institutions',
                      y_variable = theme,
                      num_projects_theme = total_institutions,
                      label_size = 2,
                      hjust = 1) +
    facet_wrap(~region)
  
  ggsave(glue('{exdir}/total_institutions_regions.png'))
  

  
  

#1. total projects within theme=====================================================

themes <- unique(institutions$theme)
export_plots <- lapply(themes, function(theme_){
  
  message(theme_)
  
  #Fetch theme details from lookup 
  theme_code_ <- find_in_lkp(institutions, fetch = 'theme_code', when = 'theme', equals = theme_)[1]
  theme_dir <- find_in_lkp(lkp_themes, 'dirname', 'theme_code', theme_code_)
  
  #define exit directory of this theme
  exdir_theme <- glue('{exdir}/{theme_dir}')
  
  if(!dir.exists(exdir_theme)){
    dir.create(exdir_theme)
    
  }
  
  
  #Define data of the theme ----------------------------------------------------
  db_theme <- institutions %>% filter(theme == theme_)
  
  #Count projects by theme
  num_projects_theme <- prettyNum(unique_insts(db_theme$Institution), big.mark = ',')
  
  #Count projects by Improvement -------------------------------------------------
  projects_foa <- db_theme %>%
    group_by(FOACode_new, FOA_new) %>%
    summarise(projects = unique_insts(Institution),
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
                      unit_title = "Institutions",
                      #For some cases there are 1:m improvements to foas
                      #So data must be re-groupped and re summarise
                      regroup = T,
                      num_projects_theme)
  
  
  
  #Plot using FOA name
  plot_foa <- projects_foa %>%
    plot_num_projects(theme_, 
                      y_variable = FOA_new,
                      unit_title = "Institutions",
                      regroup = F,
                      num_projects_theme)
  
 
  
  ggsave(glue('{exdir_theme}/insts_foas.png'),plot_foa)
  cli::cli_alert_info("done")
  ggsave(glue('{exdir_theme}/insts_improvements.png'),plot_imp)
  
  
  
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
                      unit_title = "Institutions",
                      regroup = T,
                      num_projects_theme, 
                      label_size = 3) +
    facet_wrap(~ region)
  
  
  #Plot by foa name ----------------------------------------------------------------
  plot_foa_reg <- projects_foa_region %>%
    plot_num_projects(theme_, 
                      y_variable = FOA_new,
                      unit_title = "Institutions",
                      regroup = F,
                      num_projects_theme, 
                      label_size = 3) +
    facet_wrap(~ region)

  
  ggsave(glue('{exdir_theme}/insts_improvements_reg.png'),plot_imp_reg)
  ggsave(glue('{exdir_theme}/insts_foas_reg.png'),plot_foa_reg)
  
  
  
  
  
})


 



