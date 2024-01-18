#Parameters for report
library(stringr)
library(glue)
library(dplyr)
dir_project <- 'C:/repositaries/1.work/IAEA3'
dir_plots <- file.path(dir_project, 'analysis/plots')
dir_plots_projects <- file.path(dir_plots, "2.projects")
dir_plots_insts <- file.path(dir_plots, "3.institutions")



gmdacr::load_functions(file.path(dir_project,'functions'))