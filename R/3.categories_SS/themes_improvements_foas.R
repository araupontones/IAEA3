library(googlesheets4)
library(dplyr)
library(rio)
library(tidyr)
library(stringr)

url_gs <- 'https://docs.google.com/spreadsheets/d/15SY9gpr5ELo0dLRYZNjqzNzOKbF8JYtkI42eBIZDe68/edit#gid=597634711'

#themes ========================================================================
raw_themes <- read_sheet(url_gs, 'themes')

names(raw_themes)
cat_themes <- raw_themes %>%
  #excluding this because it is not part of the report
  filter(Theme != "Nuclear Knowledge Development and Management") %>%
  rename(title = Theme,
         value = theme_code)


export(cat_themes,'questionnaires/categories/themes.xlsx', sheetName = "Categories", overwrite = T)

#Improvements ==================================================================
#' Generate a list of the improvements  

raw_improvements <- read_sheet(url_gs, "improvements") 
names(raw_improvements)
cat_improvements <- raw_improvements %>%
  #drop rows used for comments in sheet
  dplyr::filter(!str_detect(improvement_code, "Q"),
                improvement_code != "NULL") %>%
  #dropping this because IAEA asked to 
  dplyr::filter(Improvement != "Knowledge on application and development of Small Modular Reactors (SMRs)") %>%
  group_by(Improvement) %>%
  slice(1) %>%
  ungroup() %>%
  select(title = Improvement,
         value = improvement_code) %>%
  arrange(as.numeric(value))



export(cat_improvements,'questionnaires/categories/improvements.xlsx', sheetName = "Categories", overwrite = T)

#' 
#' #Improvement levels ============================================================
#' #'these are the likert scale for each improvement 
#' cat_levels <- raw_improvements %>%
#'   #'keep only the code and the levels
#'   select(improvement_code,
#'          starts_with("Level")) %>%
#'   pivot_longer(-improvement_code,
#'                names_to = 'level',
#'                values_to = 'title') %>%
#'   filter(!is.na(title)) %>%
#'   #' assign a code to the general categories
#'   #' They start with 0 so No capacity is always the first option in the questionnaire
#'   mutate(improvement_code = ifelse(is.na(improvement_code), 0, improvement_code),
#'          level = stringr::str_remove(level,"Level "),
#'          value = paste0(improvement_code, level)
#'          ) %>%
#'   arrange(value)
#' 
#' 
#' export(select(cat_levels, title, value),'questionnaires/categories/improvements_levels.xlsx', sheetName = "Categories", overwrite = T )
#' 
#' 
#' #look up table whether improvement has custom categories =======================
#' #' Some improvements do not have a custom level
#' #' For those, we use the general category
#' lkp_levels <- cat_improvements %>%
#'   #identify those that do not have a custom level
#'   mutate(has_level = value %in% unique(cat_levels$improvement_code),
#'          has_level = ifelse(has_level,1,0)) %>%
#'   filter(!is.na(value)) %>%
#'   select(rowcode = value,
#'          has_level)
#' 
#' export(lkp_levels,'questionnaires/categories/lkp_improvement_levels.txt', sep = "\t" )
#' 
#' 
#' #TYPES =======================================================================
#' 
#' 
#' 
#' cat_types <-create_cat_options(raw_improvements, starts_con = "Type ", names_a = "type", new_var = type)
#' 
#' export(select(cat_types, title, value),'questionnaires/categories/improvements_types.xlsx', sheetName = "Categories", overwrite = T )
#' 
#' 
#' #
#' lkp_types <- cat_improvements %>%
#'   #identify those that do not have a custom level
#'   mutate(has_type = value %in% unique(cat_types$improvement_code),
#'          has_type = ifelse(has_type,1,0)) %>%
#'   filter(!is.na(value)) %>%
#'   select(rowcode = value,
#'          has_type)
#' 
#' 
#' export(lkp_types,'questionnaires/categories/lkp_improvement_types.txt', sep = "\t" )
#' 
#' #look up table whether improvement has custom categories =======================
#' #' Some improvements do not have a custom level
#' #' For those, we use the general category
#' lkp_levels <- cat_improvements %>%
#'   #identify those that do not have a custom level
#'   mutate(has_level = value %in% unique(cat_levels$improvement_code),
#'          has_level = ifelse(has_level,1,0)) %>%
#'   filter(!is.na(value)) %>%
#'   select(rowcode = value,
#'          has_level)
#' 
#' export(lkp_levels,'questionnaires/categories/lkp_improvement_levels.txt', sep = "\t" )
#' 
#' 
#' #look up table FOAs of improvement =============================================
#' lkp_foas <- raw_improvements %>%
#'   select(FoA_code, rowcode = improvement_code) %>%
#'   group_by(rowcode) %>%
#'   mutate(row = row_number()) %>%
#'   ungroup() %>%
#'   pivot_wider(rowcode,
#'               values_from = FoA_code,
#'               names_from = row,
#'               names_prefix = 'foa_') %>%
#'   filter(!is.na(rowcode))
#' 
#' export(lkp_foas,'questionnaires/categories/lkp_improvement_foas.txt', sep = "\t" )
