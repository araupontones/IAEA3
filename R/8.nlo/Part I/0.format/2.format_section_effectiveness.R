library(rio)
library(dplyr)
library(tidyr)
library(janitor)
gmdacr::load_functions('functions/')




#import raw data creted in 0.format/format_names_part_1.R
#the above script formats the data from SPSS to R so it is readable

raw_data <- import('data/7.NLO/1.raw/Part_1.rds') 

#define section
sections <- c(#"relevance")
  "effectiveness")
 # "sustainability")
#"regionalCoop",
#"future",
#"visibility")







#define themes ---------------------------------------------------------------
themes <- names(raw_data)[str_detect(names(raw_data), 'theme')]


#each team enabled a different section of the questionnaire.
#bring all themes in a tidy format
#the name of the variable (created in 0.format/format_names_part_1.R) contains all the information

append_themes <- lapply(themes, function(t){
  
  #get the suffix, it allows to identify the theme of the variables
  sufix = get_suffix(t, raw_data)
  
  
  r <- raw_data %>%
    #keep data for relevance section only,
    #and pivot it longer
    #function created in functions.
    keep_section(t, sections[1], sufix) %>%
    #   #the column name contains all the info:
    mutate(section = str_extract(name, '^.*?(?=-|_)'),
           foa = str_remove(name, '^.*_[a-z]{1,}_'),
           improvement = str_extract(foa, "_.{1,}$"),
           foa = str_remove(foa, improvement),
           improvement = str_remove(improvement, "_")
    ) %>%
    rename(period = value) %>%
    select(-name)
  
  
  
  
}) %>% do.call(rbind,.)


#clean relevance ---------------------------------------------------------------
periods <- sort(unique(append_themes$period))




clean_effectiveness <- append_themes %>%
  mutate(period = factor(period,
                         labels = periods,
                         ordered = T))

tabyl(clean_effectiveness, period)
#export ------------------------------------------------------------------------
export(clean_effectiveness, glue('data/7.NLO/2.raw_formatted/Part_1_{sections[1]}.rds'))
