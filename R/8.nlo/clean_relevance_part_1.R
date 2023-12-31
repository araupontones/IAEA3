library(rio)
library(dplyr)
library(tidyr)
library(janitor)
gmdacr::load_functions('functions/')




#import raw data creted in 0.format/format_names_part_1.R
#the above script formats the data from SPSS to R so it is readable

raw_data <- import('data/7.NLO/1.raw/Part_1.rds') 

#define section
sections <- c("relevance")
             # "effectiveness",
              #"sustainability",
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
    keep_section(t, 'relevance', sufix) %>%
    #the column name contains all the info:
    mutate(section = str_extract(name, '^.*?(?=-|_)'),
           foa = str_remove(name, '^.*_[a-z]{1,}_'),
           period = str_extract(name,'_.[0-9]{1,}_[0-9]{1,}$'),
           foa = str_remove(foa, period),
           foa = str_trim(foa),
           period = str_remove(period,'_'),
           period = str_replace(period, '_', '-')
    ) %>%
    select(-name)
  
  
  
  
}) %>% do.call(rbind,.)


#clean relevance ---------------------------------------------------------------
periods <- sort(unique(append_themes$period))
categories <- c('Not applicable', 'Initial stage;', 'Established stage', 'Master stage', 'Regional
reference')


clean_relevance <- append_themes %>%
  mutate(period = factor(period,
                            labels = periods,
                         ordered = T),
         value = case_when(value == 0 ~ categories[1],
                           value == 1 ~ categories[2],
                           value == 2 ~ categories[3],
                           value == 3 ~ categories[4],
                           value == 4 ~ categories[5],
                           T ~ NA_character_
                           
                           ),
         value = factor(value,
                        labels = categories,
                        ordered = T)
  )

#export ------------------------------------------------------------------------
export(clean_relevance, 'data/7.NLO/2.raw_formatted/Part_1_relevance.rds')
