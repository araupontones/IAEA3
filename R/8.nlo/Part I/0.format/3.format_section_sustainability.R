library(rio)
library(dplyr)
library(tidyr)
library(janitor)
library(stringr)
library(forcats)
gmdacr::load_functions('functions/')




#import raw data creted in 0.format/format_names_part_1.R
#the above script formats the data from SPSS to R so it is readable

raw_data <- import('data/7.NLO/1.raw/Part_1.rds') 

#Mapping of foas
mapping_foas <- import('data/1.reference/mapping_foas.xlsx') %>%
  #updating the name of the foa
  select(foa,
         foa_nlo1_effectiveness, 
         improvement) %>%
  filter(!is.na(foa_nlo1_effectiveness)) %>%
  group_by(foa_nlo1_effectiveness) %>%
  slice(1) %>%
  ungroup()



#lookup outcomes (to be consistent with ToC)

lkp_outcomes <- import('data/9.lookups/outcomes.xlsx')



#define section
sections <- c("sustainability")
              







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
           improvement = str_remove(improvement, "_"), 
           improvement = str_trim(improvement),
           improvement = str_to_sentence(improvement),
           improvement = str_replace_all(improvement, "/ ", "/"),
           improvement = str_replace_all(improvement, "education centres", "education centre")
           ) %>%
    rename(period = value,
           foa_nlo1_effectiveness = foa,
           outcome_nlo = improvement ) %>%
   select(-name)
  
  
  
  
}) %>% do.call(rbind,.)





#clean relevance ---------------------------------------------------------------
periods <- sort(unique(append_themes$period))


message(paste("Before: ", nrow(append_themes)))

clean_sustainability <- append_themes %>%
  mutate(period = factor(period,
                            labels = periods,
                         ordered = T)) %>%
  #get the foas
  mutate(foa_nlo1_effectiveness = str_trim(foa_nlo1_effectiveness),
         foa_nlo1_effectiveness = str_replace_all(foa_nlo1_effectiveness, "  ", " "),
         foa_nlo1_effectiveness = str_replace_all(foa_nlo1_effectiveness, "Human health", "human health"),
         foa_nlo1_effectiveness = str_replace_all(foa_nlo1_effectiveness, "afecting", "affecting")) %>%
  left_join(mapping_foas, by = c("foa_nlo1_effectiveness")) %>%
  #get the outcomes
  left_join(lkp_outcomes) %>%
  select(-outcome_nlo) %>%
  #identifit those countries that have achieved it
  mutate(achieved = ifelse(is.na(period)|period == "N/A", 0,1),
         period = fct_recode(period, "< 2000" = "> 2000"))


#export ------------------------------------------------------------------------
export(clean_sustainability, 'data/7.NLO/2.raw_formatted/Part_1_sustainability.rds')
