library(rio)
library(dplyr)
library(tidyr)
library(janitor)
library(stringr)
library(forcats)
gmdacr::load_functions('functions/')




#import raw data creted in 0.format/format_names_part_1.R
#the above script formats the data from SPSS to R so it is readable

names(raw_data)
raw_data <- import('data/7.NLO/1.raw/Part_1.rds') 

#names(mapping_foas)
#Mapping of foas
mapping_foas <- import('data/1.reference/mapping_foas.xlsx') %>%
  #updating the name of the foa
  select(theme,
         foa,
       foa_nlo1_effectiveness, 
         improvement) %>%
  filter(!is.na(foa)) %>%
  group_by(theme,foa) %>%
  slice(1) %>%
  ungroup()



#mapping_foas$foa[mapping_foas$theme == 'Health and Nutrition']

#lookup outcomes (to be consistent with ToC)

lkp_outcomes <- import('data/9.lookups/int_outcomes.xlsx')

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
           int_outcome_nlo = str_extract(foa, "_.{1,}$"),
           foa = str_remove(foa, int_outcome_nlo),
           int_outcome_nlo = str_remove(int_outcome_nlo, "_"),
           int_outcome_nlo = str_trim(int_outcome_nlo),
           int_outcome_nlo = ifelse(str_detect(int_outcome_nlo, "Upgrade institutions"), "Upgraded institutions / Laboratories/ Educational centers", int_outcome_nlo)
    ) %>%
    rename(period = value,
           foa_nlo1_effectiveness = foa) %>%
    select(-name)
  
  
  
  
}) %>% do.call(rbind,.)

#clean relevance ---------------------------------------------------------------
periods <- sort(unique(append_themes$period))

tabyl(clean_effectiveness, theme)

#14,725
clean_effectiveness <- append_themes %>%
  mutate(period = factor(period,
                         labels = periods,
                         ordered = T)) %>%
  mutate(foa_nlo1_effectiveness = str_trim(foa_nlo1_effectiveness),
         foa_nlo1_effectiveness = str_replace_all(foa_nlo1_effectiveness, "  ", " "),
         theme = from_nlo_to_foas(theme)) %>%
    #get the improvements
  left_join(mapping_foas,
            by = c("foa_nlo1_effectiveness", "theme")
            ) %>%
    #get the outcomes
    left_join(lkp_outcomes) %>%
  select(-int_outcome_nlo) %>%
  #identifit those countries that have achieved it
  mutate(achieved = ifelse(is.na(period)|period == "N/A", 0,1),
         period = fct_recode(period, "< 2000" = "> 2000"))


tabyl(clean_effectiveness, theme)
my_theme = filter(clean_effectiveness, theme == "Water and the Environment")



tabyl(my_theme, foa)
tabyl(my_theme, foa_nlo1_effectiveness)
#foa_nlo1_effectiveness
sections
#export ------------------------------------------------------------------------
export(clean_effectiveness, glue('data/7.NLO/2.raw_formatted/Part_1_{sections[1]}.rds'))
#export(clean_effectiveness, glue('data/11.powerbi/{sections}.csv'))
