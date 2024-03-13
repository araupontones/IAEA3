#export data to power BI
library(rio)
library(dplyr)
library(rio)
library(stringr)
#function that defines the questions for each foa in NLO2
source('functions/get_vars_nlo2.R')


#define data =============================================================
dir_nlo <-  'data/7.NLO/2.raw_formatted'
sections <- list.files(dir_nlo, pattern = "Part_2", full.names = T)

#load dictionary of foas and keep relevant variables for NLO2
lkp_foas <- import('data/1.reference/mapping_foas.xlsx') %>%
  select(foa_nlo2, foa,improvement ) %>%
  filter(!is.na(foa_nlo2)) %>%
  group_by(foa_nlo2) %>%
  slice(1) %>%
  ungroup()




#read food and agriculture
db_theme <- import(file.path(dir_nlo, "Part_2_fa.rds"))


names(db_theme)

#identify foas in dataset (the data set is structured in a wide format)
#each foa has its column. And are named, foa_1, foa_2, etc.
#So we can identify all the variables that are associated with the foas

foas <- names(db_theme)[which(str_detect(names(db_theme), "foa"))]

#define the questions of each foa, this is done by looking at the questionnaire
#The questionnaire was designed to select multiple foas, for each foa, a section
#was enabled
#this function is defined in functions
vars_foa <- get_vars_nlo2(theme = 'fa')


#split the data for each foa ==================================================
db_foas <- lapply(foas, function(foa){
  
  
  before_join <- db_theme %>%
    select(#RespondentID, 
           country, role,theme, 
           #only this foa
           all_of(foa),
           #the variables associated to this foa (defined in line 42)
           all_of(vars_foa[[foa]])) %>%
    #rename the foa variable to match with lkp_foas
    rename_at(vars(starts_with("foa")), function(x)x = "foa_nlo2") %>%
    #filterout rows that were not answer for this foa
    filter(!is.na(foa_nlo2))
  
  #count number of unanswered questions
  before_join$na_count <- apply(before_join, 1, function(x) sum(x ==""))
  
  #keep only answered sections
  answered_section <- before_join %>%
    filter(na_count < length(vars_foa[[foa]]) ) %>%
    select(-na_count)
  
  
  message(paste("Before: ", nrow(answered_section)))
  
  #get the names of the foas and improvements of the evaluation framework
  after_join <- answered_section %>%
    left_join(lkp_foas) %>%
    relocate(foa, improvement, .after = theme ) %>%
    arrange(country)
    
  message(paste("After:", nrow(after_join)))
  
  #export data in excel format
  exfile <- print(unique(after_join$foa))
  extheme <- print(unique(after_join$theme))
  export(after_join, glue('data/7.NLO/6.NLO2_formatted_foas/{extheme}/{exfile}.xlsx'), overwrite = T)
  
  return(after_join)
  
  
})

names(db_foas) <- foas



#create a codebook ===========================================================
questions <- names(db_theme)[which(str_detect(names(db_theme), "q00"))]
codebook <- lapply(questions, function(question){
  
  #find the foa that corresponds to this question
  for(i in c(1:length(db_foas))){
    
    #check in which db_foa this question exits
    it_is <- question %in% names(db_foas[[i]])
    
    #and fetch the foa and improvement for it
    if(it_is){
      
      my_foa <- unique(db_foas[[i]]$foa)
      improvement <- unique(db_foas[[i]]$improvement)
    }
    
  }
  
  
  
  tibble(
    code = question,
    question = attributes(db_theme[[question]])$label,
    foa = my_foa,
    improvement = improvement
  )
}) %>% do.call(rbind,.)

export(codebook, glue('data/7.NLO/6.NLO2_formatted_foas/codebook_FOOD_AGR_NLO2.xlsx'))

