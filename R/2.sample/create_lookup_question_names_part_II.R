
#For each theme, exports an excel sheet
#the sheet displays the number of unanswered questions by FOA
library(rio)
library(dplyr)
library(tidyr)
library(janitor)
library(openxlsx)

##read raw data to have the attributes
#read file, it was given by Eloisa via email
sav_file <- "data/7.NLO/1.raw/IAEA TC PROGRAMME ACHIEVEMENTS IN THE 21ST CENTURY - PART II.sav"
raw_data <- haven::read_sav(sav_file) 

#h <- import('data/7.NLO/2.raw_formatted/Part_2_h.rds') %>% filter(!is.na(foa_2))




#lookup file with labels of questions ------------------------------------------
#using it to get the names of the questions
all_questions <- attributes(raw_data[count_answers$question])$names
lkp_var_names <- lapply(names(raw_data), function(x){
  
  tibble(question = x,
         label =  attributes(raw_data[[x]])$label)
  
  
}) %>% do.call(rbind,.)


export(lkp_var_names, 'data/9.lookups/lkp_question_names_part_II.rds')
