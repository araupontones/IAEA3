
#For each theme, exports an excel sheet
#the sheet displays the number of unanswered questions by FOA
library(rio)
library(dplyr)
library(tidyr)
library(janitor)
library(openxlsx)
library(stringr)
gmdacr::load_functions('functions')
##read raw data to have the attributes
#read file, it was given by Eloisa via email
sav_file <- "data/7.NLO/1.raw/IAEA TC PROGRAMME ACHIEVEMENTS IN THE 21ST CENTURY - PART II.sav"
raw_data <- haven::read_sav(sav_file) 

#h <- import('data/7.NLO/2.raw_formatted/Part_2_h.rds') %>% filter(!is.na(foa_2))

#lookup of sections in the questionnaire, used to identify sections of each foa within each theme
lkp_sections <- import('data/9.lookups/lkp_clean_sections_nlo_part_II.xlsx', sheet = 'foas')


#lookup file with labels of questions ------------------------------------------
lkp_var_names <- import('data/9.lookups/lkp_question_names_part_II.rds')






#Read all files created in R/8.nlo/Part II 0.format....R
r_dir <- 'data/7.NLO/2.raw_formatted/'
all_files <- list.files(r_dir, pattern = "Part_2")



#format data so each foa has only its enabled sections -------------------------
split_foas <- lapply(all_files, function(raw_section){
  
  
  #read file of this section
  raw <- import(file.path(r_dir, raw_section))
  
  #get id of section
  id_section <- str_remove_all(raw_section, "Part_2_|\\.rds")
  
 
  
  #get all varables that start with foa_
  foas <- str_detect(names(raw), "foa_")
  vars_foas <- names(raw)[foas]
  
  #create data for each foa and its enabled sections:
  data_foa <- lapply(vars_foas, function(f){
    
    foa_id <- str_remove(f,"foa_")
    
    #get attributes from the lookup table
    starts <- lkp_sections$starts[lkp_sections$theme == id_section & lkp_sections$foa_id == foa_id]
    ends <- lkp_sections$ends[lkp_sections$theme == id_section & lkp_sections$foa_id == foa_id]
    
    #define the questions enabled for each FOA
    questions <- questions_of_section(starts, ends)
   
    #Format data for this FOA
    f <- raw %>%
      rename(this = f) %>%
      select( RespondentID,
              theme,
              foa = this,
             starts_with(questions)) %>%
      #filter rows that do not belong to this FOA
      filter(!is.na(foa)) %>%
      mutate_all(as.character) %>%
      #Pivot longer so all the foas can be appended 
      pivot_longer(-c(RespondentID, theme, foa),
                   names_to = 'question')
    
    
   
    
  }) %>% do.call(rbind,.) #close the loop of theme/foa
  
}) %>% do.call(rbind,.) #close the loop of theme



#count interviews by foa -------------------------------------------------------
count_interviews <- split_foas %>%
  group_by(theme, foa) %>%
  summarise(interviews = n_distinct(RespondentID), .groups = 'drop')



#count answers by foa and question ---------------------------------------------

count_answers <- split_foas %>%
  #Count the number of answeres for each queston
  group_by(theme, foa, question) %>%
  summarise(answered = sum(!value %in% c(NA_character_, "")), .groups = 'drop') %>%
  #get the names of the questions
  left_join(lkp_var_names) %>%
  #Filter others because their labels are not question specefici
  #all these labels are Other (especify)
  filter(!str_detect(question, "_other")) %>%
  #removing sufiix because the multiple select questions have the same label
  mutate(question = str_remove_all(question, '_[0-9]{1,}|_other'))%>%
  #because some multiple select are missing (i.e the box wasnt ticked), we are 
  #assuming that the section was answered if at least one question was answered
  group_by(theme, foa,label, question) %>%
  summarise(n_answered = max(answered), .groups = 'drop') %>%
  #get the number of interviews for which the section was enabled
  left_join(count_foas) %>%
  #estimate the percentage
  mutate(p_answered = n_answered/interviews,
         p_unanswered = 1-p_answered,
         across(starts_with('p'), function(x)scales::percent(x,1 ))
         ) %>%
  arrange('centre')
  

#create clean excel sheets that can be added to the outline report.
lapply(unique(count_answers$theme), function(x){
  
  message(x)
  d_th <- filter(count_answers, theme == x) %>%
    select(-theme) %>%
    #droping matrixes asked by year
    filter(!question %in% c("q0030", "q0033", "q0034", "q0036")) %>%
    arrange(question)
  cols <- ncol(d_th)
  rows <- nrow(d_th) +1
  
  wb <- createWorkbook()
  addWorksheet(wb, "sheet")
  writeData(wb, 'sheet', d_th, row.names = F, colNames = T)
  
  #styles
  style_header <- createStyle(fontName = 'Open Sans',
                              fontSize = 9,
                              fontColour = '#FFFFFF',
                              textDecoration = 'bold',
                              bgFill = 'black',
                              border = "TopBottomLeftRight",
                              borderColour = '#FFFFFF')
  
  addStyle(wb, "sheet", style = style_header, rows = 1, cols = 1:cols)
  
  style_normal <- createStyle(halign = "left",
                              valign = 'center',
                              fontName = 'Open Sans',
                              fontSize = 9,
                              fontColour = 'black',
                              border = "TopBottomLeftRight",
                              borderColour = 'black',
                              wrapText = T)
  
  setColWidths(wb, 'sheet', cols = c(1,2), widths = 35)
  
 
  
  addStyle(wb, "sheet", style = style_normal, rows = 2:rows, cols = 1:cols, gridExpand = T)
  exfile = glue('data/7.NLO/4.Check_unanswered_Part_II/{x}.xlsx')
  openxlsx::saveWorkbook(wb, exfile, overwrite = TRUE)
  
  
  
  
})
#export
