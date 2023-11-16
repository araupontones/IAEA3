#Using "questionnaires/NLO/Part I.PDF" as a reference and guidance
library("haven")
library("gmdacr")
library(dplyr)
library(susor)
library(stringr)
library(glue)
library(rio)

list.files('data/7.NLO/1.raw/')
#read file, it was given by Eloisa via email
sav_file <- "data/7.NLO/1.raw/IAEA TC PROGRAMME ACHIEVEMENTS IN THE 21ST CENTURY - PART II.sav"
raw_data <- read_sav(sav_file)

#look up countries is created in R/2.sample, it is used to get the region
lkp_countries <- import("data/9.lookups/countries.rds" )


#loopup of sections in the questionnaire, used to split the questionnaires
lkp_sections <- import('data/9.lookups/lkp_clean_sections_nlo_part_II.xlsx', sheet = 'themes')


#play around with the function attributes to learn how to fetch the variable labels
attributes(raw_data$q0004_0001)
attributes(raw_data$q0004_0001)



#Format single select ==========================================================
#These are basic single select questions

#define names of the columns
col_names_single <- c(q0001 = "role",
                      q0002 = "country"
                      #q0045 = "promoted_participation_women",
                      #q0046 = 'received_comments'
)


#replace value for label and rename variables 

format_single_select <- function(.data, col_names){
  
  .data%>%
    #get the label of the variables
    mutate(across(names(col_names), function(x)susor_get_stata_labels(x))) %>%
    #change the name of the variables
    rename_at(vars(names(col_names)), function(x)col_names[x]) 
  
  
}




raw_singles <- raw_data %>% format_single_select(col_names_single)


# raw_singles %>%
#   group_by(country) %>%
#   summarise(interviews = n()) %>%
#   export(., "data/7.NLO/3.Country_checks/Part_II_interviews_countries.xlsx")



#Format multiple select =======================================================
#These are simple mulitple select. One question, multiple answers
attributes(raw_data$q0005_0001_0001)

#define name of multiple select variables
col_names_multiple <- c(#"q0003" = "name_",
                        "q0004" = "theme",
                        "q0005" = "foa_fa",
                        "q0026" = "foa_h",
                        "q0053" = "foa_e",
                        "q0058" = "foa_i",
                        "q0066" = "foa_w",
                        "q0074" = "foa_s"
                        #"q0043" = "sdg_report_"
                        )
prefixes <- names(col_names_multiple)

#replace value for label and rename variables

format_multiple_select <- function(.data, col_names){
  
  
  .data %>%
    #get the label of the variables
    mutate(across(starts_with(prefixes), function(x)susor_get_stata_labels(x))) %>%
    
    #rename using the definition of the names 
    rename_at(vars(starts_with(prefixes)), function(x){
      
      #get the prefix
      prefix <- str_extract(x, "^.*?(?=-|_)")
      prefix <- col_names_multiple[prefix]
      #get the sufix: the sequence number of the variable
      sufix <- as.numeric(str_extract(x, "[^_]*$"))
      #define new name
      glue('{prefix}__{sufix}')
      
    })
  
}








raw_multiple <- raw_singles %>% format_multiple_select(prefixes) %>%
  rename(First_name = q0003_0001,
         Last_name = q0003_0002) 
names(raw_multiple)


#using this from the lookup to ease the splitting of the data
#Split the data in themes and export each file separately
id_themes <- lkp_sections$theme_id

splited <- lapply(id_themes, function(id){
  message(id)
  #Get the attributes of each theme section
  #number where section starts and ends
  sufix <- lkp_sections$theme[lkp_sections$theme_id == id]
  starts <- lkp_sections$starts[lkp_sections$theme_id == id]
  ends <- lkp_sections$ends[lkp_sections$theme_id == id]
  
  
  
  questions <- questions_of_section(starts, ends)
  
  #split the data by theme and export
  data_section <- raw_multiple %>%
    select(RespondentID,
           country,
           theme = glue('theme__{id}'),
           starts_with(glue('foa_{sufix}')),
           starts_with(questions)
           ) %>%
    #remove prefix of foa so it is consistent
    rename_at(vars(starts_with("foa")), function(x)str_remove(x, glue("_{sufix}_"))) %>%
    filter(!is.na(theme))
  
  
  
  export(data_section, glue('data/7.NLO/2.raw_formatted/Part_2_{sufix}.rds')) 
 
 
  
  
 
  
 

  
})


