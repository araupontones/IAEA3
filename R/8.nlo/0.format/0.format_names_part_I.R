library("haven")
library("gmdacr")

#Using "questionnaires/NLO/Part I.PDF" as a reference

sav_file <- "data/7.NLO/1.raw/IAEA TC PROGRAMME ACHIEVEMENTS IN THE 21ST CENTURY - PART I.sav"

raw_data <- read_sav(sav_file)
lkp_countries <- import("data/9.lookups/countries.rds" )


attributes(raw_data$q0004_0001)
attributes(raw_data$q0004_0001)



#Format single select ==========================================================

#define names of the colums
col_names_single <- c(q0001 = "role",
                      q0002 = "country",
                      q0045 = "promoted_participation_women",
                      q0046 = 'received_comments'
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



#Format multiple select =======================================================

attributes(raw_data$q0005_0001_0001)

#define name of multiple select variables
col_names_multiple <- c("q0004" = "theme__",
                        "q0042" = "sdg_contribution__",
                        "q0043" = "sdg_report__")
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


raw_multiple <- raw_singles %>% format_multiple_select(prefixes)


## format sections ===========================================================

format_sections <- function(.data, section, prefix, theme){
  
  
  
  
  .data %>%
    rename_at(vars(starts_with(section)), function(x){
      #use the label as the name of the variable
      sapply(x, function(x){
        
        
        label = attributes(raw_data[[x]])$label
        no_commas = str_remove_all(label, ",|\\.")
        snake = str_replace_all(no_commas, '""|-', "_")
        #add prefix to identify section
        new_name <- paste(prefix,theme, snake, sep = "_")
        
        if(prefix == "regionalCoop"){
          
          new_name = paste(new_name, names(attributes(raw_data[[x]])$labels), sep = "___")
          
        }
        
        new_name
        
      })
      
      
    })
}


raw_sections <- raw_multiple %>%
  #FOOD AND AGRICULTURE
  format_sections("q0005", "relevance", 'fa') %>%
  format_sections("q0006", "effectiveness", 'fa') %>%
  format_sections('q0007', "sustainability",'fa') %>%
  format_sections('q0008', "regionalCoop",'fa') %>%
  format_sections('q0009', "future",'fa') %>%
  format_sections('q0010', "visibility",'fa') %>%
  
  #HEALTH AND NUTRITION
  format_sections("q0011", "relevance", 'h') %>%
  format_sections("q0012", "effectiveness",'h') %>%
  format_sections('q0013', "sustainability",'h') %>%
  format_sections('q0014', "regionalCoop",'h') %>%
  format_sections('q0015', "future",'h') %>%
  format_sections('q0016', "visibility",'h') %>%
  
  #ENERGY
  format_sections("q0017", "relevance", 'e') %>%
  format_sections("q0018", "effectiveness",'e') %>%
  format_sections('q0019', "sustainability",'e') %>%
  format_sections('q0020', "regionalCoop",'e') %>%
  format_sections('q0021', "future",'e') %>%
  format_sections('q0022', "visibility",'e') %>%
  
  #INDUSTRIAL APPLICATIONS
  format_sections("q0023", "relevance", 'i') %>%
  format_sections("q0024", "effectiveness",'i') %>%
  format_sections('q0025', "sustainability",'i') %>%
  format_sections('q0026', "regionalCoop",'i') %>%
  format_sections('q0027', "future",'i') %>%
  format_sections('q0028', "visibility",'i') %>%
  
  #WATER
  format_sections("q0029", "relevance", 'w') %>%
  format_sections("q0030", "effectiveness",'w') %>%
  format_sections('q0031', "sustainability",'w') %>%
  format_sections('q0032', "regionalCoop",'w') %>%
  format_sections('q0033', "future",'w') %>%
  format_sections('q0034', "visibility",'w') %>%
  
  #NUCLEAR
  format_sections("q0035", "relevance", 'n') %>%
  format_sections("q0036", "effectiveness",'n') %>%
  format_sections('q0037', "sustainability",'n') %>%
  format_sections('q0038', "regionalCoop",'n') %>%
  format_sections('q0039', "future",'n') %>%
  format_sections('q0040', "visibility",'n') %>%
  
  #Partnerships
  format_sections('q0041', "partnerships",'') %>%
  #Women participation
  format_sections('q0044', "women",'') %>%
  #Priority
  format_sections('q0047', "priority_women",'') %>%
  #Measures
  format_sections('q0048', "measures_STEM",'') %>%
  
  
  #get labels of variables --------
  mutate(across(c(
    
    starts_with('relevance'),
    starts_with('sustainability'),
    starts_with('effectiveness'),
    starts_with('future'),
    starts_with('visibility'),
    starts_with('partnerships'),
    starts_with('priority_women'),
    starts_with('measures_STEM')
    
    
  )
  ,function(x)susor_get_stata_labels(x))
  ) %>%
  #drop indicators of pages (redundante)
  select(-starts_with('p00')) %>%
  #get region
  mutate(country = forcats::fct_recode(country,  "State of Palestine" ="T.T.U.T.J of T. Palestinian A." )) %>%
  left_join(lkp_countries) %>%
  mutate(region = ifelse(country == "Gambia","Africa", region ))
 




setdiff(unique(raw_sections$country), lkp_countries$country)


export(raw_sections,'data/7.NLO/1.raw/Part_1.rds')
# 
# check <- names(raw_sections)[str_detect(names(raw_sections), "q0042")]
# 
# sapply(check, function(x){
#   
#   print(x)
#   print(attributes(raw_sections[[x]]))
#   message(attributes(raw_sections[[x]])$label)
# })
# 
# attributes(raw_sections[['q0008_0004_0008']])$label
