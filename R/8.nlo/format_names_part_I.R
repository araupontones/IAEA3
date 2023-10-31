library("haven")
library("gmdacr")

#Using "questionnaires/NLO/Part I.PDF" as a reference

sav_file <- "data/7.NLO/IAEA TC PROGRAMME ACHIEVEMENTS IN THE 21ST CENTURY - PART I.sav"

raw_data <- read_sav(sav_file)


attributes(raw_data$q0004_0001)
attributes(raw_data$q0004_0001)



#Format single select ==========================================================

#define names of the colums
col_names_single <- c(q0001 = "role",
                      q0002 = "country"
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
col_names_multiple <- c("q0004" = "theme__")
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

format_sections <- function(.data, section, prefix){
  
  
  
  
  .data %>%
    rename_at(vars(starts_with(section)), function(x){
      #use the label as the name of the variable
      sapply(x, function(x){
        
        
        label = attributes(raw_data[[x]])$label
        no_commas = str_remove_all(label, ",|\\.")
        snake = str_replace_all(no_commas, '""|-', "_")
        #add prefix to identify section
        new_name <- paste(prefix, snake, sep = "_")
        
        if(prefix == "regional_coop"){
          
          new_name = paste(new_name, names(attributes(raw_data[[x]])$labels), sep = "___")
          
        }
        
        new_name
        
      })
      
      
    })
}


raw_sections <- raw_multiple %>%
  #FOOD AND AGRICULTURE
  format_sections("q0005", "relevance") %>%
  format_sections("q0006", "effectiveness") %>%
  format_sections('q0007', "sustainability") %>%
  format_sections('q0008', "regional_coop") %>%
  format_sections('q0009', "future") %>%
  format_sections('q0010', "visibility") %>%
  
  #HEALTH AND NUTRITION
  format_sections("q0011", "relevance") %>%
  format_sections("q0012", "effectiveness") %>%
  format_sections('q0013', "sustainability") %>%
  format_sections('q0014', "regional_coop") %>%
  format_sections('q0015', "future") %>%
  format_sections('q0016', "visibility") %>%
  
  mutate(across(c(
    
    starts_with('relevance'),
    starts_with('sustainability'),
    starts_with('effectiveness'),
    starts_with('future'),
    starts_with('visibility'))
    ,function(x)susor_get_stata_labels(x))) 
#%>%
 # select(starts_with('visibility'))



names(raw_sections)

check <- names(raw_sections)[str_detect(names(raw_sections), "q0011")]

sapply(check, function(x){
  
  print(x)
  print(attributes(raw_sections[[x]]))
  message(attributes(raw_sections[[x]])$label)
})

attributes(raw_sections[['q0008_0004_0008']])$label
