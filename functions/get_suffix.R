# get the sufix of the theme



get_suffix <- function(tema, raw_data){
  
  
  theme <-attributes(raw_data[[tema]])$levels
  id_theme <- as.numeric(str_remove(tema,'theme____'))
  
  sufix = case_when(id_theme == 1 ~ 'fa',
                    id_theme == 2 ~ 'h',
                    id_theme == 3 ~ 'e',
                    id_theme == 4 ~ 'i',
                    id_theme == 5 ~ 'w',
                    id_theme == 6 ~ 'n'
                    
                    
  )
  
  return(sufix)
  
}