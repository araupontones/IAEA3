create_df_complete <- function(.data, drop_na_email = T){
  
  
  if(drop_na_email){
    
    d1 <- .data %>%
      filter(!is.na(EMail),
             !is.na(Institution), 
             Institution != "",
             Nationality != "NULL") 
    
  } else {
    
    d1 <- .data
    
  }
 
  
  
  
  
}
