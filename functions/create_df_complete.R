create_df_complete <- function(.data){
  .data %>%
    filter(!is.na(EMail),
           !is.na(Institution), 
           Institution != "",
           Nationality != "NULL") 
  
}
