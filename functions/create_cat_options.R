create_cat_options <- function(.data, 
                               starts_con = "Level ", 
                               names_a = "level",
                               new_var = level
){
  
  .data %>%
    #'keep only the code and the levels
    select(improvement_code,
           starts_with(starts_con)) %>%
    pivot_longer(-improvement_code,
                 names_to = names_a,
                 values_to = 'title') %>%
    filter(!is.na(title)) %>%
    #' assign a code to the general categories
    #' They start with 0 so No capacity is always the first option in the questionnaire
    mutate(improvement_code = ifelse(is.na(improvement_code), 0, improvement_code),
           {{new_var}} := stringr::str_remove({{new_var}},starts_con),
           value = paste0(improvement_code, {{new_var}})
    ) %>%
    arrange(value)
  
  
  
}
