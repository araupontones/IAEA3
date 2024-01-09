#Structure data to be analyzed by likert

library(dplyr)

#'@param group_1 The grouping by which the count is to be conducted
#'@param group_2 The grouping by which the graph will be displayed.

by_likert <- function(.data,
                      group_1,
                      group_2){
  
  
  
  .data %>%
    group_by_at(group_1) %>%
    summarise(total_cat = n()) %>%
    ungroup() %>%
    group_by_at(group_2) %>%
    mutate(total_answers= sum(total_cat),
           perc = total_cat/total_answers,
           check = sum(perc),
           significant = ifelse(likert == "Significant", perc, 0)) %>%
    ungroup()
    
}

