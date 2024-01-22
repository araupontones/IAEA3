#Get percentage of answers by category

library(dplyr)

#'@param group_1 The grouping by which the count is to be conducted,
#'Always set the last element as the one for which the percentage is to be estiated
#'@param perc_of The varoable for which the percentage is to be estimated

group_perc <- function(.data,
                      group_1,
                      perc_of,
                      sort_cat = c("Regional reference","Master stage")){
  
  
  
  .data %>%
    group_by_at(c(group_1, perc_of)) %>%
    summarise(total_cat = n()) %>%
    ungroup() %>%
    rename(categories = perc_of) %>%
    #the last element in the group vector is removed so the percentage can be estimated
    group_by_at(group_1) %>%
    mutate(total_answers= sum(total_cat),
           perc = total_cat/total_answers,
           check = sum(perc),
           sort = ifelse(categories %in% sort_cat, perc, 0),
           sort = sum(sort)
           
           
           ) %>%
    ungroup() 
  
  
}



group_relevance <- function(.data,
                            grupo1 = c('region','theme')
                            
                            ){
  
  
  .data %>%
    group_by_at(c('id',grupo1)) %>%
    summarise(regional_master = max(regional_master),
              initial_established = max(initial_established)) %>%
    group_by_at(grupo1) %>%
    summarise(regional_master = mean(regional_master),
              initial_established = mean(initial_established)) %>%
   pivot_longer(-c(grupo1)) %>%
   mutate(sort = ifelse(name == "regional_master", value, 0))
  
  
}
