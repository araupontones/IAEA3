library(rio)
library(ggplot2)
library(RColorBrewer)
library(extrafont)

#How would you define the degree to which the TCP has contributed in
#achieving results in each are of support in your country

relevance <- import('data/7.NLO/2.raw_formatted/Part_1_relevance.rds')

View(relevance)

tabyl(relevance, period,value)

clean_relevance <- filter(relevance, !is.na(value)) %>%
  relocate(id,theme,FOA_new, period, value) 
  


#maximium degree of contribution -----------------------------------------------
degree_max <- clean_relevance %>%
  group_by(id, country, region, FOA_new) %>%
  summarise(degree_contribution = max(value))
 


tabyl(degree_max, degree_contribution) %>% arrange(desc(n))

#Increase at least from one step -----------------------------------------------

degree_increase <- clean_relevance %>%
  group_by(id, country, region, FOA_new, theme) %>%
  arrange(id,FOA_new,theme, period) %>%
  #check that the stage in period is greater than the last one
  summarise(periods = n(),
            min = min(value),
            max = max(value)
            ) %>%
  #check if the stage improve 
  mutate(better = ifelse(min < max, T, F)) %>%
  relocate(better, .after = theme)  %>%
  #only estimate for those that have more than one period
  dplyr::filter(periods > 1) %>%
  mutate(t = min < max)


l
