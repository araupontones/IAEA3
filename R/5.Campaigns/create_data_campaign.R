library(dplyr)
library(janitor)
s <- rio::import('data/2.sample/cps_sample.csv')
#before running, download the interview links.
links <- read.delim('data/3.Assignments/interviews.tab')





clean_links <- links %>%
  select(url = assignment__link,
         cp_id = cp_hidden
  ) %>%
  left_join(select(s, Name, Country_name, email, region, director, cp_id)) %>%
  relocate(Name, email,Country_name, region, director) %>%
  select(-cp_id) %>%
  rename(Country = Country_name)
#%>%
 # rename_all(function(x)paste("cpt_", x))
  
  
  
#campaigns
  
  export(clean_links, 'data/4.campaigns/cps.csv')


