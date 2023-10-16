#QA response rates by countries

library(dplyr)
library(tidyr)
library(rio)
library(janitor)
library(susor)
library(openxlsx)
gmdacr::load_functions('functions')

#define paths =================================================================
raw_dir <- 'data/6.data-collection/1.raw/cp'
raw_data <- import(file.path(raw_dir, 'cp.rds'))

# gStatus ======================================================================

status <- raw_data %>%
  relocate(Country_name, Name, email, interview__status, n_questions_unanswered)



#summary by country

by_country <- status %>%
  group_by(Country_name) %>%
  summarise(Sampled = n(),
            Accessed = sum(interview__status %in% c("Submitted", "Responding")),
            Submitted = sum(interview__status == "Submitted"),
            Avg_questions_unanswered = round(mean(n_questions_unanswered, na.rm = T),1),
            response_rate = Submitted / Sampled
            
  ) 
  


#Get row with totals totals --------------------------------------------------------------------
TOTALS <- tibble(
  
  Country_name = "TOTAL",
  Sampled = nrow(status),
  Accessed = sum(by_country$Accessed),
  Submitted = sum(by_country$Submitted),
  Avg_questions_unanswered = round(mean(by_country$Avg_questions_unanswered, na.rm = T),1),
  response_rate = Submitted/Sampled
  
)




#Define response rate
by_country_totals <- rbind(by_country, TOTALS) %>%
  mutate(response_rate = scales::percent(response_rate, accuracy = .1),
         Avg_questions_unanswered = ifelse(is.na(Avg_questions_unanswered), NA_real_, Avg_questions_unanswered))



# export summary by country
crate_xlsx_by_country(db = by_country_totals,
        
                      exfile = "data/6.data-collection/2.response_rates/responses_by_country.xlsx")


