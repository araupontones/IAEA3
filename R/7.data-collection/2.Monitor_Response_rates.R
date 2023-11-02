#QA response rates by countries

library(dplyr)
library(tidyr)
library(rio)
library(janitor)
library(susor)
library(openxlsx)
library(scales)
library(glue)
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
            Bounced = sum(interview__status == "Bounced"),
            Accessed = sum(interview__status %in% c("Submitted", "Responding")),
            Submitted = sum(interview__status == "Submitted"),
            Avg_questions_unanswered = round(mean(n_questions_unanswered, na.rm = T),1),
            response_rate = Submitted / Sampled
            
  ) 
  


#Get row with totals totals --------------------------------------------------------------------
TOTALS <- tibble(
  
  Country_name = "TOTAL",
  Sampled = nrow(status),
  Bounced = sum(by_country$Bounced),
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
                      exfile = glue::glue("data/6.data-collection/2.response_rates/responses_by_country_{Sys.Date()}.xlsx")
)

total_countries = length(unique(by_country$Country_name))
havent_submitted =sum(by_country$Submitted ==0)
perc_havent = scales::percent(havent_submitted/total_countries)

sample_n = nrow(sample)
open = 4168
open_perc =scales::percent(open/sample_n)
accessed = comp = by_country_totals$Accessed[by_country_totals$Country_name == 'TOTAL']
acc_pers = percent(accessed/sample_n,accuracy = .01)
comp = by_country_totals$Submitted[by_country_totals$Country_name == 'TOTAL']
comp_pers = percent(comp/sample_n, accuracy = .01)

knitr::combine_words(sort(by_country$Country_name[by_country$Submitted==0]), sep = ", ")

avg_unanswered = round(mean(status$n_questions_unanswered[status$interview__status == "Submitted"], na.rm = T),1)
max_unanswered = round(max(status$n_questions_unanswered[status$interview__status == "Submitted"], na.rm = T),1)

glue::glue('from the {total_countries} countries in the sample, {havent_submitted} ({perc_havent}) have not submitted any interview yet.')
glue::glue('{open} ({open_perc}) have opened the invitation email')
glue('{accessed} ({acc_pers}) have accessed')
glue('{comp} ({comp_pers}) have submitted')
glue('The submitted interview with the maximum number of unanswered question, has {max_unanswered} unanswered')
glue("The average submitted interview has a total of {avg_unanswered} question")
