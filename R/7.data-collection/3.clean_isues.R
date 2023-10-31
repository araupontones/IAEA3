
#correct sample based on feedback from CPS
library(rio)
library(dplyr)
library(tidyr)
library(glue)
library(openxlsx)
# Load original sample ===========================================================
original_sample <- import('data/2.sample/cps_sample.csv', encoding = 'UTF-8')

#lkp_countries
lkp_countries <- original_sample %>%
  select(Country_name, country_id, region) %>%
  distinct()


#to check that corrected countries exist 
countries <- sort(unique(original_sample$Country_name))

#Load issues ===================================================================
issues <- import('data/6.data-collection/3.issues/follow-up survey-10.17.xlsx')


#names(issues)

#clean issues ----------------------------------------------------------------

issues_clean <- issues %>%
  rename(email =`Email Address`) %>%
  #clean email
  mutate(
    email = tolower(email),
    email = case_when(email == 'maria.zalazar@ib.edu.ar' ~ 'mflorzalazar@gmail.com',
                      T ~ email)) %>%
  #get the id from the sample
  left_join(select(original_sample, cp_id, email, Country_name)) %>%
  group_by(email) %>%
  mutate(count = n()) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(Date = as.Date(Date),
         Date = format(Date, "%d %B %Y"))




corrected_sample <- original_sample %>%
  mutate(Country_name = case_when(
    #17/10/2023
    cp_id == 5014 ~ "Bahamas",
    cp_id == 6183 ~ "Chile",
    cp_id == 158049 ~ "Paraguay",
    cp_id == 6186 ~ "Uruguay",
    cp_id == 76072 ~ "United Arab Emirates",
    cp_id == 153104 ~ "Latvia",
    #23/10/23
    cp_id == 155010 ~ "South Africa",
    cp_id == 40085 ~ "Paraguay",
    #25/10/2023
    cp_id == 68023 ~ "Papua New Guinea",
    
    #30/10/2023
    cp_id == 158095 ~ "Argentina",
    #31/10/2023 
    cp_id == 139001 ~ "Colombia",
    cp_id == 49097 ~ "Qatar",
    cp_id == 56003 ~ "Qatar",
    
    T ~ Country_name),
    
    
    Name = case_when(
    #17/10/2023  
      cp_id == 10003 ~ 'KHALILOV, Zaur',
    #23/10/2023
      email =='pcereal@lamolina.edu.pe' ~ 'JIMENEZ DAVALOS, Jorge Eduardo',
    
    #30/10
  cp_id == 119099 ~ "GALINGANA, Jake John",
  
  #31/10
  cp_id == 72010 ~ 'TAL, Yoram',
     
      T ~ Name) ,
    
    email = case_when(
      #23/10
     
      email == 'pcereal@lamolina.edu.pe' ~"jjimenezd@lamolina.edu.pe" ,
      cp_id == 119099 ~ 'jjgalingana@gmail.com',
      #31/10
      cp_id == 72010 ~ 'yoram.tal@moh.gov.il',
                      T ~ email
                      )

    #---------------------------------------------------------------------------------------------
   
    
    ) %>%
  #correct coutry_id
  left_join(lkp_countries, by = c('Country_name'), suffix = c("", "_new")) %>%
  mutate(country_id = ifelse(country_id != country_id_new, country_id_new, country_id),
         region = ifelse(region != region_new, region_new, region)
         ) %>%
  #join with issues 
  left_join(select(issues_clean, cp_id, Issue, assign_new, solved)) %>%
  mutate(assign = assign_new & is.na(solved)) %>%
  #remove cases from sample as asked by CPs
  filter(!cp_id %in% c(107006,
                       118068,
                       76030)) %>%
  relocate(assign_new)





#Check that country corrected is OK
sum(is.na(corrected_sample$country_id))
sum(is.na(corrected_sample$region))
sum(corrected_sample$assign, na.rm = T)

export(corrected_sample, 'data/2.sample/cps_sample_corrected.csv')

#Create assignments -----------------------------------------------------------

assignments <- corrected_sample %>%
  filter(assign) %>%
select(country = country_id,
       counterpart = cp_id) %>%
  mutate(`_quantity` = -1,
         `_responsible` = "andres_int",
         `_webmode` = 1,
         cp_hidden = counterpart
  )


#export(assignments,glue::glue('questionnaires/assignments/cps_{Sys.Date()}.txt'))

#Create data for campaign ======================================================

date_folder <- '311023'
links <- read.delim(glue('data/3.Assignments/{date_folder}/interviews.tab'), encoding = "UTF-8") %>%
  #only keep those cases that havent been solved yet
  left_join(select(assignments, cp_hidden, counterpart)) %>%
  filter(!is.na(counterpart)) %>%
  group_by(counterpart) %>%
  filter(assignment__id == max(assignment__id)) %>%
  select(-counterpart) %>%
  ungroup()



clean_links <- links %>%
  select(url = assignment__link,
         cp_id = cp_hidden
  ) %>%
  left_join(select(corrected_sample, Name, Country_name, email, region, director, cp_id)) %>%
  relocate(Name, email,Country_name, region, director) %>%
  select(-cp_id) %>%
  rename(Country = Country_name)


#export(clean_links, glue::glue('data/4.campaigns/cps_solved_{Sys.Date()}.csv'))






#create data campaigns ========================================================
#to send a new email to the solved cases

solved_cases <- issues_clean %>%
  select(-c(cp_id, Country_name)) %>%
  arrange(Date)


olive <- '#E2EFDA'
rows = nrow(solved_cases) + 1
wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "Issues")
writeData(wb, "Issues", solved_cases, rowNames = F, colNames = T)

style_header <- createStyle(fontName = 'Calibri',
                            fontSize = 11,
                            textDecoration = 'bold',
                            border = "TopBottomLeftRight",
                            borderColour = 'black')

addStyle(wb, "Issues", style_header,rows = 1, cols = 1:ncol(solved_cases))

style_body <- createStyle(fontName = 'Calibri',
                            fontSize = 11,
                            border = "TopBottomLeftRight",
                            borderColour = 'black')

addStyle(wb, "Issues", style_body,rows = 2:rows, cols = 1:ncol(solved_cases), gridExpand = T)

conf_style  <- createStyle(fontName = 'Calibri',
                                     fontSize = 11,
                                     border = "TopBottomLeftRight",
                                     borderColour = 'black',
                           bgFill = olive)

conditionalFormatting(wb, "Issues",
                      cols = 1:ncol(solved_cases),
                      rows = 2:rows,
                      rule = "$L2==TRUE",
                      style = conf_style
)


setColWidths(wb, 'Issues', cols = c(1:6, 10:13), widths = 20)
setColWidths(wb, 'Issues', cols = c(7:9), widths = 78)

openxlsx::saveWorkbook(wb, 'data/6.data-collection/3.issues/solved-issues.xlsx', overwrite = TRUE)

