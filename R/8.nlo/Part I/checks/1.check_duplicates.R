#Using "questionnaires/NLO/Part I.PDF" as a reference and guidance
library("haven")
library("gmdacr")
library(dplyr)
library(susor)
library(stringr)
library(glue)
library(rio)
library(stringi)
library(tidyr)
library(openxlsx)

#read file, it was given by Eloisa via email
sav_file <- "data/7.NLO/1.raw/IAEA TC PROGRAMME ACHIEVEMENTS IN THE 21ST CENTURY - PART I.sav"
raw_data <- read_sav(sav_file)



create_wide <- function(by){
  
  raw_data %>%
    #mutate_all(as.character) 
    rename(country = q0002,
           role = q0001) %>%
    mutate(country = susor_get_stata_labels(country),
           role = susor_get_stata_labels(role),
           name = glue("{toupper(stri_trans_general(q0003_0002, 'Latin-ASCII'))}, {str_to_sentence(q0003_0001)}")
    ) %>%
    relocate(country, name, role) %>%
    select(-starts_with("Collector"),
           -starts_with("Custom"),
           -starts_with('q0003'),
           -starts_with('p0')) %>%
    mutate_all(as.character) %>%
    mutate_all(function(x){ifelse(x == "", NA_character_, x)}) %>%
    rowwise() %>%
    mutate(answered = sum(!is.na(across(starts_with('q'))))) %>%
    arrange(country,name, role,StartDate) %>%
    group_by_at(by) %>%
    mutate(times_submitted = n(),
           int = row_number()) %>%
    relocate(country, name,role, answered, times_submitted,  int) %>%
    .[,1:8] %>%
    pivot_wider(id_cols = c(all_of(by), times_submitted),
                names_prefix = "answers_",
                names_from = int,
                values_from = answered)
  
  
}


wide_country <- create_wide(by = c("country"))
wide_name <- create_wide(by = c("country", "name", "role"))

dupes <- janitor::get_dupes(wide_name, name)

sum(wide_country$times_submitted > 5)

wide_name %>% 
  group_by(country) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

export_checks <- function(by){
  
  
  if(by == 'name'){
    
    
    wide = wide_name
    criteria = "$D2>1"
    exfile = 'NLOs'
  } else {
    wide = wide_country
    criteria = "$B2>1"
    exfile = 'countries'
  }
  
  
  
  rows <- nrow(wide) + 1
  cols <- ncol(wide)
  
  wb <- openxlsx::createWorkbook()
  addWorksheet(wb, 'duplicates')
  writeData(wb, 'duplicates', wide, rowNames = F, colNames = T)
  
  #header 
  #styles
  style_header <- createStyle(fontName = 'Open Sans',
                              fontSize = 9,
                              fontColour = '#FFFFFF',
                              textDecoration = 'bold',
                              bgFill = 'black',
                              border = "TopBottomLeftRight",
                              borderColour = '#FFFFFF')
  
  addStyle(wb, "duplicates", style = style_header, rows = 1, cols = 1:cols)
  
  
  #format normal 
  
  normal_style= createStyle(fontName = 'Open Sans',
                            fontSize = 8,
                            fontColour = 'black',
                            border = "TopBottomLeftRight",
                            borderColour = 'black'
  )
  
  addStyle(wb, 'duplicates', normal_style, cols = 1:cols, rows = 2:rows, gridExpand = T)
  
  
  #conditional formatting
  dup_style= createStyle(fontName = 'Open Sans',
                         fontSize = 8,
                         fontColour = 'black',
                         border = "TopBottomLeftRight",
                         borderColour = 'black',
                         bgFill = '#FDED65')
  
  conditionalFormatting(wb, "duplicates",
                        fontSize = 8,
                        cols = 1:cols,
                        rows = 2:rows,
                        rule = criteria,
                        style = dup_style
  )
  
  setColWidths(wb, 'duplicates', cols = 1, widths = 29)
  setColWidths(wb, 'duplicates', cols =2, widths = 40)
  
  freezePane(wb, "duplicates", firstActiveRow = 2) 
  openxlsx::saveWorkbook(wb, glue('data/7.NLO/5.checks/check_duplicates_{exfile}.xlsx'), overwrite = T)
  message('exported')
  
  
}

export_checks('country')
export_checks('name')

