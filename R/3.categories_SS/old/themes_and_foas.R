#Themes and foas

library(rio)
library(dplyr)
library(stringr)


foas_ref <- import('data/1.reference/Copy of FOA mapping - old to new.xlsx')



names(foas_ref)

unique(foas_ref$`Thematic Area then and now`)

#function to detect strings
detecta <- function(x, find){stringr::str_detect(x, find)}

#function to create catefories 

create_categories <- function(.data, title, value, parentval, ...){
  
  .data %>%
   select(title = {{title}},
          value ={{value}},
          parentval = {{parentval}}) 
  
  
  
}



#categories of themes ==========================================================
themes <- foas_ref %>%
  select(theme = `Thematic Area then and now`) %>%
  group_by(theme) %>%
  count() %>%
  ungroup() %>%
  mutate(value = case_when(detecta(theme, "Food") ~ 1,
                           detecta(theme, "Health") ~ 2,
                           detecta(theme, "Energy") ~ 3,
                           detecta(theme, "Industrial") ~ 4,
                           detecta(theme, "Water") ~ 5,
                           detecta(theme, "Safety") ~ 6,
                           detecta(theme, "Knowledge") ~ 7
                           )) %>%
  arrange(value)


cat_themes <- create_categories(themes, title = theme, value = value) %>%
  mutate(parentvalue = "",
         attachmentname = ""
         )

export(cat_themes,'questionnaires/categories/themes.xlsx', sheetName = "Categories", overwrite = T)

#categories foa ================================================================
foas <- foas_ref %>%
  select(foa =`FoA new` , 
         theme = `Thematic Area then and now`,
         code_iaea = new) %>%
  mutate(foa = str_trim(foa)) %>%
  group_by(foa) %>%
  slice(1) %>%
  left_join(select(themes, theme,parentvalue = value)) %>%
  arrange(parentvalue, foa) %>%
  group_by(theme) %>%
  mutate(value = paste0(parentvalue, row_number()),
         attachmentname = "") %>%
  ungroup() %>%
  select(title = foa,
         value,
         parentvalue,
         attachmentname,
         code_iaea,
          theme)
  

export(foas,'questionnaires/categories/foas_.xlsx', sheetName = "Categories", overwrite = T)




