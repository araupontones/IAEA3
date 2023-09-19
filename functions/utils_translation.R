
library(httr)
library(xlsx)

# function to create download url ==========================================
#' @param questoinnare id of the questionnaire in SS designer
#' @return url to download the translation template of the questionnaire
template_url <- function(questionnaire){
  
  glue::glue('https://designer.mysurvey.solutions/translations/{questionnaire}/template')
  
}




#' Function to download templates =======================================
#' @param questoinnaire Name of the questionnaire as it is stored in 
#' lookUp_translations.xlsx
#' @return downloads translation template in data/1.CAPI/translations

download_templates <- function(x, exdir, lookUp){
  
  #ids and names
  id <- lookUp$id_designer[lookUp$questionnaire == x]
  
  #name <- names(questionnaires)[questionnaires == x]
  exfile <- glue::glue('{exdir}/template_{x}.xlsx')
  
  message(glue::glue("Downloading template {x} from Survey Solutions Designer"))
  
 
  #download and save in exdir
  r <- httr::GET(template_url(id),
                 authenticate(user= 'andres.arau@outlook.com', password = "Seguridad1"),
                 write_disk(exfile, overwrite = T)
  )
  # write_disk(exfile, overwrite = T))
  
  
  if(r$status_code == 200){
    
    cli::cli_alert_success(glue::glue("{x} template has been downloaded"))
    
  }
  
}


#download Gsheets=======================================================
#' @param country Country to download the template from
#' @param lookUp look up tab of lookUp_translations.xlsx
#' @param questionnaire names of the questionaires in ookUp_translations.xlsx
#' @return  a list by Country with the translations from google sheet


download_gsheets <- function(country,
                             lookUp,
                             questionnaires){
  
  #confirm that the country is the correct one
  message(country)
  #get ss url of the country
  ss <- lookUp$ss[lookUp$country == country][1]
  
  
  #read each questionnaire
  translations <- lapply(questionnaires, function(q){
    if(!is.na(ss)){
      
      googlesheets4::read_sheet(ss = ss, sheet = q)
      
    }
    
    
  })
  
  names(translations) <- questionnaires
  
  return(translations)
  
  
}


#append all tabs of template into a single file
#' this is done to translate specified categories (the @@@@_ in the template)
#' Thus, this function reads the template and append all of its tabs into one,
#' then creates a variable call tab to identify the tab that they belong
#' @param path_to_file path to template file

append_template_tabs <- function(path_to_file){
  
  #load workbook to get the sheet names
  #temp <- xlsx::loadWorkbook(path_to_file)
  
  #sheets <- names(getSheets(temp))
  #readxl::ex
  sheets <-readxl::excel_sheets(path_to_file)
  #read data for each tab, read, and create a tab column
  append_template <- lapply(sheets, function(x){
    
    #print(x)
    tab <- rio::import(path_to_file, sheet = x)
    tab$tab = x
    
    return(tab)
    
  })
  
  #append files into a single one
  appended <- do.call(rbind, append_template)
  
  return(appended)
  
}


#export template back =========================================================
#' takes the joint data (translations and templtes) and exports the template in
#' SS format
#' @param data_back long data with translations and template joint
#' @param exfile file to export
#' 
export_template_back <- function(data_back, exfile){
  
  sort(unique(data_back$tab))
  split_tabs <- lapply(split(data_back, data_back$tab), function(x,
                                                                 tabs = sort(unique(data_back$tab))){
    #fetch name of the tab
    current <-x$tab[1]
    #get the order of the split
    order <- which(tabs == current)
    
    #remove tab column
    
    
    y <- x %>% select(-tab) 
    
    
    
    if(order == 1){
      
      write.xlsx(y,
                 file=exfile,
                 sheetName=current,
                 append = F,
                 row.names = F)
    } else if(current !=1) {
      print(current)
     
      if(!current %in% c('@@@_cps', '@@@_countries', '@@@_years'))
      write.xlsx(y,
                 file=exfile,
                 sheetName=current,
                 append = T,
                 row.names = F)

      
      
    }
    
    #x
  })
  
  
}


#Update translations un gsheets ==============================================

jgc <- function()
{
  .jcall("java/lang/System", method = "gc")
}    

#join translations with templates =============================================
#' @param lookUP = ookUp_translations.xlsx[lookUp_countries],
#' @param dir_designer  dir_designer,
#' @param dir_gsheets = dir_gsheets,
#' @param translations_gsheets = translations_gsheets,
#' @paramcountries = countries

join_templates_gsheet <- function(qn,
                                  lookUP,
                                  dir_designer,
                                  dir_gsheets,
                                  dir_import_back,
                                  translations_gsheets,
                                  countries
){
  
  
  message(qn)
  
  #read SS template of this questionnaire 
  template_path <- glue::glue('{dir_designer}/template_{qn}.xlsx')
  
  # #append all tabs of template into a single one
  template <- append_template_tabs(template_path)
  
  
  
  
  # #remove cascading that are ids (redundant for translation)
  ids <- template %>%
    dplyr::filter(tab %in% c('@@@_cps', '@@@_countries', '@@@_years'))
  
  template_not_ids <- template %>%
    dplyr::filter(!tab %in% c('@@@_cps', '@@@_countries', '@@@_years'))
  
  
  # #create unique text so redundant translations arenot translated twice
  unique_text <- template_not_ids %>%
    group_by(`Original text`) %>%
    mutate(text_id = dplyr::cur_group_id(),
           order = row_number()) %>%
    ungroup() %>%
    dplyr::filter(order == 1)
  
  
  
  
  
  
  # #Update translations in google sheets
  
  
  
  for(c in countries){
    cli::cli_alert_success(c)
    
    #get ss of gsheet
    ss<- lookUP$ss[lookUP$country == c][1]
    
    
    
    #define exit dir
    exdir <- file.path(dir_gsheets, c)
    exfile <- glue::glue('{exdir}/{qn}.rds')
    
    
    if(!dir.exists(exdir)){
      
      dir.create(exdir)
    }
    
    
    #translation  made in gsheetsof this questionnaire for this country
    gsheet <- translations_gsheets[[c]][[qn]] %>%
      mutate(`Original text` = as.character(`Original text`))
    
    #join with the template (the template exported by SS)
    template_ghseet <- unique_text %>%
      select(`Original text`) %>%
      left_join(gsheet, by = "Original text")
    
    
    
    #save locally
    rio::export(template_ghseet, exfile)
    
    
    
    #write into ggshets
    #get garbage collection
    #gc()
    #jgc()
    googlesheets4::write_sheet(template_ghseet, ss = ss, sheet = qn)
    
    #Sys.sleep(5)
    
    #save template to import back into survey solutions
    language <- lookUP$language[lookUP$country == c][1]
    exdir_back <- file.path(dir_import_back, language)
    exfile_back <- glue::glue('{exdir_back}/{c}.xlsx')
    
    if(!dir.exists(exdir_back)){
      
      dir.create(exdir_back)
    }
    
    
    data_back <- template %>%
      select(-Translation) %>%
      left_join(template_ghseet, by = "Original text",
                na.) %>%
      rbind(ids) %>%
      mutate_all(function(x){ifelse(is.na(x), "", x)}) %>%
      mutate(Translation = ifelse(Translation == "NULL", "", Translation))
    
    
    
    
    
    #save template
    #return(data_back)
    save_data <- export_template_back(data_back, exfile = exfile_back)
    
  } 
  
  
  
  
  
  # 
  # 
  # 
  
  
  
  
  
  
  
  
}




