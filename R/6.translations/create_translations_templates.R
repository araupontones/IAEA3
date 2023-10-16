library(dplyr)
source("functions/utils_translation.R")
#add heap size to interact with google sheets
#options(java.parameters = "-Xmx6000m")




#Define parameters ------------------------------------------------------------
dir_translations <- "data/5.translations"
dir_designer <- file.path(dir_translations,"1.Download_from_designer")
dir_gsheets <- file.path(dir_translations,"2.Translations_Gsheets")
dir_import_back <- file.path(dir_translations,"3.To_import_in_Designer")




#look_up questionnaires details ----------------------------------------------
# this file has all the relevant information to download templates
# from survey solutions, and from Gsheets
# it is divided in two tabs (questionnaires: info of questionnaires,
# countries: info of url of gsheet for each country)
# The id of the translation template is manually copied from the designer

lookUp_qns <- rio::import(file.path(dir_translations, "lookUp_translations.xlsx"),
                          sheet = "questionnaires")

lookUp_countries <- rio::import(file.path(dir_translations, "lookUp_translations.xlsx"),
                                sheet = "countries") 

questionnaires <- lookUp_qns$questionnaire
#countries <- lookUp_countries$country[-which(is.na(lookUp_countries$country))]
countries <- lookUp_countries$country

#countries <- "Russian"
countries
#Download template from designer ---------------------------------------------
downloaded_templates <- download_templates(
  x = "CP",
  exdir = dir_designer,
                   lookUp = lookUp_qns)





translations_gsheets <- lapply(countries, 
                               download_gsheets, 
                               lookUp = lookUp_countries,
                               questionnaires = questionnaires)

names(translations_gsheets) <- countries




#Update translations by joining Gsheets with templates =====================
# By doing this we do 2 things:
#1: update items to be translated in Gsheets (update if there are modifications
#in questionnaire)
#2: update templates that are imported back into Survey Solutions
# The output is first saved in dir_gsheets, then it is exported to Gsheets
# And then it is joined to the templates and saved in dir_import_back



join_templates_ss <- lapply(questionnaires,
                            join_templates_gsheet,
                            lookUP = lookUp_countries,
                            dir_designer = dir_designer,
                            dir_gsheets = dir_gsheets,
                            dir_import_back  = dir_import_back,
                            translations_gsheets = translations_gsheets,
                            countries = countries)

