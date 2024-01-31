#Check which countries resonded to which themes of the surves

#Function to keep only themes in data
keep_areas <- function(.data, survey, min_answers = 15){
  .data %>%
    rowwise() %>%
    mutate(answered = sum(!is.na(across(starts_with('q'))))) %>%
    filter(answered > min_answers) %>%
    #mutate_all(as.character) 
    rename(country = q0002) %>%
    mutate(country = susor_get_stata_labels(country)) %>%
    select(country, starts_with('q0004')) %>%
    mutate(across(starts_with('q'), function(x)susor_get_stata_labels(x))) %>%
    pivot_longer(-country,
                 values_to = 'theme') %>%
    filter(!is.na(theme)) %>%
    select(-name) %>%
    mutate(survey = survey)
}





countries_by_theme <- function(min_answers= 15){

sav_file <- "data/7.NLO/1.raw/IAEA TC PROGRAMME ACHIEVEMENTS IN THE 21ST CENTURY - PART I.sav"
sav_file2 <- "data/7.NLO/1.raw/IAEA TC PROGRAMME ACHIEVEMENTS IN THE 21ST CENTURY - PART II.sav"
raw_data <- read_sav(sav_file)
raw_data2 <- read_sav(sav_file2)
raw_cps <- import('data/6.data-collection/4.clean/cps.rds')

countries_in_region <- countries_region()




#append all surveys ===========================================================
nlo1 <- raw_data %>% keep_areas('nlo1')
nlo2 <- raw_data2 %>% keep_areas('nlo2')
cp <- raw_cps %>% select(country, theme) %>%
  mutate(theme = susor_get_stata_labels(theme),
         survey = 'cp')


all <- do.call(rbind, list(nlo1, nlo2, cp)) %>%
  mutate(theme = str_to_sentence(theme),
         id_theme = ids_themes(theme),
         theme = names_themes_2(id_theme)) %>%
  clean_countries()


return(all)

}
