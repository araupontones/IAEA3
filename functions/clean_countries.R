#funcition to clean country names =================================================
clean_countries <- function(.data){
  
  .data %>%
    mutate(country = trimws(country),
           country = case_when(
             country %in% c("Bolivia, Plurinational State o") ~ "Bolivia",
             country %in% c('The Bahamas') ~ "Bahamas",  
             country %in% c("Brunei") ~ "Brunei Darussalam" ,
             country %in% c("Côte d'Ivoire", 'Ivory Coast') ~ "Cote d'Ivoire",
             country %in% c('Iran') ~ "Iran, Islamic Republic of",
             country %in% c('South Korea') ~ "Korea, Republic of" ,
             country %in% c('Eswatini', 'Swaziland') ~ 'Kingdom of Eswatini',
             country %in% c("Lao People's Democratic Republ",
                            "Laos") ~ 'Lao P.D.R.',
             country %in% c('Macedonia') ~  "North Macedonia" ,
             country %in% c("Moldova") ~ "Republic of Moldova",
             country %in% c('Russia') ~ "Russian Federation", 
             country %in% c('Saint Vincent and the Grenadin',
                            'Saint Vincent and the Grenadines') ~ 'Saint Vincent & the Grenadines',
             country %in% c('Syria') ~ "Syrian Arab Republic" ,
             country %in% c('Republic of Serbia') ~ "Serbia",
             country %in% c('Venezuela, Bolivarian Republic') ~ "Venezuela",
             country %in% c('Democratic Republic of the Con',
                            "Democratic Republic of the Congo") ~ 'Democratic Rep. of the Congo',
             country %in% c('Congo', 'Republic of Congo') ~ 'Congo, Rep. of',
             country %in% c("Turkey") ~   "Türkiye" ,
             country %in% c('Vietnam') ~  "Viet Nam"  ,
             country %in% c('T.T.U.T.J of T. Palestinian A.') ~ "State of Palestine",
             T ~ country)
    )
}
