#funcition to clean country names =================================================
clean_countries <- function(.data){
  
  .data %>%
    mutate(country = trimws(country),
           country = case_when(
             country %in% c("Bolivia, Plurinational State o") ~ "Bolivia",
             country %in% c("Côte d'Ivoire") ~ "Cote d'Ivoire",
             country %in% c('Eswatini') ~ 'Kingdom of Eswatini',
             country %in% c("Lao People's Democratic Republ") ~ 'Lao P.D.R.',
             country %in% c('Saint Vincent and the Grenadin') ~ 'Saint Vincent & the Grenadines',
             country %in% c('Venezuela, Bolivarian Republic') ~ "Venezuela",
             country %in% c('Democratic Republic of the Con') ~ 'Democratic Rep. of the Congo',
             country %in% c('Congo') ~ 'Congo, Rep. of',
             country %in% c('T.T.U.T.J of T. Palestinian A.') ~ "State of Palestine",
             T ~ country)
    )
}
