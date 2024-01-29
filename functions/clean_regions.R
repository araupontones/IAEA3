clean_regions <- function(.data){
  
  .data %>%
    mutate(region = case_when(country == 'Regional Africa' ~ "Africa",
                              country == "Gambia" ~ "Africa",
                              country == 'Regional Asia & the Pacific' ~ "Asia and the Pacific",
                              country == 'Regional Europe' ~ "Europe",
                              country == "Regional Latin America" ~ " Latin America and the Caribbean",
                              country == "Interregional" ~ "Interregional",
                              T ~ region
    ))
}
