#keep data for section and theme

keep_section <- function(.data,t,section, sufix){
  
  .data %>%
    select(id = RespondentID,
         country,
         region,
         role,
         theme = t,
         starts_with(glue('{section}_{sufix}'))
  ) %>%
    dplyr::filter(!is.na(theme)) %>%
    tidyr::pivot_longer(-c(theme, country, role, id, region))
  
}


