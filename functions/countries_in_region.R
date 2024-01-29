#get countries in region

countries_region <- function(){
lkp_countries <- import('data/9.lookups/countries.rds')
countries_in_region <- lkp_countries %>%
  group_by(region) %>%
  count(name = 'countries_in_region')

return(countries_in_region)

}

get_regions <- function(.data){
  lkp_countries <- import('data/9.lookups/countries.rds')
  .data %>%
    left_join(lkp_countries, by = "country") %>%
    clean_regions()
    
}

