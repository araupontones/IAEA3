#sf_map
library(rnaturalearth)
library(rnaturalearthdata)
source('functions/clean_countries.R')

world_sf <- function(){
  
  
  
  world <- ne_countries(scale = "medium", returnclass = "sf") %>%
    relocate(sovereignt) %>%
    rename(country = sovereignt) %>%
    clean_countries() %>%
    filter(is.na(tiny),
           !is.na(homepart),
           continent != "Antarctica") 
  
  return(world)
  
}
