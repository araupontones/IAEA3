
#function to detect strings
detecta <- function(x, find){stringr::str_detect(x, find)}

ids_themes <- function(theme){
  
  case_when(detecta(theme, "Food") ~ 1,
            detecta(theme, "Health") ~ 2,
            detecta(theme, "Energy") ~ 3,
            detecta(theme, "Industrial") ~ 4,
            detecta(theme, "Water") ~ 5,
            detecta(theme, "Safety|safety") ~ 6,
            detecta(theme, "Knowledge") ~ 7
  )
}



names_themes <- function(theme){
  
  case_when(detecta(theme, "fa") ~ "FOOD and AGRICULTURE",
            detecta(theme, "h") ~ "HEALTH and NUTRITION",
            detecta(theme, "e") ~ "ENERGY PLANNING and NUCLEAR POWER",
            detecta(theme, "i") ~ "INDUSTRIAL APPLICATIONS and RADIATION TECHNOLOGY",
            detecta(theme, "w") ~ "WATER and ENVIRONMENT",
            detecta(theme, "n") ~  "NUCLEAR SAFETY AND SECURITY",
            detecta(theme, "Knowledge") ~ ""
  )
}


sufixes <- c("fa", "h", "e", "i", "w", "n")


names_themes_2 <- function(id){
  
  case_when(id == 1 ~ "FOOD and AGRICULTURE",
            id == 2 ~ "HEALTH and NUTRITION",
           id == 3 ~ "ENERGY PLANNING and NUCLEAR POWER",
            id == 4 ~ "INDUSTRIAL APPLICATIONS and RADIATION TECHNOLOGY",
            id == 5 ~ "WATER and ENVIRONMENT",
           id == 6  ~  "NUCLEAR SAFETY AND SECURITY",
            id == 7 ~ ""
  )
}

#From NLO to lokup foas
from_nlo_to_foas <- function(id){
  
  
  case_when(id == "FOOD and AGRICULTURE" ~ "Food and Agriculture",
            id == "HEALTH and NUTRITION" ~ "Health and Nutrition",
            id == "ENERGY PLANNING and NUCLEAR POWER" ~ "Energy",
            id == "INDUSTRIAL APPLICATIONS and RADIATION TECHNOLOGY" ~ "Industrial Applications/Radiation technology",
            id == "WATER and ENVIRONMENT" ~ "Water and the Environment",
            id == "NUCLEAR SAFETY AND SECURITY" ~ "Safety and Security",
            T ~ "A"
  )
  
  
}

