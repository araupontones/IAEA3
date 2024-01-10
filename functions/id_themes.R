
#function to detect strings
detecta <- function(x, find){stringr::str_detect(x, find)}

ids_themes <- function(theme){
  
  case_when(detecta(theme, "Food") ~ 1,
            detecta(theme, "Health") ~ 2,
            detecta(theme, "Energy") ~ 3,
            detecta(theme, "Industrial") ~ 4,
            detecta(theme, "Water") ~ 5,
            detecta(theme, "Safety") ~ 6,
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
