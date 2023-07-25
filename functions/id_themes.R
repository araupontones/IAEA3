
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
