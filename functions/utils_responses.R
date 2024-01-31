# get responses by tool and country


#fix names of respondents
get_responses <- function(){
  
  responses <- import('report/sample/responses.xlsx')
  
  names(responses) <- responses[1,]
  names1<- paste("NLO1", names(responses)[2:4], sep = "-")
  names_2 <-paste("NLO2", names(responses)[5:7], sep = "-")
  names(responses) <- c("country", names1, names_2, "CP-Counterpart")
  responses <- responses[-1,]
  
  return(responses)
  
}

#check which countries responded to which survey ==============================
#check that countries responded to which

responses_surveys <- function(){
  
  responses <- get_responses()
  
  responses %>%
  mutate(
    #responded to nlo1
    nlo1 = !is.na(`NLO1-NLO`)|!is.na(`NLO1-NLA`)|!is.na(`NLO1-Regional Coordinator`),
    #responded to nlo2     
    nlo2 = !is.na(`NLO2-NLO`)|!is.na(`NLO2-NLA`)|!is.na(`NLO2-Regional Coordinator`),
    
    #responded to CP     
    cp = !is.na(`CP-Counterpart`),
    
    #responded to all
         
    all_surveys = cp & nlo1 & nlo2,
    
    #only cp     
    only_cp = cp & !nlo1 & !nlo2,
         
    #cp and NLO 1
    cp_nlo1 = cp & nlo1 & !nlo2,
         
    #CP and NLO 2
    cp_nlo2 = cp & !nlo1 & nlo2,
    
    #nlos but not cp
    
    nlos_no_cp = !cp & nlo1 & nlo2,
    
    only_nlo2 = !cp & !nlo1 & nlo2
         
  )  %>%
    #keep only relevant variables
    select(country, all_surveys, only_cp, cp_nlo1, cp_nlo2, nlos_no_cp, only_nlo2) %>%
  #keep only one observatoin per country
    pivot_longer(-country,
                 names_to = 'status') %>%
    filter(value)
  
}


responses_surveys_themes <- function(.data){
  
  
  
  .data %>%
    mutate(
      #responded to nlo1
      nlo1 = !is.na(nlo1),
      #responded to nlo2     
      nlo2 = !is.na(nlo2),
      
      #responded to CP     
      cp = !is.na(cp),
      
      #responded to all
      
      all_surveys = cp & nlo1 & nlo2,
      
      #only cp     
      only_cp = cp & !nlo1 & !nlo2,
      
      #cp and NLO 1
      cp_nlo1 = cp & nlo1 & !nlo2,
      
      #CP and NLO 2
      cp_nlo2 = cp & !nlo1 & nlo2,
      
      #nlos but not cp
      
      nlos_no_cp = !cp & nlo1 & nlo2,
      
      only_nlo2 = !cp & !nlo1 & nlo2
      
    )  %>%
    #keep only relevant variables
    select(country, theme, id_theme,all_surveys, only_cp, cp_nlo1, cp_nlo2, nlos_no_cp, only_nlo2) %>%
    #keep only one observatoin per country
    pivot_longer(-c(country,theme, id_theme),
               names_to = 'status') %>%
    filter(value)
  
}
