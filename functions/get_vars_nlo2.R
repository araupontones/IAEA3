#defines the questions of the variables for each theme in the NLO survey

get_vars_nlo2 <- function(theme){
  
  if(theme == 'fa'){
    
    vars_foa <- list(foa_1 = paste0("q000", c(6:8)),
                     foa_2 = c("q0009", paste0("q00", c(10:13))),
                     foa_3 = paste0("q00", c(14:16)),
                     foa_4 = paste0("q00", c(17:25))
                     
    )
    
    
  }
  
  
  return(vars_foa)
  
}
