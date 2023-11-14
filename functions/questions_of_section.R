questions_of_section <- function(starts, ends){
  
  section <- c(starts:ends)
  less_10 <- section == section[which(section < 10)]
  section <- as.character(section)
  section[less_10] <- paste0("0", section[less_10])
  
  questions <- paste0('q00', section)
  
  return(questions)
  
  
  
}

