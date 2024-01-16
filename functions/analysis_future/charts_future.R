charts_future <- function(i, mode = "themes", db){
  
  #define parameters
  grupo1 = list_analysis[[i]]$group_1
  grupo2 = list_analysis[[i]]$group_2
  subtitulo =  list_analysis[[i]]$subtitle
  
  if(mode != 'themes'){
    
    #grupo1 = str_replace(grupo1, "theme", "foa")
    #grupo2 = str_replace(grupo2, "theme", "foa")
    subtitulo = str_replace(subtitulo, "Theme", "Field of Activity")
  }
  
  #Create data for plot
  #by_likert is created in functions/analysis_future
  #it counts answers by category
  data_plot <- db %>% by_likert(group_1 = grupo1,
                                          group_2 = grupo2)
  
  #create the chart
  #ggLikertbarsFuture is created in functions/plots
  grafica <- data_plot %>%
    ggLikertbarsFuture(titulo = "Importance of TCP in achieving results over the next 5-10 years.",
                       subtitulo = subtitulo) 
  
  #facet only if the list analysis is not 1, if it is not theme
  if(i != 1){
    
    grafica <- grafica +
      facet_wrap(grupo1[1])
  } else{
    
    grafica
  }
  
  
  #export chart
  # ggsave(glue('analysis/plots/2.future/{subtitulo}.png'),
  #        units = 'cm')
  
  return(grafica)
}
