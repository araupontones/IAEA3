library(rio)
library(ggplot2)
library(dplyr)
gmdacr::load_functions('functions/themes/')

foas <- import('data/9.lookups/foas.rds')

#All Projects 
projects <- import('data/1.reference/Copy of CPs_2022_09_12.xlsx') %>%
  filter(!is.na(FOACode)) %>%
  count(FOACode,Area )



dictionary <- import('data/1.reference/Copy of FOA mapping - old to new.xlsx') %>%
  rename(Area = `Thematic Area`)

#How many old FOAs?=============================================================

create_plot <- function(.data, title){
  
  
  .data %>%
    ggplot(aes(x = n,
               y = reorder(Area,n),
               label= n)) +
    geom_col(fill = blue_sky) +
    geom_text(hjust= 1,
              color = 'white',
              family = "Open Sans") +
    labs(y = "",
         x= title) +
    
    theme_main() +
    theme(axis.title.x = element_text(color = "black"),
          axis.text = element_text(size = 12))
}

dictionary %>%
  count(Area ) %>%
  create_plot("# of OLD FOAs")
  


names(dictionary)

# How Many new FOas? ======================================================
names(dictionary)

length(unique(dictionary$new))
dictionary %>%
  count(new, Area) %>%
  count(Area) %>%
  create_plot(title = "# of NEW FOAS") +
  theme(axis.text.x = element_blank())

#How Many FOA NLO1 ================================================================

sum(t$n)
unique(dictionary$cp_improvements)
t <- dictionary %>%
  filter(!is.na(foa_nlo1)) %>%
  count(`foa_nlo1`, Area) %>%
  count(Area) %>%
  create_plot(title = "# of FOAS in NLO1") +
  theme(axis.text.x = element_blank())
names(dictionary)
#How many foas in CP
t <- dictionary %>%
  filter(!is.na(cp_improvements)) %>%
  count(cp_improvements, Area) %>%
  count(Area) %>%
  create_plot(title = "# of FOAS in CP") +
  theme(axis.text.x = element_blank())

t
