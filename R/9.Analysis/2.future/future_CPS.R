#Future perspective from CPs

#research question ============================================================
#In your opinion, what are the aspects that may help ensuring sustainability of 
#the achievements reached with IAEA TCP support? 
#Please RANK your 3 priority technical support needed.

library(dplyr)
library(janitor)
library(tidyr)
library(rio)
library(ggplot2)
library(stringr)
library(extrafont)

#load data --------------------------------------------------------------------------
raw_d <- import('data/6.data-collection/4.clean/cps.rds') %>%
  select(counterpart,country, region, theme, ldc, joined, starts_with('future'))

#These where the categories in the questionnaire -----------------------------
needs = c(
  '1' ='Trained staff remain in the institution',
  '2' ='Trained staff applying knowledge acquired',
  '3' = 'Equipment properly maintained',
  '4' = 'Appropriate operational funding',
  '5' = 'Technology relevant to country priorities',
  '98' = 'Others')


needs_name <- function(x){
  
  needs[x]
  
}



#clean a bit ---------------------------------------------------------------------

#identify observations that left this section unanswered
raw_d$na_count <- apply(select(raw_d, starts_with('future')), 1, function(x) sum(is.na(x)) == 6)


clean_d <- raw_d %>%
  #drop observations with missing answers
  filter(!na_count) %>%
  select(-na_count) %>%
  #format data to better read
  pivot_longer(starts_with('future'),
               values_to = 'rank',
               names_to = 'needs_id'
  ) %>%
  #get the names of the needs
  mutate(needs = str_remove(needs_id, 'future__'),
         needs = needs_name(needs)
  ) %>%
  select(-needs_id)



#total of 2,605 counterparts
length(unique(clean_d$counterpart))

#analysis ====================================================================



#function to format data ------------------------------------------------------
format_ranks <- function(.data,
                         by = c('theme')){
  
  
  .data %>%
    #drop when rank is 0
    filter(rank != 0) %>%
    #Count the number of votes by group
    group_by_at(c('rank', 'needs', by)) %>%
    summarise(votes = n(),
              .groups = 'drop') %>%
    #count the number of counterparts by ranking
    group_by_at(c('rank', by)) %>%
    mutate(counterparts = sum(votes),
           perc = votes/counterparts * 100,
           check = sum(perc)) %>%
    arrange(rank, desc(perc)) %>%
    ungroup() %>%
    #count the numner of times an aspect was selected within the group
    #to be able to sort the aspects in the plot
    group_by_at(c('needs', by)) %>%
    mutate(times_selected = sum(votes),
           rank =factor(rank,
                        levels= c("3", "2", "1"),
                        ordered = T)) %>%
    ungroup() 
  
  
}





#draw plot ---------------------------------------------------------------------

plot_ranks <- function(.data){
  
  .data %>%
    ggplot(
      
      aes(x = perc,
          y = reorder(aspect, times_selected),
          fill = rank
      ) 
    ) +
    geom_col() +
    lapply(c(25,50,75), function(x){
      geom_vline(xintercept = x,
                 linetype = 'dashed')
      
      
    }) +
    scale_fill_brewer(breaks = rev(ranks),
                      labels = rev(ranks),
                      palette = "Greens",
                      name = "Ranked"
    ) +
    scale_x_continuous(breaks = seq(0,100, 25),
                       labels = function(x)paste0(x, "%"),
                       #labels = seq(0,100, 25),
                       limits = c(0,100),
                       expand = c(0,0)
    ) +
    scale_y_discrete(labels = label_wrap_gen(25)) +
    labs(title = 'Aspects that may help ensuring sustainability of the\nachievements reached with TCP.',
         subtitle = "Ranked by their piority.") +
    theme_main() +
    theme(legend.title = element_text())
  
}
ranks <- levels(global$rank)


#create data
#1. global
global<- clean_d %>% format_ranks(NULL)

#2. themes
themes <- clean_d %>%
    format_ranks(by = c('theme', 'region'))

export(themes, 'data/11.powerbi/future.csv')


#3. Region
regions <- clean_d %>%
  format_ranks(by = 'region')

global_plot <- global %>% plot_ranks()
themes_plot <- themes  %>% plot_ranks() + facet_wrap(~theme)
regions_plot <- regions  %>% plot_ranks() + facet_wrap(~region)

ggsave('analysis/plots/2.future/cp/global_future.png',
       global_plot)

ggsave('analysis/plots/2.future/cp/themes_future.png',
       themes_plot)

ggsave('analysis/plots/2.future/cp/regions_future.png',
       regions_plot)


regions_plot


