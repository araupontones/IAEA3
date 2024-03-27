library(rio)
library(ggplot2)
library(RColorBrewer)
library(extrafont)
library(dplyr)
library(janitor)
library(glue)


relevance <- import('data/7.NLO/2.raw_formatted/Part_1_relevance.rds')

names(relevance)


tabyl(relevance, stage)
powerbi_rel <- relevance %>%
  #drop missing foas (we need to associate the missing ones in the mapping_foas)
  filter(!is.na(foa)) %>%
  group_by(country,region, theme,improvement, foa, foa_nlo1) %>%
  #identify the highest stage of each country in each foa
  mutate(max_stage =  max(stage),
         max = stage == max_stage
  ) %>%
  ungroup() %>%
  relocate(max_stage, stage, country, foa) %>%
  filter(max) %>%
  #identify when these where achieved
  group_by(country,region, theme,improvement, foa, foa_nlo1) %>%
  summarise(when_impact = min(period, na.rm = T),
           achieved_stage = max(stage),
            .groups = 'drop'
  ) %>%
  mutate(achieved_stage = as.character(achieved_stage),
         achieved_stage = case_when(achieved_stage == "Not applicable" ~ paste0("0.", achieved_stage),
                                    achieved_stage == "Initial stage" ~ paste0("1.", achieved_stage),
                                    achieved_stage == "Established stage" ~ paste0("2.", achieved_stage),
                                    achieved_stage == "Master stage" ~ paste0("3.", achieved_stage),
                                    achieved_stage == "Regional reference" ~ paste0("4.", achieved_stage),
                                    
                                    
                                    
                                    
                                    ),
         achieved = 1
         )


tabyl(powerbi_rel, achieved_stage)
export(powerbi_rel, glue('data/11.powerbi/relevance.csv'))
