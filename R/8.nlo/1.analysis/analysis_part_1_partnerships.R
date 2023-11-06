library(rio)
library(ggplot2)
library(RColorBrewer)
library(extrafont)
library(treemap)
library(treemap)
install.packages('treemap')

partners <- import('data/7.NLO/2.raw_formatted/Part_1_partnerships.rds')
missing <- scales::percent(sum(is.na(partners$value)) / nrow(partners))

glue('{missing} of the questions were missing')

#QA --------------------------------------------------------------------------

names(partners)
qa <- partners %>% 
  group_by(id) %>%
  summarise(questions = n(),
            unanswered = sum(is.na(value)),
            perc_unanswered = unanswered/questions
  )

empty <- sum(qa$perc_unanswered == 1)
empty
tabyl(partners, theme)
names(partners)
names(data_plot)


data_plot <- partners %>%
  filter(theme == "FOOD and AGRICULTURE") %>%
  filter(value != "Other") %>%
  filter(value != "Not applicable") %>%
  filter(!is.na(value)) %>%
  group_by(sector, value) %>%
  summarise(total = n()) %>%
  ungroup() %>%
  mutate(value = glue('{value}\n ({total})'))



treemap(data_plot,
        index= c("sector","value"),
        vSize = "total",
        #title
        title = "Most significant partnerships in the context of the TC programme.",
        fontfamily.title = "Open Sans",
        
        fontsize.labels = c(18,12),
        fontfamily.labels = "Open Sans",
        bg.labels = 'white',
        palette = "Blues",
        align.labels=list(
          #titles (sector)
          c("left", "top"),
          #labels (values)
          c("center", "center")
          
        ),
        border.col = c("white"),
        border.lwds=c(7,2) 
        
        
        ) 




ggsave('plots/visibility_partnerships.png',
       last_plot())
