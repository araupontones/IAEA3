library(ggplot2)
library(extrafont)
#check response rates
all_responses <- list.files('data/6.data-collection/2.response_rates', pattern = "xlsx")
all_responses

reports <- lapply(all_responses, function(x){
  
  
  r <- rio::import(file.path("data/6.data-collection/2.response_rates",x)) %>%
    filter(Country_name == "TOTAL") %>%
      mutate(date = str_remove_all(x, "responses_by_country_|.xlsx")) %>%
    select(Submitted, Accessed, date)
    
  
  
  
}) %>% do.call(rbind,.)



clean_r <- reports %>%
  mutate(date = lubridate::ymd(date)) %>%
  pivot_longer(-date,
               names_to = "status")

responses  = max(clean_r$value[clean_r$status == 'Submitted'])
rsponses_label = prettyNum(responses, big.mark = ",")




ggplot(clean_r,
       aes(x = date,
           y = value,
           color = status)
       ) +
  
  geom_line(size = 1) +
  geom_point(size = 3) +
  scale_x_date(date_breaks = '1 day', date_labels = "%b %d") +
  scale_color_manual(name = "",
                     values = c('#019BAD', '#72B62C')) +
  scale_y_continuous(
    breaks = seq(0,responses +500, 500),
    labels = function(x)prettyNum(seq(0,responses +500, 500), big.mark = ",")) +
  theme_minimal() +
  labs(y = "Counterparts",
       x ="",
       title = "Evolution of Responses",
       subtitle = glue("CP Survey: {rsponses_label} counterparts have submitted their interview."),
       caption = "Data: CP Survey | November 23 2023")+
  theme(
    plot.title.position = 'plot',
    plot.title = element_text(size = 20, face = 'bold'),
    plot.subtitle = element_text(size = 14),
        axis.text.x = element_text(angle = 90),
    axis.title.x = element_text(margin = margin(r = 20)),
    legend.text = element_text(size = 11),
        legend.position = 'top',
        text = element_text(family = "Open Sans")
        )



# str_remove_all(all_responses, "responses_by_country_|.xlsx")
