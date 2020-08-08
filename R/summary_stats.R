# data manipulation and summary stats

source("C:/virtual_learning/R/global.R")


dataframe %>%
  mutate( across(contains('oo'), 
                 .fns = list(cat = ~ntile(., 2))) )


school_conf0 <- resp0 %>% 
  group_by(School, CONFIDENCE) %>% 
  summarise(Responses = n()) %>% 
  ungroup()

ggplot(data = school_conf0, aes(x = str_wrap(CONFIDENCE, 25), y = Responses)) +
  geom_bar(stat = "identity", color = "black") + 
  facet_wrap(~School) + 
  # scale_y_discrete(name = "Number of Responses") + 
  coord_flip() + 
  ggtitle("Virtual Learning Questionnaire - Confidence by District") +
  theme_fivethirtyeight() + 
  theme(strip.text.x = element_text(face = "bold"),
        axis.title.x = element_text(),
        axis.text.x = element_text())

# number of childred by age
