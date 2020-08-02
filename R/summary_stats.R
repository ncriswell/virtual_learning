# data manipulation and summary stats

source("C:/virtual_learning/R/global.R")

# where are the responses from?
district_count0 <- resp_melt1 %>% 
  filter(QUESTION_CODE == "DISTRICT") %>% 
  mutate(School = fct_lump_min(RESPONSE, min = 2, other_level = "< 2 Responses")) %>% 
  group_by(School) %>% 
  summarise(Num_Resp = n()) %>% 
  ungroup()

ggplot(data = district_count0, aes(x = fct_reorder(School, Num_Resp), y = Num_Resp)) +
  geom_segment(aes(xend = School, yend = 0)) + 
  geom_point() + 
  coord_flip()

# get the count of 
