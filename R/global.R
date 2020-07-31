# global.R
# load libraries, utility functions, refernce data

#### Setting Things Up ####=====================================================
# load libraries

library(tidyverse)        # tools for data anlaysis
library(googlesheets4)    # read/write google sheets
library(ggthemes)         # themes for plotting
library(scales)           # enhanced ggplot scales
library(lubridate)        # date/time functions

# authenticate google
options(gargle_quiet = FALSE)

gs4_auth(email = "ncriswell@gmail.com")

#### Reference Data ####========================================================

col_ref0 <- read_sheet("https://docs.google.com/spreadsheets/d/1lqTpN0id5B30J5Tg7xDvo-DPXjC42THhsRDMPD4hFY0/edit#gid=1590031945", 
                          sheet = "COLUMN_NAMES")

imp_ref0 <- read_sheet("https://docs.google.com/spreadsheets/d/1lqTpN0id5B30J5Tg7xDvo-DPXjC42THhsRDMPD4hFY0/edit#gid=1590031945", 
                       sheet = "REF_IMPORTANCE")

ess_ref0 <- read_sheet("https://docs.google.com/spreadsheets/d/1lqTpN0id5B30J5Tg7xDvo-DPXjC42THhsRDMPD4hFY0/edit#gid=1590031945", 
                       sheet = "REF_ESSENTIAL")

#### Read and Transforms Responses ####=========================================
resp0 <- read_sheet("https://docs.google.com/spreadsheets/d/1_Ao0QGbhJSbKa1mJ0WrOuwGE4BxFoq4sH-AgqrAK5TM/edit?ts=5f22bcb7#gid=806358879", 
                    sheet = "Form Responses 1",
                    col_types = "c")

resp0 <- resp0 %>% 
  mutate(RECORD_ID = str_remove_all(Timestamp, "[ :/-]")) %>% 
  select(-(51:53))

# Need to pivot this down big time
resp_melt0 <- resp0 %>% 
  gather(key = "QUESTION", 
         value = "RESPONSE", -RECORD_ID)

# add the easier to deal with names
resp_melt1 <- resp_melt0 %>% 
  left_join(col_ref0)

write_sheet(resp_melt1, "https://docs.google.com/spreadsheets/d/1LQdxe44OHT_V1ksaISdo3lmagjkwV4m3QPC4zgnNbP8/edit#gid=0",
            sheet = "RESPONSE_MELT")
