# global.R
# load libraries, utility functions, refernce data

#### Setting Things Up ####=====================================================
setwd("C:/virtual_learning")

# load libraries
library(tidyverse)        # tools for data anlaysis
library(googlesheets4)    # read/write google sheets
library(ggthemes)         # themes for plotting
library(scales)           # enhanced ggplot scales
library(lubridate)        # date/time functions

# authenticate google
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
                    col_types = "c", .name_repair = "minimal")

resp0 <- resp0[, -(51:53)]

resp0[is.na(resp0)] <- "No Response"

names(resp0) <- col_ref0$QUESTION_CODE[match(names(resp0), 
                                              col_ref0$QUESTION)]

resp0 <- resp0 %>% 
  mutate(RECORD_ID = str_remove_all(TIMESTAMP, "[ :/-]")) 

resp0 <- resp0 %>% 
  mutate(School = fct_lump_min(DISTRICT, min = 2, other_level = "Other (<2 responses)")) %>% 
  mutate_at(vars(contains("GRD_COUNT")), .funs = ~parse_number(.)) %>% 
  mutate_at(vars(contains("GRD_COUNT")), .funs = ~replace_na(., 0))

# Need to pivot this down big time
resp_melt0 <- resp0 %>% 
  gather(key = "QUESTION_CODE", 
         value = "RESPONSE", -RECORD_ID)

# add the easier to deal with names
resp_melt1 <- resp_melt0 %>% 
  left_join(col_ref0)

# In addition to a long and narrow data set, we should also make an even wider
#  data set that turns all the categorical responses into 0/1

# Prepping the factor variables
factor_vars <- col_ref0 %>% 
  filter(CAT == "FACTOR") %>% 
  pull(QUESTION_CODE)

factors0 <- resp0 %>% 
  select(RECORD_ID, all_of(factor_vars))

factors_step1 <- lapply(resp0 %>% select(all_of(factor_vars)), 
                        function(m) bind_cols(resp0 %>% select(RECORD_ID), VAR = m))

factors_step2 <- lapply(factors_step1, function(m) {
  m %>% mutate(VAL = 1) %>% 
    pivot_wider(names_from = VAR, 
                values_from = VAL, 
                values_fill = list(VAL = 0))
})

factors_step3 <- mapply(n = factors_step2, 
                        m = list(names(factors_step2)), 
                        FUN = function(m, n){
                          names(n) <- paste0(m, names(n))
                        }, SIMPLIFY = FALSE)

write_sheet(resp_melt1, "https://docs.google.com/spreadsheets/d/1LQdxe44OHT_V1ksaISdo3lmagjkwV4m3QPC4zgnNbP8/edit#gid=0",
            sheet = "RESPONSE_MELT")


