library(tidyverse)
library(readxl)
library(janitor)

r_trial_data <- read_excel("data-raw/R trial 10.13.21.xlsx") %>% 
  clean_names()

r_trial_data %>% 
  mutate(year = 2020)


  
r_trial_data %>% 
  select(contains("going_well"))

r_trial_data %>% 
  count(going_well_design)

r_trial_data %>% 
  count(going_well_admin) %>% 
  drop_na() %>% 
  mutate(n = 100 * n / sum(n))


cols_changes <- r_trial_data %>% 
  select(contains("change")) %>% 
  colnames()
  
  
r_trial_data %>% 
  select(contains("change")) %>% 
  mutate(sum_yes = rowSums(.[cols_changes] == "Changes made"))

r_trial_data %>% 
  select(contains("change")) %>% 
  rowwise() %>% 
  mutate(the_count = sum(c_across(everything()) == "Changes made"))


r_trial_data %>% 
  select(contains("change")) %>% 
  mutate(sum_yes = rowSums(.[c("changes_programs", "changes_ops")] == "Changes made"))


r_trial_data %>% 
  select(contains("ability")) %>% 
  mutate(across(contains("ability"), ~parse_number(.)))

# If things were more complex I'd use {stringr}

r_trial_data %>% 
  select(contains("ability")) %>% 
  mutate(ability_implement = case_when(ability_implement == "5- High" ~ 5,
                                       ability_implement == "3" ~ 3,
                                       TRUE ~ NA_real_))







