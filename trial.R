#Load Packages

library(tidyverse)
library(janitor)
#install.packages("readxl")
library(readxl)

#Import Data

R_trial_10_13_21 <- read_excel(path = "data-raw/R trial 10.13.21.xlsx")

#Clean data

ability_scale <- R_trial_10_13_21 %>% 
  clean_names() %>% 
  select(contains("ability")) %>% 
  mutate(ability_implement = str_remove(ability_implement, "- High")) %>% 
  mutate(ability_response_rates = str_remove(ability_response_rates, "- High")) %>% 
  mutate(ability_useful_data = str_remove(ability_useful_data, "- High")) %>% 
  mutate(ability_analyze = str_remove(ability_analyze, "- High")) %>% 
  mutate(ability_interpret = str_remove(ability_interpret, "- High")) %>%  
  mutate(ability_use_results = str_remove(ability_use_results, "- High")) %>%  
  mutate(ability_ctl = str_remove(ability_ctl, "- High")) %>% 
mutate(ability_implement = str_remove(ability_implement, "- Low")) %>% 
  mutate(ability_response_rates = str_remove(ability_response_rates, "- Low")) %>%  
  mutate(ability_useful_data = str_remove(ability_useful_data, "- Low")) %>% 
  mutate(ability_analyze = str_remove(ability_analyze, "- Low")) %>%  
  mutate(ability_interpret = str_remove(ability_interpret, "- Low")) %>% 
  mutate(ability_use_results = str_remove(ability_use_results, "- Low")) %>% 
  mutate(ability_ctl = str_remove(ability_ctl, "- Low")) %>% 
  drop_na(ability_implement) %>% 
  mutate

#Original graph code

ggplot(data = ability_scale,
       mapping = aes(x = ability_implement)) +
  geom_bar() +
  coord_flip() +
  labs(title = "Table 1 | ability_implement",
       x = "Ability Rating",
       y = "") +
  theme_classic()

#Graph code as function
ability_bar_graphs <- function(ability_type, title2) {
  ggplot(data = ability_scale,
       mapping = aes(x = ability_type)) +
  geom_bar() +
  coord_flip() +
  labs(title = title2,
       x = "Ability Rating",
       y = "")
}

#The arguments are working, but the bar graph shows up different when I use a function vs. the original code.

#Ability to CTL
ability_bar_graphs(ability_type = "ability_ctl",
                   title2 = "ctl")

#ability to implement
ability_bar_graphs(ability_type = "ability_implement",
                   title2 = "Ability to Implement")


 
  
 
