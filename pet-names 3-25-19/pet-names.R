library(tidyverse)
library(lubridate)
library(formattable)
library(gridExtra)
library(grid)

seattle_pets <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-26/seattle_pets.csv")

# Most breed-specific names
t <- seattle_pets %>% 
  filter(species == "Dog" & !is.na(animals_name)) %>% 
  group_by(primary_breed, animals_name) %>% 
  summarise(n = n()) %>% 
  mutate(freq = n/sum(n)) %>% 
  ungroup() %>% 
  group_by(primary_breed) %>%
  mutate(n_breed = sum(n)) %>% 
  filter(n_breed >= 428) %>% 
  ungroup() %>%
  group_by(animals_name) %>% 
  mutate(name_n = sum(n)) %>% 
  ungroup() %>% 
  mutate(name_freq = name_n/sum(n)) %>% 
  filter(n != name_n) %>%
  mutate(diff = freq/name_freq) %>% 
  filter(n >= 3) %>% 
  group_by(primary_breed) %>% 
  top_n(n = 1, wt = diff) %>% 
  mutate(likelihood = round(diff, 0)) %>% 
  select(primary_breed, animals_name, likelihood)

colnames(t) <- c("Breed", "Most breed-specific name", "Ratio")

formattable(t,
            align = c("l", "c", "c"),
            list("Ratio" = color_tile("#DeF7E9", "#71CA97")))

