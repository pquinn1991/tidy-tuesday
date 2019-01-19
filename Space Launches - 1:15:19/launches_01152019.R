library(tidyverse)

launches <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-01-15/launches.csv")

launches %>% 
  mutate(state_code = ifelse(state_code %in% c("RU", "SU"), "USSR/Russia", as.character(state_code))) %>% 
  mutate(state_code = ifelse(state_code == "US", "United States", as.character(state_code))) %>% 
  mutate(state_code = ifelse(state_code == "F", "France", as.character(state_code))) %>% 
  mutate(state_code = ifelse(state_code == "CN", "China", as.character(state_code))) %>% 
  mutate(agency_type = ifelse(category == "F", "failed", as.character(agency_type))) %>%
  mutate(agency_type = factor(agency_type, levels = c("startup", "private", "state", "failed"))) %>%
  mutate(state_code = factor(state_code, levels = c("France", "United States", "USSR/Russia", "China"))) %>% 
  filter(state_code %in% c("USSR/Russia", "United States", "France", "China")) %>% 
  ggplot(aes(x = launch_year, fill = agency_type)) + 
    geom_histogram(bins = 62) + 
    facet_grid(state_code ~ .) + 
    scale_fill_manual(values=c("#C77CFF", "#619CFF", "grey", "#F8766D")) + 
    theme_minimal() + 
    scale_y_continuous("Launches", breaks = seq(0,100,100)) + 
    scale_x_continuous("", breaks = seq(1960,2020,10)) + 
    labs(title = "Who is responsible for the increase in private sector launches?", 
         subtitle = "During the space race (from 1957 until the fall of the Soviet Union in 1991), the U.S. and USSR had massive state-run programs, with 
many launch failures early on. In the post-space race era, the U.S. and France have contributed the most to the increase in private 
sector launches (including by startups like SpaceX), while China and Russia have continued to rely on state-run programs.
         ") + 
    theme(panel.grid.minor = element_blank(), legend.title = element_blank(), plot.title = element_text(color="black", size=20, face="bold.italic"),
          axis.title.y = element_text(color="black", size=12, face="bold"), axis.text.x = element_text( color="black", size=10), 
          legend.position = c(.93, 1.07), legend.background = element_rect(fill="grey95", size=0.5, linetype="dashed",colour ="darkblue"))
