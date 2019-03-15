library(tidyverse)

board_games <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-12/board_games.csv")

blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    axis.text.x=element_blank(),
    plot.title=element_text(size=14, face="bold.italic"),
    plot.subtitle = element_text(size=10),
    legend.title = element_blank()
  )  

board_games %>% 
  mutate(major_category = sapply(strsplit(category, ","), head, 1)) %>% 
  mutate(rating_category=cut(average_rating, breaks=c(-Inf, 5, 6, 7, Inf), labels=c("< 5.0","5.0 - 6.0", "6.0 - 7.0", "7.0+"))) %>% 
  group_by(rating_category, major_category) %>% 
  summarise(n = n()) %>%
  filter(major_category %in% c("Abstract Strategy", "Adventure", "Card Game", "Dice", "Economic", "Wargame")) %>%
  mutate(freq = n/sum(n)) %>%
  ggplot(aes(x = "", y = freq, fill = major_category)) + 
    geom_bar(width = 1, stat = "identity") + 
    coord_polar("y", start = 0) + 
    blank_theme + 
    facet_grid(~rating_category, switch = "x") + 
    labs(title = "Board game categories by average rating", 
         subtitle = "Dice and card games are more likely to have lower ratings, and the most highly rated \ngames are much more likely to be economic or wargames. Note: uncommon categories \nare removed and if a game fell under multiple categories, the first category was used.\n",
         y = "Rating") + 
    scale_fill_brewer(palette="Set3")

