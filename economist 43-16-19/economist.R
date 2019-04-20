library(tidyverse)

women_research <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-16/women_research.csv")


women_research %>% 
  #filter(country == "Japan") %>% 
  ggplot(aes(x = field, y = percent_women, fill = field)) + 
  geom_bar(width = 1, stat = "identity", color = "white") + 
  facet_wrap(~ country) + 
  geom_hline(yintercept = .5, linetype = "dashed") +
  geom_text(aes(0, .55, label = "50%")) + 
  coord_polar() +
  guides(fill = guide_legend(), color=guide_legend(override.aes=list(fill=NA))) + 
  my_theme() + 
  labs(title = "Still a man's world...", subtitle = "Women among researchers with papers published 2011-2015, percent of total. \nHealth sciences are closest to gender parity across all countries, but only in Portugal \nand Brazil do they cross the 50% threshold.\n\n") + 
  scale_fill_brewer(palette="Dark2")
