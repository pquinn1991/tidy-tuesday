library(tidyverse)
library(ggalt)
library(waffle)

## Cheese consumption
cheese <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-01-29/clean_cheese.csv")

## Clean data
cheese <- cheese %>% 
  filter(Year %in% c(1970, 2017)) %>% 
  replace(., is.na(.), 0) %>% 
  select(Year:Foods.and.spreads) %>% 
  mutate(Mozzarella.Other.Italian = Mozzarella + Italian.other) %>% 
  select(-one_of(c("Brick", "Mozzarella", "Italian.other")))

## Waffle charts
waffle1970 <- waffle(sort(setNames(round(as.numeric(cheese[1,2:11]/sum(cheese[1,2:11]))*100, 0), colnames(cheese)[2:11]), decreasing = TRUE), 
                     colors = setNames(c("#fd941f", "#ffd867", "#EDB91F", "#F86724", "#fffdd0", "#3c6cb0", "lightgrey", "#ffbd88", "#E7AE5D", "#fffa8a"), colnames(cheese)[2:11]),
                     size = 0.1, 
                     flip = TRUE, 
                     legend_pos = "right", 
                     xlab = "1970")

waffle2017 <- waffle(sort(setNames(round(as.numeric(cheese[2,2:11]/sum(cheese[2,2:11]))*100, 0), colnames(cheese)[2:11]), decreasing = TRUE), 
                     colors = setNames(c("#fd941f", "#ffd867", "#EDB91F", "#F86724", "#fffdd0", "#3c6cb0", "lightgrey", "#ffbd88", "#E7AE5D", "#fffa8a"), colnames(cheese)[2:11]),
                     size = 0.1, 
                     flip = TRUE, 
                     legend_pos = "off", 
                     xlab = "2017")

## Arrange plots
grid.arrange(waffle1970, waffle2017, ncol=2, top=textGrob("If the average American had one block of cheese (1970 vs. 2017)\n", gp=gpar(fontsize=20, fontface = "bold.italic")), widths = c(12, 10))
