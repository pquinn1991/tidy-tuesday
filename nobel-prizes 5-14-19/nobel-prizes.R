library(tidyverse)

nobel_winners <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-14/nobel_winners.csv")

nobel_winners %>% 
  mutate(age = ifelse(month(birth_date) > 10, prize_year - year(birth_date) - 1, prize_year - year(birth_date))) %>%
  ggplot(aes(x = prize_year, y = age, color = category)) + 
    geom_point(alpha = 0.3, size = 2.5) + 
    geom_smooth(method = "loess", se = FALSE, span = .5) + 
    scale_y_continuous(limits = c(15,90)) + 
    scale_x_continuous(breaks = seq(1900,2000, 50)) + 
    facet_grid(. ~ category) + 
    my_theme() + 
    scale_color_brewer(palette = "Dark2") + 
    labs(y = "Age of prize winner\n", title = "World Peace? Leave it to the kids.", subtitle = "The age of Nobel Prize winners has increased over the years in every field (in some cases, significantly) with the notable exception of the Peace category, \nwhich has seen younger winners in recent years. Most notably, Pakistani activist Malala Yousafzai, who became the youngest Nobel laureate when she \nwon the prize in 2014 at just 17.")

