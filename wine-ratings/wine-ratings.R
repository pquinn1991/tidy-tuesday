library(tidyverse)

wine <- read.csv("/Users/parkerquinn/Documents/quinnGithub/TidyTuesday/wine-ratings/ratings.csv") %>%
  select(-X) %>%
  extract(title, "year", "(\\d\\d\\d\\d)", convert = TRUE)

wine %>% group_by(year) %>% filter(variety == "Merlot", year >= 1997 & year <= 2015) %>% summarise(avg = mean(price, na.rm = TRUE), n = n()) %>% ggplot(aes(x = year, y = avg)) + geom_line()
