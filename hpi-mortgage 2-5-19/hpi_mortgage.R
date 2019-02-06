library(tidyverse)
library(gganimate)
library(USAboundaries)
library(sf)

state_hpi <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-05/state_hpi.csv")
#mortgage_rates <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-05/mortgage.csv")
#recession_dates <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-05/recessions.csv")

usa <- us_states()
usa <- usa %>% filter(name != "Alaska", name != "Hawaii", name != "District of Columbia", jurisdiction_type != "territory")
usa_hpi <- usa %>% left_join(state_hpi, by = c("stusps" = "state"))


p <- usa_hpi %>% 
        ggplot() + 
        geom_sf(aes(fill = price_index)) + 
        scale_fill_distiller(name = "", palette = "BuGn", direction = 1) + 
        coord_sf(crs = 5070) + 
        theme_void() + 
        theme(panel.grid = element_line(color = 'white')) + 
        labs(title = "House Price Index in {frame_time}", caption = "Data source:  Freddie Mac House Price Index (State and National)") + 
        transition_time(year)

animate(p, fps = 3)

## Excursion -- can we look at the change year-over-year instead?
pct <- function(x) {((x/lag(x))-1)*100}

usa_hpi_change <- usa_hpi %>% 
  group_by(state_abbr, year) %>% 
  filter(month == 1) %>% 
  mutate(change = price_index) %>% 
  group_by(state_abbr) %>% 
  mutate_each(funs(pct), change)

p2 <- usa_hpi_change %>%
  ggplot() + 
  geom_sf(aes(fill = change)) + 
  scale_fill_distiller(name = "Percent change \nfrom previous year", palette = "RdBu", direction = 1) + 
  coord_sf(crs = 5070) + 
  theme_void() + 
  theme(panel.grid = element_line(color = 'white')) + 
  labs(title = "House Price Index in {frame_time}", caption = "Data source:  Freddie Mac House Price Index (State and National)") + 
  transition_time(year)

animate(p2, fps = 3)
