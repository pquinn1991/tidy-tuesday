library(tidyverse)
library(ggalt)

prison <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-01-22/prison_population.csv")

## Scatter plot for 15

counties <- prison %>%
  filter(year == 2015, !is.na(population), !is.na(prison_population), !(pop_category %in% c("Female","Male","Total"))) %>%
  group_by(county_name, region, pop_category) %>%
  summarise(total_population = sum(population), total_prison_population = sum(prison_population)) %>%
  group_by(county_name) %>%
  mutate(population_rate = total_population /sum(total_population), prinate_rate = total_prison_population / sum(total_prison_population))

counties %>% filter(pop_category == "Black") %>% 
  ggplot(aes(x = population_rate, y = prinate_rate, color = region)) + 
  geom_point(alpha = 0.3, size = 3) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", size = .5) + 
  scale_x_continuous("% Black (county)", limits = c(0,1), labels = scales::percent_format(accuracy = 1)) + 
  scale_y_continuous("% Black (prisons)", limits = c(0,1), labels = scales::percent_format(accuracy = 1)) + 
  geom_smooth(method = "loess", alpha = 0.3, size = 1, se = FALSE) +
  theme_minimal() + 
  labs(colour="Region") + 
  labs(title = "Are blacks overrepresented in prisons?", 
       subtitle = "Comparing the black population in each U.S. county versus the prison population 
in that county shows a concerning trend. Are there significant regional differences?") +
  theme(plot.title = element_text(color="black", size=20, face="bold.italic"))

## Dumbbell plot

regions <- prison %>%
  filter(year == 2015, !is.na(population), !is.na(prison_population), !(pop_category %in% c("Female","Male","Total"))) %>%
  group_by(region, pop_category) %>%
  summarise(total_population = sum(population), total_prison_population = sum(prison_population)) %>%
  group_by(region) %>%
  mutate(population_rate = total_population /sum(total_population), prison_rate = total_prison_population / sum(total_prison_population))

title = c("Percentage of each region's ","entire population ","and ","prison population ","by race")
colors = c('black', 'grey','black', 'darkorange1','black')

grid.arrange(textGrob("Race disparities in incarceration rates persist across regions", gp=gpar(fontsize = 16, fontface="bold.italic")), 
             tableGrob(t(title), 
                       theme=ttheme_minimal(padding=unit(c(0,2),'mm'),
                                            base_colour = colors)), 
             regions %>% filter(pop_category %in% c("White", "Black")) %>% 
               ggplot(aes(y=pop_category, x=population_rate, xend=prison_rate)) + 
               geom_dumbbell(size=3, color="#e3e2e1", colour_x = "grey", colour_xend = "darkorange1") + 
               facet_grid(region~.) + 
               scale_x_continuous("Population %", labels = scales::percent_format(accuracy = 1), limits = c(0,1), breaks = seq(0,1,.25)) + 
               scale_y_discrete("") + 
               geom_text(aes(x=prison_rate + (.06 * sign(prison_rate - population_rate)), y=pop_category, label=ifelse(prison_rate - population_rate > 0, paste0("+", round(prison_rate - population_rate, 2)*100, "%"), paste0(round(prison_rate - population_rate, 2)*100, "%")), color = prison_rate - population_rate < 0), size=3.5, vjust=0.5) + 
               scale_colour_manual(values=c("firebrick", "darkgrey")) + 
               theme_bw() + 
               theme(legend.position = "none", panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(), plot.title = element_text(color="black", size=16, face="bold.italic")),
             heights = c(10,5,100)
)
