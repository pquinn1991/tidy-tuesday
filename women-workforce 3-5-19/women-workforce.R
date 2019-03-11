library(tidyverse)
library(ggplot2)
library(scales)
library(grid)

jobs_gender <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-05/jobs_gender.csv")

jobs_gender %>% 
  filter(year == 2016) %>% 
  gather("gender", "earnings", 10:11) %>% 
  ggplot(aes(x = earnings, group = gender, col = gender, fill = gender)) + 
    geom_density(alpha = 0.3) + 
    facet_wrap(major_category ~ ., strip.position = "top") + 
    scale_x_continuous(limits = c(0,125000), labels = dollar) + 
    scale_y_continuous(limits = c(0,.000055)) + 
    my_theme() + 
    theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.title.x = element_blank(), plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) + 
    labs(title = "Gender wage distribution", subtitle = "Comparing the distribution of wages between males and females in the major occupation categories. \nHealthcare fields tend to have better wage parity, while most others have significant wage disparities.\n\n")
    


p2 <- jobs_gender %>% 
  filter(year == 2016, major_category %in% "Management, Business, and Financial") %>% 
  gather("gender", "percent", 6:7) %>% 
  top_n(n = 40, wt = total_workers) %>% 
  mutate(occupation = fct_reorder(occupation, -percent_female, fun = mean)) %>% 
  ggplot(aes(x = occupation, y = ifelse(gender == "workers_male", 100-percent_female, -percent_female), fill = gender)) + 
    geom_col() + 
    scale_y_continuous(limits = c(-100,100)) + 
    coord_flip() + 
    my_theme() + 
    theme(axis.title.y = element_blank(), axis.text.x = element_blank(), axis.text.y = element_text(), plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) + 
    labs(y = "", title = "Gender mix in management and business", subtitle = "Exploring the gender distribution in the most common management, business, and financial occupations. \nAs expected, construction and agricutlutre are male dominated, while medical services and education are female dominated. \nHowever, the c-suite positions are still mostly filled by men. \n\n") + 
    annotation_custom(textGrob("More men", gp=gpar(fontsize=12, fontface="bold.italic", col = "#00BFC4")), xmin = 21, xmax = 21, ymin = 50, ymax = 50) + 
    annotation_custom(textGrob("More women", gp=gpar(fontsize=12, fontface="bold.italic", col = "#F8766D")), xmin = 21, xmax = 21, ymin = -50, ymax = -50)

gt <- ggplotGrob(p2)
gt$layout$clip[gt$layout$name=="panel"] <- "off"
grid.draw(gt)
