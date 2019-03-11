phd_ranks <- phd_field %>% 
  mutate(field = gsub(".*general.*", "General", field)) %>% 
  mutate(field = gsub(".*other.*", "Other", field)) %>%  
  mutate(field = gsub(" biology.*", "", field)) %>% 
  mutate(field = gsub(", .*", "", field)) %>% 
  mutate(field = gsub(" and.*", "", field)) %>% 
  mutate(field = gsub(" \\(.*", "", field)) %>%
  filter(major_field == "Biological and biomedical sciences") %>% 
  group_by(year, field) %>% 
  summarise(tot_phds = sum(na.omit(n_phds)), broad_field = min(broad_field)) %>% 
  arrange(year, desc(tot_phds)) %>% 
  mutate(ranking = row_number()) %>%
  mutate(field = substr(field, 1, 20))

# Which are the fields that had an noteworthy rise or fall?
down_field <- c("Plant pathology", "Zoology", "Developmental")
up_field <- c("General", "Virology", "Biometrics", "Computational")

# Describe the trend for each field to color the chart
phd_ranks <- phd_ranks %>% mutate(swing = ifelse(field %in% down_field, "Down", ifelse(field %in% up_field, "Up", "None")))

# Bump chart
ggplot(phd_ranks, aes(x = year, y = ranking, group = field, color = swing)) + 
  geom_line(aes(color = swing, alpha = .9), size = 2) + 
  geom_point(aes(color = swing, alpha = .9), size = 4) + 
  geom_point(color = "#FFFFFF", size = 1) + 
  scale_y_reverse(breaks = c(1,10,20, 30, 40)) + 
  my_theme() + 
  scale_x_continuous(breaks = 2008:2017, minor_breaks = 2008:2017, limits = c(2007, 2018)) + 
  geom_text(data = phd_ranks %>% filter(year == 2008),
            aes(label = field, x = 2007.8) , hjust = 1, fontface = "bold", color = "#888888", size = 3) +
  geom_text(data = phd_ranks %>% filter(year == 2017),
            aes(label = field, x = 2017.2) , hjust = 0, fontface = "bold", color = "#888888", size = 3) + 
  scale_color_manual(values = c(Down = "#D8B365", Up = "#5AB4AC", None = "gray")) + 
  labs(x = "Year", y = "Popularity rank", title = "Biological and biomedical PhDs awarded by field", subtitle = "Among others, biometrics and computational biology skyrocket, perhaps as the field takes advantage of recent advances in modeling and computing. \nInterestingly, no major shifts away from any particular fields, and the life sciences remain among the most sought-after doctorates.")
