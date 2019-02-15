library(RColorBrewer)
library(tidyverse)
library(grid)
library(gridExtra)
library(scales)

# Read data
fed_rd <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-12/fed_r_d_spending.csv")

# Find total amounts of discretionary money spent on non-r&d stuff
fed_non_rd <- fed_rd %>% 
  group_by(year) %>% 
  summarise(rd_budget = mean(discretionary_outlays) - sum(rd_budget), 
            department = "Non-R&D", 
            total_outlays = mean(total_outlays), 
            discretionary_outlays = mean(discretionary_outlays), 
            gdp = mean(gdp))

# Combine departments so the graph is easier to read
department2 <- data.frame("department" = c(unique(as.character(fed_rd$department)), "Non-R&D"), 
                          "department2" = c("Defense", "Space/Science", "Energy", "Health/Safety", 
                                            "Health/Safety", "Space/Science", "Natural resources", 
                                            "Natural resources", "Transportation", "Natural resources", 
                                            "Health/Safety", "Defense", "Defense", "Other", "Non-R&D"))

# Combine original data with non-r&d amount and new department groups
fed_rd_combine <- fed_rd %>% 
  bind_rows(fed_non_rd) %>% 
  left_join(department2, by = "department")

# Area plot
fed_rd_combine %>% 
  mutate(rd_perc = rd_budget/discretionary_outlays, 
         department2 = factor(department2, levels = c("Non-R&D", "Defense", "Energy", "Health/Safety", 
                                                      "Natural resources", "Other", "Space/Science", "Transportation"))) %>% 
  group_by(department2, year) %>% 
  summarise(rd_perc = sum(rd_perc)) %>% 
  ggplot(aes(x = year, y = rd_perc, fill = department2)) + 
    geom_area(color = "white", size = 0.15, alpha = 0.7) + 
    scale_fill_manual(values = c("grey", brewer.pal(7, "Dark2"))) + 
    scale_y_continuous(labels = percent) + 
    theme_minimal() + 
    labs(x = "\nFiscal Year", 
         y = "Federal Discretionary Spending", 
         title = "Federal Research and Development Spending", 
         subtitle = "Although total federal spending on R&D has increased, spending as a percentage \nof the total discretionary budget has steadily fallen.") + 
    theme(legend.title=element_blank(), 
          axis.title = element_text(face = "bold"), 
          axis.text = element_text(size = 10, face = "italic"), 
          panel.grid.minor = element_blank(), 
          plot.title = element_text(size = 18, face = "bold.italic"))

# If we wanted to use all of the factor levels for department, you can expand existing color brewer palette!
#colorCount = length(unique(fed_rd_combine$department2))
#getPalette = colorRampPalette(brewer.pal(8, "Dark2"))
#factorColors <- setNames(getPalette(colorCount), unique(fed_rd_combine$department2))
#factorColors["Non-R&D"] <- "lightgrey"


# Make a plot comparing energy and climate r&d spending compared to other r&d (non-defense?)
# Do a blog post looking at these articles? 
# https://fivethirtyeight.com/features/how-much-is-the-government-spending-on-climate-change-we-dont-know-and-neither-do-they/
# https://dotearth.blogs.nytimes.com/2014/11/02/panels-latest-warming-warning-misses-global-slumber-party-on-energy-research/
energy_spend <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-12/energy_spending.csv")
climate_spend <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-12/climate_spending.csv")

fed_delta <- fed_rd_combine %>% filter(year >= 2000, department != "Non-R&D") %>% group_by(year) %>% summarise(rd_budget = sum(rd_budget)) %>% mutate(changePct = (rd_budget/lag(rd_budget))-1) %>% mutate(changePct = ifelse(is.infinite(changePct), NA, changePct), type = "Other")
climate_delta <- climate_spend %>% group_by(year) %>% summarise(rd_budget = sum(gcc_spending)) %>% mutate(changePct = (rd_budget/lag(rd_budget))-1) %>% mutate(changePct = ifelse(is.infinite(changePct), NA, changePct), type = "Climate")
energy_delta <- energy_spend %>% filter(year >= 2000) %>% group_by(year) %>% summarise(rd_budget = sum(energy_spending)) %>% mutate(changePct = (rd_budget/lag(rd_budget))-1) %>% mutate(changePct = ifelse(is.infinite(changePct), NA, changePct), type = "Energy")
rd_deltas <- rbind(fed_delta, climate_delta, energy_delta)

budget_candlestick <- rd_deltas %>% 
  ggplot(aes(x = type, y = rd_budget, color = type)) + 
    geom_boxplot() + 
    scale_y_continuous(labels = dollar_format(scale = 1/1000000000)) + 
    theme_minimal() + 
    theme(legend.position = "none", 
          plot.title = element_text(face = "bold", size = 12), 
          axis.title = element_text(face = "bold"), 
          axis.text = element_text(size = 10, face = "italic"), 
          axis.text.x = element_text(size = 10, face = "bold.italic"), 
          panel.grid.minor = element_blank()) + 
    labs(x = "", y = "", title = "Annual spending on R&D ($B)")

change_candlestick <- rd_deltas %>% 
  ggplot(aes(x = type, y = changePct, color = type)) + 
  geom_boxplot() + 
  scale_y_continuous(labels = scales::percent) + 
  theme_minimal() + 
  theme(legend.position = "none", 
        plot.title = element_text(face = "bold", size = 12), 
        axis.title = element_text(face = "bold"), 
        axis.text = element_text(size = 10, face = "italic"), 
        axis.text.x = element_text(size = 10, face = "bold.italic"), 
        panel.grid.minor = element_blank()) + 
  labs(x = "", y = "", title = "Year-to-year change")

grid.arrange(budget_candlestick, 
             change_candlestick, 
             nrow = 1, 
             bottom = textGrob(
               "Data from 2000-2017",
               gp = gpar(fontface = 3, fontsize = 9),
               hjust = 1,
               x = 1), 
             top = textGrob("Climate and Energy R&D are way behind...\nand they aren't catching up any time soon.\n", gp = gpar(fontface = 4, fontsize = 16)))




rd_deltas %>% 
  ggplot(aes(x = year, y = rd_budget, color = type)) + 
  geom_line(size = 1) + 
  scale_y_continuous(labels = dollar_format(scale = 1/1000000000)) + 
  theme_minimal() + 
  theme(legend.title = element_blank(),
        plot.title = element_text(face = "bold.italic", size = 16), 
        axis.title = element_text(face = "bold"), 
        axis.text = element_text(size = 10, face = "italic"), 
        axis.text.x = element_text(size = 10, face = "bold.italic"), 
        panel.grid.minor = element_blank()) + 
  labs(x = "", y = "Annual spending on R&D ($B)", title = "Climate and Energy R&D are way behind", subtitle = "...and they aren't catching up any time soon.") + 
  guides(colour = guide_legend(reverse=T))
