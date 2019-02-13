library(RColorBrewer)
library(tidyverse)

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
    scale_y_continuous(labels = scales::percent) + 
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

fed_avg <- fed_rd_combine %>% filter(year >= 2000, department != "Non-R&D") %>% group_by(department, year) %>% summarise(rd_budget = min(rd_budget)) %>% mutate(changePct = (rd_budget/lag(rd_budget))-1) %>% mutate(changePct = ifelse(is.infinite(changePct), NA, changePct)) %>% group_by(department) %>% summarise(rd_avg = mean(rd_budget, na.rm = TRUE), change_avg = mean(changePct, na.rm = TRUE), type = "Other R&D")

climate_avg <- climate_spend %>% group_by(department, year) %>% summarise(rd_budget = min(gcc_spending)) %>% mutate(changePct = (rd_budget/lag(rd_budget))-1) %>% mutate(changePct = ifelse(is.infinite(changePct), NA, changePct)) %>% group_by(department) %>% summarise(rd_avg = mean(rd_budget, na.rm = TRUE), change_avg = mean(changePct, na.rm = TRUE), type = "Climate R&D")

energy_avg <- energy_spend %>% filter(year >= 2000) %>% group_by(department, year) %>% summarise(rd_budget = min(energy_spending)) %>% mutate(changePct = (rd_budget/lag(rd_budget))-1) %>% mutate(changePct = ifelse(is.infinite(changePct), NA, changePct)) %>% group_by(department) %>% summarise(rd_avg = mean(rd_budget, na.rm = TRUE), change_avg = mean(changePct, na.rm = TRUE), type = "Energy R&D")

rd_avg <- rbind(fed_avg, climate_avg, energy_avg)

rd_avg %>% ggplot(aes(x = log(rd_avg), y = change_avg, color = type)) + geom_point()
rd_avg %>% ggplot(aes(x = 1, y = change_avg, color = type)) + geom_point(aes(size = rd_avg))
rd_avg %>% ggplot(aes(y = rd_avg, color = type)) + geom_boxplot()
rd_avg %>% ggplot(aes(y = change_avg, color = type)) + geom_boxplot()
