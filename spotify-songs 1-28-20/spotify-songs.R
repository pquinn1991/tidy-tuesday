## Tidy tuesday 1/28/20

library(tidyverse)
library(readr)
library(tidytuesdayR)
library(formattable)
library(GGally)
library(lubridate)
library(extrafont)
library(ggrepel)

# Get the Data

tuesdata <- tidytuesdayR::tt_load('2020-01-21') 
tuesdata <- tidytuesdayR::tt_load(2020, week = 4)

spotify_songs <- tuesdata$spotify_songs

#############################################################################################################

gg_spotify <- spotify_songs %>% 
                filter(playlist_genre %in% c("edm"), track_album_release_date >= "2014-01-01") %>% 
                group_by(track_id) %>% 
                slice(1) %>% 
                mutate(track_album_release_date = as.Date(track_album_release_date, format = "%Y-%m-%d"), year = year(track_album_release_date)) %>% 
                mutate(ACOUSTICNESS = as.factor(ifelse(acousticness > .25, "High acousticness", "Low acousticness")))

gg_spotify %>%
  ggplot(aes(x = track_album_release_date, y = track_popularity)) + 
    geom_point(aes(color = ACOUSTICNESS), alpha = 0.25, size = 2.2) + 
    geom_smooth(color = "red", linetype = 1, fill = "grey30", alpha = 0.3, size = .75, se = FALSE) + 
    scale_color_manual(values=c("purple4", "grey60")) + 
    my_theme() + 
    ylab("Popularity") + 
    #annotate("segment", x = as.Date("2014-01-01", format = "%Y-%m-%d"), xend = as.Date("2014-01-01", format = "%Y-%m-%d"), y = 0, yend = 105, color = "black") + 
    #annotate("segment", x = as.Date("2014-01-01", format = "%Y-%m-%d"), xend = as.Date("2020-01-30", format = "%Y-%m-%d"), y = 0, yend = 0, color = "black") + 
    geom_point(data = gg_spotify %>% filter(track_id == "06dT0EYXrhRQV1MsvWhNGv"), fill = "grey50", color = "black", pch = 21, alpha = 1, size = 2.2, stroke = 1.3) + 
    geom_label_repel(aes(label=ifelse(track_id == "06dT0EYXrhRQV1MsvWhNGv","Bad (feat. Vassy) - David Guetta \nAcousticness = 0.001",'')),hjust=0,vjust=0, segment.size = 0, ylim = c(40, 50), xlim = c(as.Date("2014-01-01", format = "%Y-%m-%d"), as.Date("2020-01-01", format = "%Y-%m-%d")), size = 3) + 
    geom_point(data = gg_spotify %>% filter(track_id == "14sOS5L36385FJ3OL8hew4"), fill = "purple", color = "black", pch = 21, alpha = 1, size = 2.2, stroke = 1.3) + 
    geom_label_repel(aes(label=ifelse(track_id == "14sOS5L36385FJ3OL8hew4","Happy Now - Kygo \nAcousticness = 0.374",'')),hjust=0,vjust=0, segment.size = 0, ylim = c(82, 95), xlim = c(as.Date("2018-01-01", format = "%Y-%m-%d"), as.Date("2020-01-01", format = "%Y-%m-%d")), size = 3) + 
    ggtitle("EDM songs are getting more acoustic...and more popular", subtitle = "Electronic Dance Music (EDM) has become a bigger and more popular genre in \nthe last five years. This may be because EDM songs have become much more \nacoustic than before. \n")


  