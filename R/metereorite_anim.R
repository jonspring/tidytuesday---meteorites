library(tidyverse)
library(gganimate)

library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

meteorites <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-11/meteorites.csv")


# I want to animate the meteorites because that would look cool.
# Not able so far within native gganimate transitions, so making manual version.

fall_frames <- 20
hit_frames <- 60

meteorites_anim <- meteorites %>%
  filter(year >= 1940,
         year <= 2013,
         !is.na(year),
         !is.na(long), 
         fall == "Fell") %>%
  mutate(start_lat = 0,
         start_long = long + 90) %>%
  uncount(fall_frames + hit_frames, .id = "id_frame") %>%
  mutate(anim_yr = year - (fall_frames - id_frame),
         fall_pct = id_frame / fall_frames,
         cur_lat = pmin(1, fall_pct) * lat + (1 - pmin(1, fall_pct)) * start_lat,
         cur_long = pmin(1, fall_pct) * long + (1-pmin(1,fall_pct)) * start_long) %>%
  mutate(met_color = case_when(fall_pct > 0.95 ~ "black", 
                               fall_pct > 0.7  ~ "firebrick1",
                               TRUE  ~ "#FFFFCC"),
         met_alpha = case_when(fall_pct <= 1 ~ fall_pct,
                               TRUE ~ 1 - (id_frame - fall_frames)/(hit_frames+1))) %>%
  filter(anim_yr >= 1940, anim_yr <= 2013)


ggplot(meteorites_anim, aes(long, lat)) + 
  geom_polygon(data = map_data("world"), aes(group = group), fill = "gray60", alpha = 0.2) +
  geom_text(aes(x = -170, y = -65, label = anim_yr), size = 20, fontface = "bold", color = "gray90", alpha = 0.8) +
  geom_point(aes(x = cur_long, y = cur_lat, size = sqrt(mass), group = id, alpha = met_alpha, color = met_color)) +
  coord_map(projection = "mollweide", orientation = c(90, 0, 0)) +
  theme_minimal(base_size = 20) +
  theme(plot.subtitle = element_text(color = "gray70", face = "bold")) +
  scale_x_continuous(labels = NULL) +
  scale_y_continuous(labels = NULL) +
  scale_color_identity() +
  scale_alpha_identity() +
  scale_size_area(max_size = ) +
  guides(size = F) +
  labs(title = "The Sky is Falling", 
       subtitle = "Falling rocks by year and size", x = "", y ="") +
  transition_manual(anim_yr) -> a

animate(a, duration = 10, fps = 25, end_pause = 50, type = "cairo", width = 800, height = 500)
