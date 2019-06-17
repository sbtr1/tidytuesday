library(dplyr)
library(ggmap)
library(ggthemes)
library(gganimate)

meteorites <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-11/meteorites.csv")

meteorites <- meteorites %>% 
  drop_na() %>% 
  filter(mass > 0) %>% 
  filter(year > 1850 & year < 2020)

world <- map_data("world")

p <- ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group=group), 
               fill = "cornsilk", color = "grey50", size = .3) +
  geom_point(data = meteorites, aes(x = long, y = lat, size = mass), alpha = .6, color = "firebrick3") +
  scale_color_discrete(name = "Type") +
  scale_size_continuous(guide = FALSE) +
  theme_minimal() +
  theme(text = element_text(family = "Helvetica"),
        plot.title = element_text(size = 20),
        plot.background = element_rect(fill = "#003366"),
        plot.subtitle = element_text(face = 'bold', size = 20),
        panel.background = element_rect(fill = "#003366"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank()) +
  labs(title = "Meteorite landings",
       subtitle = "Year: {closest_state}",
       caption = "Data: NASA.gov") +
  transition_states(year, transition_length = 1, state_length = 1) +
  #transition_time(year) +
  enter_fade() +
  exit_shrink() +
  ease_aes("linear") +
  shadow_mark(past = TRUE) +
  shadow_mark(alpha = 0.1) +
  NULL



animate(p, fps=15,duration=15)

anim_save("meteorite_animated.gif", p, fps = 15, width = 800, height = 500)


