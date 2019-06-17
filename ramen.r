library(tidyverse)
library(ggthemes)
library(ggridges)

ramen_ratings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-04/ramen_ratings.csv")

ramen_ratings <- ramen_ratings[complete.cases(ramen_ratings[ , 6]),]

ramen_ratings <- ramen_ratings %>% 
  mutate(country = replace(country, country == "USA", "United States")) %>% 
  mutate(country = replace(country, country == "Sarawak", "Malaysia")) %>% 
  mutate(continent = case_when(country %in% c("Canada", "United States", "Mexico") ~ "N. America",
                               country %in% c("Germany", "UK") ~ "Europe",
                               TRUE ~ "Asia"))


ramen_ratings <- ramen_ratings %>%
  add_count(country) %>% 
  filter(n > 25)



ramen_ratings %>% 
  mutate(country = fct_reorder(country, stars, fun=median)) %>% 
  ggplot(aes(x=country, y=stars, fill=continent)) +
  geom_boxplot() + theme_hc() +
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        legend.position = "bottom",
        panel.background = element_rect(fill = "darkgrey", size = 0.1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "darkgrey")
        ) +
  labs(title = "Ramen ratings by country") + 
  coord_flip()
  


ramen_ratings %>% 
  mutate(country = fct_reorder(country, stars, fun=median)) %>% 
  ggplot(aes(x=stars, y=country, fill=continent)) +
  geom_density_ridges() +
  theme_ridges(grid = FALSE, center_axis_labels = TRUE) +
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        legend.position = "bottom") + 
  labs(title = "Ramen ratings by country")

ramen_ratings %>% 
  mutate(country = fct_reorder(country, stars, fun=median)) %>% 
  ggplot(aes(x=stars, y=country, fill=continent)) +
  stat_density_ridges(quantile_lines = TRUE)

