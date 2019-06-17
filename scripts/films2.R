#### Tidy Tuesday week 30 - Movie revenues

## Load libraries
library(tidyverse)
library(lubridate)
library(ggrepel)

## Load data
movies = read_csv("../data/movie_profit.csv") %>% 
  select(-X1) %>%                                                           # remove X1 column
  distinct(release_date, movie, production_budget, .keep_all=T) %>%         # remove duplicate movies
  mutate(release_date = mdy(release_date),
         genre = factor(genre),
         year = year(release_date),
         value_proportion = worldwide_gross/production_budget
  ) %>% 
  filter(release_date < "2018-10-20")                                       # remove movies that haven't come out yet

## Plot
ggplot(data=movies, aes(x=reorder(genre, value_proportion, max), y=value_proportion)) + geom_point(aes(size=worldwide_gross/1000000, color=genre), alpha=0.5) + 
  geom_label_repel(data=movies[movies$value_proportion>250,], aes(label=movie), min.segment.length=5) +
  ylim(c(0,500)) +
  scale_color_brewer(palette="Set1", guide=F) + 
  scale_size(range = c(1, 20)) +
  scale_x_discrete(limits = rev(levels(reorder(movies$genre, movies$value_proportion, max)))) +
  labs(title="Which movie genres are most profitable to produce?", subtitle="Movies with a revenue more than 250x their budget are labelled\n", x="", y="Revenue as proportion of budget\n", size="Worldwide gross\nrevenue (millions USD)", caption="Source: the_numbers, plot by @veerlevanson") +
  theme_minimal(14) + 
  theme(legend.position = "right",
        text=element_text(family="Roboto"),
        plot.title = element_text(size=18, hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(size = 12, hjust = 1),
        axis.text.y = element_text(hjust = 0),
        panel.grid = element_line(colour = "#F0F0F0"),
        plot.margin = unit(c(1,0.5,0.5,1), "cm")
  )

# Save plot
ggsave(filename = "week30_HorrorMovies/Movie_revenue.jpg", width=15, height=10, units="cm", scale=1.6)

# Percentage of movies that make more than their budget
round(sum(movies$value_proportion>1)/nrow(movies)*100,1)
