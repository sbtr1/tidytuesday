#### Tidy Tuesday week 30 - Movie revenues

## Load libraries
library(tidyverse)
library(lubridate)
library(ggrepel)

## Load data
films = read_csv("movie_profit.csv") %>% 
  select(-X1) %>%                                                           # remove X1 column
  distinct(release_date, movie, production_budget, .keep_all=T) %>%         # remove duplicate movies
  mutate(release_date = mdy(release_date),
         genre = factor(genre),
         year = year(release_date)
  ) %>% 
  filter(release_date < "2018-10-20")      

films$return <- (films$worldwide_gross/films$production_budget - 1) * 100
films$log_return <- log(films$return)

films[is.na(films$log_return)] <- -100

films <- films %>% arrange(desc(return))

films %>% top_n(20) %>%
  ggplot(aes(x=reorder(movie,-return),y=return, fill=genre)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  scale_fill_brewer(palette="Set3") +
  ggtitle("Movies with highest return on investment") +
  ylab("Return on investment, %") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        axis.title.x=element_blank())

ggplot(data=films, aes(films$return)) + geom_histogram()

films %>% ggplot(aes(x=genre, y=return, fill=genre)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width=0.2, alpha=0.2) +
  ylab("Return on investment") +
  theme(axis.title.x=element_blank(),
        legend.position="none") +
  ggtitle("Return on investment by genre")

films %>% ggplot(aes(x=genre, y=log_return, fill=genre)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width=0.2, alpha=0.2) +
  ylab("Return on investment, logarithmic") +
  theme(axis.title.x=element_blank(),
        legend.position="none") +
  ggtitle("Return on investment by genre")

library(beeswarm)
beeswarm(return ~ genre, data = films, pch = 16)

