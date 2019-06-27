library(tidyverse)
library(summarytools)
library(tidytext)
library(igraph)
library(ggraph)
library(gganimate)
library(ggthemes)
library(lubridate)
library(ggforce)


#read data:
ufo_sightings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-25/ufo_sightings.csv")


#Clean and tokenize 
bigram_counts <- ufo_sightings %>%
  select(description) %>%
  mutate(description = str_replace_all(description, "\\&\\#\\d+", "")) %>%  # Remove &#digit
  filter(!is.na(description)) %>%
  unnest_tokens(bigram, description, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  count(word1, word2, sort = TRUE) %>%
  filter(word1 != "nuforc" & word2 != "nuforc")

#Create graph structure:
bigram_graph <- bigram_counts %>%
  filter(n > 225) %>%
  graph_from_data_frame()

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

#plot:
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightgreen", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()  +
  theme(plot.margin=unit(c(.5, .5, .5, .5),"cm")) +
  labs(title = "Common Bigrams in UFO Sighting Descriptions",
       subtitle = "Data provided by NUFORC")




ufo_sightings_new<-ufo_sightings

ufo_sightings_new$date_time <- as.Date(ufo_sightings_new$date_time, "%m/%d/%Y %H:%M")

ufo_sightings_new$year<-year(ufo_sightings_new$date_time)

ufo_sightings_new$year<-cut(ufo_sightings_new$year,
                            breaks=c(1910,1919,1929,1939,
                                     1949,1959,1969,1979,
                                     1989,1999,2009,2015),
                            labels=c("1910s","1920s","1930s","1940s",
                                     "1950s","1960s","1970s","1980s",
                                     "1990s","2000s","2010s"))

ufo_sightings_new %>%
  group_by(year,ufo_shape) %>%
  count(ufo_shape) %>%
  remove_missing() %>%
  ggplot(.,aes(x=str_wrap(ufo_shape,10),y=n))+geom_col(fill=blues9[8])+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))+
  geom_text(aes(label=n),hjust=-0.1,angle=90)+
  scale_y_continuous(expand = c(0,1000))+
  transition_states(year)+ease_aes("linear")+
  xlab("Shape")+ylab("Frequency")+
  ggtitle("UFO shapes over the Years",
          subtitle = "Year:{closest_state}")