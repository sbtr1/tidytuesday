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
incidents <- readr::read_csv("../data/incident.csv")


bigram_counts <- incidents %>%
  select(description) %>%
  mutate(description = str_replace_all(description, "\\&\\#\\d+", "")) %>%  # Remove &#digit
  filter(!is.na(description)) %>%
  unnest_tokens(bigram, description, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  count(word1, word2, sort = TRUE) %>%
  filter(word1 != "nuforc" & word2 != "nuforc")


bigram_graph <- bigram_counts %>%
  filter(n > 10) %>%
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
  labs(title = "Common Bigrams in Argus Incident Reports")
