library(tidyverse)
library(summarytools)
library(tidytext)
library(igraph)
library(ggraph)
library(gganimate)
library(ggthemes)
library(lubridate)
library(ggforce)
library(widyr)
library(viridis)
library(wordcloud)


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
  filter(n > 250) %>%
  graph_from_data_frame()

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

#plot:
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "salmon", size = 3.5) +
  geom_node_text(aes(label = name), repel = TRUE, size=3.5) +
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


###
theme_options <- theme(
  legend.title = element_text(
    size = 9, margin = margin(b = 5), face = 'bold'
  ),
  legend.text = element_text(size = 7),
  legend.margin = margin(t = 15, b = 15),
  legend.key.width = unit(10, 'points'),
  plot.title = element_text(
    margin = margin(b = 12), color = '#32b37f', size = 14, hjust = 0.5,
    face = 'bold'
  ),
  plot.subtitle = element_text(
    margin = margin(b = 15), size = 11, hjust = 0.5, face = 'bold'
  ),
  plot.caption = element_text(color = '#dadada', size = 6, hjust = 1.09),
  plot.margin = margin(t = 40, r = 20, b = 20, l = 20)
)

id <- rownames(ufo_sightings)
ufo_sightings <- cbind(id=id, ufo_sightings)

ufo_words <- ufo_sightings %>%
  unnest_tokens('word', description) %>% 
  filter(!word %in% stop_words$word) %>% 
  filter(!word %in% c("44","2","3","4","5", "10","33","quot", "nuforc")) %>% 
  select(id,word)




top_ufo_words <- ufo_words %>%
  group_by(word) %>%
  summarize(total = n()) %>%
  arrange(desc(total)) %>%
  head(100)

top_ufo_words %>% 
  with(wordcloud(word,total))

threshold <- 0.05

ufo_word_correlations <- ufo_words %>%
  filter(word %in% top_ufo_words$word) %>%
  pairwise_cor(word, id, sort = TRUE) %>%
  filter(correlation > threshold) %>%
  arrange(desc(correlation))

ufo_averages_per_word <- ufo_words %>%
  filter(word %in% top_ufo_words$word) %>%
  group_by(word) %>%
  summarize(
    total = n()
    #avg_points = mean(points, na.rm = TRUE),
    #avg_price = mean(price, na.rm = TRUE)
  ) %>%
  rename(name = word) %>%
  arrange(desc(total))

graph <- ufo_word_correlations %>%
  rename(weight = correlation) %>%
  mutate(alpha = weight) %>%
  graph_from_data_frame(vertices = ufo_averages_per_word)




ggraph(graph, layout = 'auto', niter = 15000) +
  geom_edge_link(aes(edge_alpha = alpha*5), edge_width = 0.5, colour="white") +
  geom_node_point(aes(size = total, color = total)) +
  geom_node_text(
    aes(label = name), size = 3.5, repel = TRUE, colour="white"
  )  +
  labs(
    title = "Top 50 words in UFO sightings",
    subtitle = "sized by word frequency"
  ) +
  theme_graph() +
  theme(legend.position="none",
        plot.background = element_rect(fill="black"),
        plot.title=element_text(colour="white",hjust=0.5, vjust=0.5, face='bold'),
        plot.subtitle=element_text(colour="white",hjust=0.5, vjust=0.5, face='bold')) +
  scale_color_gradient(low = "olivedrab2", high="olivedrab4")
