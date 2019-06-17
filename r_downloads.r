library(tidyverse)
downloads <- read_csv("r-downloads.csv")




countries <- downloads %>% 
  filter(country != "NA") %>% 
  group_by(country) %>% 
  summarise(count = n()) %>% 
  top_n(10) 

top <- countries$country

continents <- read_csv("continents.csv")

downloads <- merge(downloads, continents, by="country")

timeHMS_formatter <- function(x) {
  h <- floor(x/(60*60))
  m <- floor(x %% 60)
  lab <- sprintf('%02d:%02d', h, m) # Format the strings as HH:MM:SS
}

downloads %>% 
  filter(country %in% top) %>% 
  ggplot(aes(x=country, y=time, fill=continent)) +
  geom_violin() +
  geom_jitter(alpha=0.1, width=0.3) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  coord_flip() +
  scale_y_continuous(label=timeHMS_formatter, breaks = pretty(downloads$time, n = 5)) +
  ggtitle("CRAN downloads on Oct 23 by country and time")
  
