library(tidyverse)
library(tmap)
library(gganimate)
library(ggthemes)

milk <- read_csv("../data/state_milk_production.csv")
milk$year <- as.integer(milk$year)

anim <- milk %>% 
  group_by(region, year) %>% 
  summarise(sum_milk=sum(milk_produced)/1000000000) %>% 
  ggplot(aes(x=year,y=sum_milk, fill=region)) +
  geom_area() +
  facet_wrap(~region, ncol=5) +
  scale_x_continuous(breaks=seq(1970,2020,10)) +
  theme_fivethirtyeight() +
  theme(axis.title=element_blank(),
        legend.position="none",
        axis.text.x  = element_text(angle=90, vjust=0.5)) +
  labs(title="Rise of the Pacific", subtitle="Sum of milk production by region (bn lb)") +
  transition_reveal(along=year) +
  NULL

anim_save("filenamehere.gif", anim)

milk <- na.omit(milk)
