library(tidyverse)
library(plotly)

births <- read_csv("../data/us_births_2000-2014.csv")

births2 <- births %>% group_by(month, date_of_month) %>% 
  summarise(avg_births = mean(births)) 

births2$month <- as.factor(births2$month)
births2$date_of_month <- as.factor(births2$date_of_month)

births2$avg_births_z <- (births2$avg_births - mean(births2$avg_births)) / sd(births2$avg_births)

ggplot(data = births2, aes(x=month, y=date_of_month, fill=avg_births_z)) + 
  geom_tile(alpha=0.9) +
  scale_fill_distiller(direction=1) +
  ggtitle("Average births by day of the year") +
  theme(legend.position="none") +
  theme_minimal() +
  xlab("Month") +
  ylab("Day")

ggplot(data = births2, aes(x=month, y=date_of_month, color=avg_births_z)) + 
  geom_point(size=4, alpha=0.7) +
  scale_color_distiller(palette = "Blues", direction=1) +
  ggtitle("Average births by day of the year") +
  theme(legend.position="none") +
  theme_minimal() +
  xlab("Month") +
  ylab("Day")



