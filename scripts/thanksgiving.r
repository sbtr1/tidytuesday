library(tidyverse)
library(ggthemes)

meals <- read.csv("../data/thanksgiving_meals.csv")

meals <- meals %>% filter(celebrate=="Yes")

thx_table <- meals %>%
  group_by(prayer, us_region) %>% 
  summarize(TOTAL = n()) %>%
  na.omit() %>%
  spread(key = prayer, value = TOTAL) 

thx_table$pray <- thx_table$Yes/(thx_table$Yes+thx_table$No)

thx_table %>%
  ggplot(aes(x=us_region, y=pray, label=pray*100)) + 
    geom_point(size=4) + 
    geom_segment(aes(x=us_region, 
                   xend=us_region, 
                   y=0, 
                   yend=pray)) + 
    labs(title="Percentage of Respondents that Pray", 
       subtitle="by Region", 
       caption="source: fivethirtyeight") + 
    theme_fivethirtyeight() +
    theme(axis.text.x = element_text(angle=50, vjust=1, hjust=0.9))

meals[, c(27:39)] <- sapply(meals[, c(27:39)], as.numeric)

meals[is.na(meals)] <- 0


pie <- meals %>%
  group_by(us_region) %>% 
  summarize(apple = sum(pie1), buttermilk=sum(pie2),cherry=sum(pie3),
            chocolate=sum(pie4), coconut_cream=sum(pie5), key_lime=sum(pie6),
            peach=sum(pie7), pecan=sum(pie8), pumpkin=sum(pie9), sweet_potato=sum(pie10),
            none=sum(pie11), other=sum(pie12)) %>% 
  na.omit() 

pie_tidy <- pie %>% 
  gather("type", "number", 2:13)


pie_tidy$type <- factor(pie_tidy$type, levels=c("pumpkin", "apple", "pecan", "buttermilk",
                                      "cherry","chocolate","coconut_cream","key_lime",
                                      "peach","sweet_potato","other","none"))

pie_tidy$type <- fct_rev(pie_tidy$type)

pie_tidy %>% ggplot(aes(x=type,y=number, fill=us_region)) +
  geom_bar(stat="identity") +
  facet_wrap(~us_region) + 
  coord_flip() +
  theme_grey() +
  theme(legend.position = "none", axis.title.y=element_blank()) +
  labs(title="What pie do most people eat for Thanksgiving?", 
       subtitle="Faceted by region", 
       caption="source: fivethirtyeight",
       y="Number of respondents")

`pie[, c(2:13)] <- (pie[, c(2:13)])/rowSums(pie[2:13])
pie_normalized <- pie %>% 
  gather("type", "number", 2:13)

pie_normalized$type <- factor(pie_normalized$type, levels=c("pumpkin", "apple", "pecan", "buttermilk",
                                                "cherry","chocolate","coconut_cream","key_lime",
                                                "peach","sweet_potato","other","none"))

pie_normalized$type <- fct_rev(pie_normalized$type)



pie_normalized %>% ggplot(aes(x=type,y=us_region, color=us_region, size=number**0.7)) +
  geom_point(alpha=0.7, shape="circle") +
  coord_flip() +
  theme_economist() +
  theme(legend.position = "none", axis.title=element_blank()) +
  labs(title="What pie do most people eat for Thanksgiving?", 
       subtitle="Sized by relative frequency", 
       caption="source: fivethirtyeight") +
  theme(axis.text.x = element_text(angle=50, vjust=1, hjust=0.9))

