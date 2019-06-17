library(tidyverse)
library(ggridges)
library(ggrepel)
library(ggthemes)

turnout <- read_csv("voter_turnout.csv")
turnout$percentage <- turnout$votes/turnout$eligible_voters * 100

turnout2 <- turnout[turnout$year %% 4 == 0, ]
turnout2 <- turnout2[!is.na(turnout2$percentage),]

turnout3 <- turnout[turnout$year %% 4 == 2, ]
turnout3 <- turnout3[!is.na(turnout3$percentage),]


is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

turnout2 %>%
  group_by(year) %>%
  mutate(outlier = ifelse(is_outlier(percentage), state, NA)) %>%
  ggplot(., aes(x = factor(year), y = percentage)) +
  geom_boxplot(fill="#4271AE", alpha=0.9, color= "#1F3552", outlier.colour = "black") +
  geom_text(aes(label = outlier), na.rm = TRUE, hjust = 0, nudge_x = 0.1, size=3.5) +
  ggtitle("Voter turnout in presidential elections") +
  theme(legend.position="none") +
  theme_fivethirtyeight() +
  ylab("Voter turnout (%)") +
  xlab("Year")

turnout3 %>%
  group_by(year) %>%
  mutate(outlier = ifelse(is_outlier(percentage), state, NA)) %>%
  ggplot(., aes(x = factor(year), y = percentage)) +
  geom_boxplot(fill="#4271AE", alpha=0.9, color= "#1F3552", outlier.colour = "black") +
  geom_text(aes(label = outlier), na.rm = TRUE, hjust = 0, nudge_x = 0.1, size=3.5) +
  ggtitle("Voter turnout in midterm elections") +
  theme(legend.position="none") +
  theme_minimal() +
  ylab("Voter turnout (%)") +
  xlab("Year")


