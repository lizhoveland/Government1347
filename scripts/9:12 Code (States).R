## install via `install.packages("name")`
library(tidyverse)
library(ggplot2)
library(maps)
library(scales)
## set working directory here
setwd("~")

####----------------------------------------------------------#
#### Read and clean pres pop vote ####
####----------------------------------------------------------#

## read
popvote_df <- read_csv("popvote_1948-2016.csv")

## subset
popvote_df %>% 
  filter(year == 2016) %>% 
  select(party, candidate, pv2p)

## format
(popvote_wide_df <- popvote_df %>%
    select(year, party, pv2p) %>%
    spread(party, pv2p))

## modify
(popvote_wide_df <- popvote_wide_df %>% 
    mutate(winner = case_when(democrat > republican ~ "D",
                              TRUE ~ "R")))

pvstate_df <- read_csv("popvote_bystate_1948-2016.csv")

## create individual state data
penn <- pvstate_df %>%
  subset(state == "Pennsylvania")

florida <- pvstate_df %>%
  subset(state == "Florida")

nc <- pvstate_df %>%
  subset(state == "North Carolina")

arizona <- pvstate_df %>%
  subset(state == "Arizona")

michigan <- pvstate_df %>%
  subset(state == "Michigan")

wisco <- pvstate_df %>%
  subset(state == "Wisconsin")

## ggplot Wisconsin (did not use in blog post)

wisco %>% 
  ggplot(aes(x=year, y=D)) +
  geom_point(color="blue") +
  stat_smooth(method="lm", se=FALSE, color = "black") +
  geom_line(size=1, color = "blue") +
  scale_x_continuous(breaks = c(1948, 1952, 1956, 1960, 1964, 1968, 1972, 1976, 1980, 1984, 1988, 1992, 1996, 2000, 2004, 2008, 2012, 2016)) +
  scale_y_continuous(breaks = c(0, 250000, 500000, 750000, 1000000, 1250000, 1500000), labels = comma) +
  xlab("") +
  ylab("Vote Total") + 
  ggtitle("Democratic Vote Total from 1948-2016: Wisconsin") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 70, vjust = 1, hjust=1))

ggsave("Wisconsin_Democratic_Vote_TotalFINAL2.png", height = 6, width = 6)

## ggplot Penn (did not use in blog post)

penn %>% 
  ggplot(aes(x=year, y=D)) +
  geom_point(color="blue") +
  stat_smooth(method="lm", se=FALSE, color = "black") +
  geom_line(size=1, color = "blue") +
  scale_x_continuous(breaks = c(1948, 1952, 1956, 1960, 1964, 1968, 1972, 1976, 1980, 1984, 1988, 1992, 1996, 2000, 2004, 2008, 2012, 2016)) +
  scale_y_continuous(breaks = c(1500000, 2000000, 2500000, 3000000, 3500000), labels = comma) +
  xlab("") +
  ylab("Vote Total") + 
  ggtitle("Democratic Vote Total from 1948-2016: Pennsylvania") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 70, vjust = 1, hjust=1))

ggsave("Penn_Democratic_Vote_TotalFINAL.png", height = 6, width = 6)

## ggplot Florida

florida %>% 
  ggplot(aes(x=year, y=D)) +
  geom_point(color="blue") +
  stat_smooth(method="lm", se=FALSE, color = "black") +
  geom_line(size=1, color = "blue") +
  scale_x_continuous(breaks = c(1948, 1952, 1956, 1960, 1964, 1968, 1972, 1976, 1980, 1984, 1988, 1992, 1996, 2000, 2004, 2008, 2012, 2016)) +
  scale_y_continuous(breaks = c(500000, 1000000, 1500000, 2000000, 2500000, 3000000, 3500000, 4000000, 4500000, 5000000), labels = comma) +
  xlab("") +
  ylab("Vote Total") + 
  ggtitle("Democratic Vote Total from 1948-2016: Florida") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 70, vjust = 1, hjust=1))

ggsave("Flo_Democratic_Vote_TotalFINAL.png", height = 6, width = 6)

## ggplot AZ

arizona %>% 
  ggplot(aes(x=year, y=D)) +
  geom_point(color="blue") +
  stat_smooth(method="lm", se=FALSE, color = "black") +
  geom_line(size=1, color = "blue") +
  scale_x_continuous(breaks = c(1948, 1952, 1956, 1960, 1964, 1968, 1972, 1976, 1980, 1984, 1988, 1992, 1996, 2000, 2004, 2008, 2012, 2016)) +
  scale_y_continuous(breaks = c(0, 250000, 500000, 750000, 1000000, 1500000), labels = comma) +
  xlab("") +
  ylab("Vote Total") + 
  ggtitle("Democratic Vote Total from 1948-2016: Arizona") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 70, vjust = 1, hjust=1))

ggsave("Arizona_Democratic_Vote_TotalFINAL.png", height = 6, width = 6)

## ggplot Michigan (did not use in blog post)

michigan %>% 
  ggplot(aes(x=year, y=D)) +
  geom_point(color="blue") +
  stat_smooth(method="lm", se=FALSE, color = "black") +
  geom_line(size=1, color = "blue") +
  scale_x_continuous(breaks = c(1948, 1952, 1956, 1960, 1964, 1968, 1972, 1976, 1980, 1984, 1988, 1992, 1996, 2000, 2004, 2008, 2012, 2016)) +
  scale_y_continuous(breaks = c(500000, 1000000, 1500000, 2000000, 2500000, 3000000, 3500000), labels = comma) +
  xlab("") +
  ylab("Vote Total") + 
  ggtitle("Democratic Vote Total from 1948-2016: Michigan") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 70, vjust = 1, hjust=1))

ggsave("Michigan_Democratic_Vote_TotalFINAL.png", height = 6, width = 6)

## ggplot NC

nc %>% 
  ggplot(aes(x=year, y=D)) +
  geom_point(color="blue") +
  stat_smooth(method="lm", se=FALSE, color = "black") +
  geom_line(size=1, color = "blue") +
  scale_x_continuous(breaks = c(1948, 1952, 1956, 1960, 1964, 1968, 1972, 1976, 1980, 1984, 1988, 1992, 1996, 2000, 2004, 2008, 2012, 2016)) +
  scale_y_continuous(breaks = c(500000, 1000000, 1500000, 2000000, 2500000, 3000000, 3500000), labels = comma) +
  xlab("") +
  ylab("Vote Total") + 
  ggtitle("Democratic Vote Total from 1948-2016: North Carolina") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 70, vjust = 1, hjust=1))

ggsave("NC_Democratic_Vote_TotalFINAL.png", height = 6, width = 6)

