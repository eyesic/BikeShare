library(readr)
library(tidyverse)
hour <- read_csv("Documents/Personal Files/R/BikeSharing/hour.csv")
day <- read_csv("Documents/Personal Files/R/BikeSharing/day.csv")

names(day)

dayGrouped <- day %>%
  group_by(season) %>%
  summarize(
    Casualusers = sum(casual),
    Registeredusers = sum(registered),
    AvgTemp = round(mean(temp) *100, 2),
    Users = sum(casual) + sum(registered)
  )
dayGrouped_long <- dayGrouped %>%
  pivot_longer(cols = c(Casualusers, Registeredusers), names_to = "UserType", values_to = "Count")

ggplot(dayGrouped_long, aes(x = season, y = Count, fill = UserType)) +
  geom_col(position = "stack") +
  theme_minimal() +
  coord_cartesian(ylim = c(300000, 1150000)) +
  scale_fill_manual(values = c("Casualusers" = "lightblue", "Registeredusers" = "lightgray")) +
  labs(fill = "User Type")

ggplot(dayGrouped) +
  geom_col(aes(season, Users, fill=as.factor(AvgTemp))) +
  coord_cartesian(ylim = c(300000, 1150000)) +
  theme_minimal() +
  scale_fill_brewer(palette="OrRd")

dayGroupedTwice <- day %>%
  group_by(yr, season) %>%
  summarize(
    Casualusers = sum(casual),
    Registeredusers = sum(registered),
    AvgTemp = round(mean(temp) * 100, 2),
    Users = sum(casual) + sum(registered)
  )

ggplot(dayGroupedTwice) +
  geom_col(aes(season, Users, fill=as.factor(AvgTemp))) +
  coord_cartesian(ylim = c(100000, 650000)) +
  theme_minimal() +
  scale_fill_brewer(palette="OrRd") +
  facet_wrap(~yr)

ggplot(day) +
  geom_point(aes(cnt,temp,color=as.factor(weekday))) +
  theme_minimal() +
  facet_wrap(~workingday) +
  scale_color_brewer(palette = "Dark2")
  
