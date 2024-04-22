library(readr)
library(tidyverse)
hour <- read_csv("hour.csv")
day <- read_csv("day.csv")


##Day Analysis For Group
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
  

## Question 1

#set date variable to correct format
hour$dteday <- as.Date(hour$dteday)

# Plotting the relationship between time of rental and registered users
ggplot(hour, aes(x = dteday, y = registered, alpha=0.1)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "Relationship Between Time of Rental and Registered Users",
       x = "Date",
       y = "Number of Registered Users") +
  theme_minimal()

# Same graph for casual users
ggplot(hour, aes(x = dteday, y = casual, alpha=0.1)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "Relationship Between Time of Rental and Casual Users",
       x = "Date",
       y = "Number of Registered Users") +
  theme_minimal()

# Plotting the number of rentals over time
ggplot(hour, aes(x = dteday, y = cnt)) +
  geom_col() +
  labs(title = "Number of Rentals Over Time",
       x = "Date",
       y = "Number of Rentals") +
  theme_minimal()

# Plotting the number of rentals by season
ggplot(hour, aes(x = factor(season), y = cnt, fill = factor(season))) +
  geom_boxplot() +
  labs(title = "Number of Rentals by Season",
       x = "Season",
       y = "Number of Rentals") +
  theme_minimal()


## I didn't like any of those graphs for the question so I made these following ones

# Plotting the distribution of rentals across seasons by user registration status
ggplot(hour, aes(x = factor(season), y = registered, fill = "Registered Users")) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_bar(aes(y = casual, fill = "Casual Users"), stat = "identity", position = "dodge") +
  labs(title = "Distribution of Rentals Across Seasons by User Registration Status",
       x = "Season",
       y = "Number of Rentals",
       fill = "User Type") +
  theme_minimal()

# Plotting the trend of rentals over time for registered and casual users
ggplot(hour, aes(x = dteday, y = registered, color = "Registered Users")) +
  geom_line() +
  geom_line(aes(y = casual, color = "Casual Users")) +
  labs(title = "Trend of Rentals Over Time for Registered and Casual Users",
       x = "Date",
       y = "Number of Rentals",
       color = "User Type") +
  theme_minimal()



# Question 2

# Set bin size 
hour$hour_bin <- factor(hour$hour_bin, levels = c("(0,240]", "(240,480]", "(480,720]", "(720,960]", "(960,1200]", "(1200,1440]"))

# Convert dteday to Date format
hour$dteday <- as.Date(hour$dteday)

# Extract month from dteday
hour$month <- format(hour$dteday, "%m")

# Sum the count of rented bikes by hour and month
hour_sum <- aggregate(cnt ~ hr + month, data = hour, sum)

# Plot
ggplot(hour_sum, aes(x = hr, y = cnt, color = month, group = month)) +
  geom_line() +
  labs(x = "Hour of Day", y = "Total Count of Rented Bikes", color = "Month") +
  theme_minimal() +
  ggtitle("Variation of Bike Rentals by Time of Day Across Different Months")
