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


data <- mutate(day, yr = ifelse(yr == 0, 2011, 2012),
               mnth = case_when(
                 mnth == 1 ~ "January",
                 mnth == 2 ~ "February",
                 mnth == 3 ~ "March",
                 mnth == 4 ~ "April",
                 mnth == 5 ~ "May",
                 mnth == 6 ~ "June",
                 mnth == 7 ~ "July",
                 mnth == 8 ~ "August",
                 mnth == 9 ~ "September",
                 mnth == 10 ~ "October",
                 mnth == 11 ~ "November",
                 mnth == 12 ~ "December"
               ))

# Convert weekday column to actual day names
data <- mutate(data, weekday = case_when(
  weekday == 0 ~ "Sunday",
  weekday == 1 ~ "Monday",
  weekday == 2 ~ "Tuesday",
  weekday == 3 ~ "Wednesday",
  weekday == 4 ~ "Thursday",
  weekday == 5 ~ "Friday",
  weekday == 6 ~ "Saturday"
))

# Convert 'dteday' column to date format
data$dteday <- as.Date(data$dteday)

# Define important event dates
important_dates <- c(
  "2011-04-16", "2011-04-17", "2011-07-04", "2011-09-24",
  "2012-04-14", "2012-04-15", "2012-07-04", "2012-09-01",
  "2012-11-23", "2012-12-25"
)

# Create binary variable indicating whether it's an important date
data$is_important <- ifelse(data$dteday %in% as.Date(important_dates), 1, 0)

# Define peak blossom dates
peak_blossom_dates <- c("2011-03-29", "2012-03-20")

# Create binary variable indicating whether it's a peak blossom day
data$is_peak_blossom <- ifelse(data$dteday %in% as.Date(peak_blossom_dates), 1, 0)

# Create binary variable indicating whether it's both an important date and a peak blossom day
data$is_important_and_peak <- ifelse(data$is_important == 1 & data$is_peak_blossom == 1, 1, 0)

# Print the updated dataset
head(data)

sorted_data <- data[order(data$casual), ]

# Select the top 5 and bottom 5 rows
top_5_least <- sorted_data[1:5, ]
bottom_5_most <- sorted_data[(nrow(sorted_data) - 4):nrow(sorted_data), ]

# Create a combined dataset for display
combined_data <- rbind(top_5_least, bottom_5_most)

# Convert 'dteday' column to Date format
combined_data$dteday <- as.Date(combined_data$dteday)

# Create a copy of the dataset with formatted date
formatted_combined_data <- combined_data
formatted_combined_data$dteday <- format(formatted_combined_data$dteday, "%Y-%m-%d")
formatted_combined_data <- formatted_combined_data %>%
  select(dteday,
         season,
         mnth,
         weekday,
         holiday,
         workingday,
         temp,
         hum,
         windspeed,
         casual,
         registered,
         is_important,
         is_peak_blossom,
         is_important_and_peak) %>%
  mutate(temp = round(temp * 100),
         hum = round(hum * 100),
         windspeed = round(windspeed * 100))

binary_to_yes_no <- function(x) {
  ifelse(x == 0, "No", "Yes")
}

# Modify the binary variables to Yes or No
formatted_combined_data <- formatted_combined_data %>%
  mutate_at(vars(is_important, is_peak_blossom, is_important_and_peak, holiday, workingday), binary_to_yes_no) %>%
  rename(
    Date = dteday,
    Season = season,
    Month = mnth,
    DayOfWeek = weekday,
    Holiday = holiday,
    WorkingDay = workingday,
    Temperature = temp,
    Humidity = hum,
    WindSpeed = windspeed,
    CasualUsers = casual,
    RegisteredUsers = registered,
    ImportantDate = is_important,
    PeakBlossomDay = is_peak_blossom,
    ImportantAndPeakBlossomDay = is_important_and_peak
  )


# Display the combined dataset with kable
library(kableExtra)

# Display the combined dataset with kable and color formatting
kable(formatted_combined_data, caption = "Top 5 Days with Least and Most Casual Users", align = "c") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  column_spec(7, color = "white",
              background = spec_color(formatted_combined_data$Temperature[1:10], end = 0.7)) 

